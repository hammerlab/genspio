open! Base
module Filename = Caml.Filename

let ( // ) = Filename.concat

module Shell_script = struct
  type t = {name: string; content: unit Genspio.EDSL.t; dependencies: t list}

  open Genspio.EDSL

  let make ?(dependencies = []) name content = {name; content; dependencies}

  let sanitize_name n =
    let m =
      String.map n ~f:(function
        | ('0' .. '9' | 'a' .. 'z' | 'A' .. 'Z' | '-') as c -> c
        | _ -> '_' ) in
    try String.sub ~pos:0 ~len:40 m with _ -> m

  let path {name; content; _} =
    let open Caml in
    let hash = Marshal.to_string content [] |> Digest.string |> Digest.to_hex in
    let tag = String.sub hash 0 8 in
    "_scripts" // Fmt.str "%s_%s.sh" (sanitize_name name) tag

  let call f = exec ["sh"; path f]

  type compiled = {files: (string * string list) list; call: unit Genspio.EDSL.t}

  let rec compile ({name; content; dependencies} as s) =
    let filename = path s in
    let dep_scripts = List.map ~f:compile dependencies in
    (* dbg "name %s filename: %s" name filename; *)
    { files=
        ( filename
        , [ "# Script %s"; "# Generated by Genspio"
          ; Fmt.str "echo 'Genspio.Shell_script: %s (%s)'" name filename
          ; Genspio.Compile.to_many_lines content ] )
        :: List.concat_map dep_scripts ~f:(fun c -> c.files)
    ; call= call s }
end

module Run_environment = struct
  module File = struct
    type t = Http of string * [`Xz] option

    let local_file_name =
      let noquery url = String.split ~on:'?' url |> List.hd_exn in
      function
      | Http (url, None) -> "_cache" // Filename.basename (noquery url)
      | Http (url, Some `Xz) ->
          "_cache"
          // Filename.(basename (noquery url) |> fun f -> chop_suffix f ".xz")

    let tmp_name_of_url = function
      | Http (url, ext) ->
          ("_cache" // Caml.Digest.(string url |> to_hex))
          ^ Option.value_map ~default:"" ext ~f:(fun `Xz -> ".xz")

    let make_files files =
      List.map files ~f:(function Http (url, act) as t ->
          let base = local_file_name t in
          let wget =
            let open Genspio.EDSL in
            check_sequence
              [ ("mkdir", exec ["mkdir"; "-p"; "_cache"])
              ; ( "wget"
                , exec ["wget"; url; "--output-document"; tmp_name_of_url t] )
              ; ( "act-and-mv"
                , match act with
                  | None -> exec ["mv"; "-f"; tmp_name_of_url t; base]
                  | Some `Xz ->
                      seq
                        [ exec ["unxz"; "-k"; tmp_name_of_url t]
                        ; exec
                            [ "mv"; "-f"
                            ; Filename.chop_suffix (tmp_name_of_url t) ".xz"
                            ; base ] ] ) ] in
          (base, [], wget) )
  end

  module Ssh = struct
    let ssh_options =
      [ "-oStrictHostKeyChecking=no"; "-oGlobalKnownHostsFile=/dev/null"
      ; "-oUserKnownHostsFile=/dev/null" ]

    let host_file f = Fmt.str "root@@localhost:%s" f

    let sshpass ?password cmd =
      match password with None -> cmd | Some p -> ["sshpass"; "-p"; p] @ cmd

    let scp ?password ~ssh_port () =
      sshpass ?password @@ ["scp"] @ ssh_options @ ["-P"; Int.to_string ssh_port]

    let script_over_ssh ?root_password ~ssh_port ~name script =
      let open Shell_script in
      let open Genspio.EDSL in
      let script_path = path script in
      let tmp = "/tmp" // Filename.basename script_path in
      make ~dependencies:[script] (Fmt.str "SSH exec %s" name)
      @@ check_sequence
           [ ( "scp"
             , exec
                 ( scp ?password:root_password ~ssh_port ()
                 @ [script_path; host_file tmp] ) )
           ; ( "ssh-exec"
             , exec
                 ( sshpass ?password:root_password
                 @@ ["ssh"] @ ssh_options
                 @ [ "-p"; Int.to_string ssh_port; "root@localhost"
                   ; Fmt.str "sh %s" tmp ] ) ) ]
  end

  type vm =
    | Qemu_arm of
        { kernel: File.t
        ; sd_card: File.t
        ; machine: string
        ; initrd: File.t option
        ; root_device: string }
    | Qemu_amd46 of {hda: File.t; ui: [`No_graphic | `Curses]}

  module Setup = struct
    type t =
      | Ssh_to_vm of unit Genspio.EDSL.t
      | Copy_relative of string * string

    let ssh_to_vm u = [Ssh_to_vm u]
    let copy (`Relative src) (`Relative dst) = Copy_relative (src, dst)
  end

  type t =
    { name: string
    ; root_password: string option
    ; setup: Setup.t list
    ; ssh_port: int
    ; local_dependencies: [`Command of string] list
    ; vm: vm }

  let make vm ?root_password ?(setup = []) ~local_dependencies ~ssh_port name =
    {vm; root_password; setup; local_dependencies; name; ssh_port}

  let qemu_arm ~kernel ~sd_card ~machine ?initrd ~root_device =
    make (Qemu_arm {kernel; sd_card; machine; initrd; root_device})

  let qemu_amd46 ?(ui = `No_graphic) ~hda = make (Qemu_amd46 {hda; ui})
  let http ?act uri = File.Http (uri, act)

  let start_qemu_vm : t -> Shell_script.t = function
    | { ssh_port
      ; vm= Qemu_arm {kernel; machine; sd_card; root_device; initrd; _}
      ; _ } ->
        let open Shell_script in
        let open Genspio.EDSL in
        make "Start-qemu-arm"
          (exec
             ( [ "qemu-system-arm"; "-M"; machine; "-m"; "1024M"; "-kernel"
               ; File.local_file_name kernel ]
             @ Option.value_map initrd ~default:[] ~f:(fun f ->
                   ["-initrd"; File.local_file_name f] )
             @ [ "-pidfile"; "qemu.pid"; "-net"; "nic"; "-net"
               ; Fmt.str "user,hostfwd=tcp::%d-:22" ssh_port; "-nographic"
               ; "-sd"; File.local_file_name sd_card; "-append"
               ; Fmt.str "console=ttyAMA0 verbose debug root=%s" root_device ]
             ) )
    | {ssh_port; vm= Qemu_amd46 {hda; ui}; _} ->
        (* See https://wiki.qemu.org/Hosts/BSD
           qemu-system-x86_64 -m 2048 \
            -hda FreeBSD-11.0-RELEASE-amd64.qcow2 -enable-kvm \
            -netdev user,id=mynet0,hostfwd=tcp:127.0.0.1:7722-:22 \
            -device e1000,netdev=mynet0 *)
        let open Shell_script in
        let open Genspio.EDSL in
        make "Start-qemu"
          (exec
             ( [ "qemu-system-x86_64"
                 (* ; "-M"
                    * ; machine *); "-m"
               ; "1024M" (* ; "-enable-kvm" → requires `sudo`?*); "-hda"
               ; File.local_file_name hda ]
             @ [ "-pidfile"; "qemu.pid"; "-netdev"
               ; Fmt.str "user,id=mynet0,hostfwd=tcp::%d-:22" ssh_port
               ; ( match ui with
                 | `Curses -> "-curses"
                 | `No_graphic -> "-nographic" ); "-device"
               ; "e1000,netdev=mynet0" ] ) )

  let kill_qemu_vm : t -> Shell_script.t = function
    | {name; _} ->
        let open Genspio.EDSL in
        let pid = get_stdout (exec ["cat"; "qemu.pid"]) in
        Shell_script.(make (Fmt.str "kill-qemu-%s" name))
        @@ check_sequence
             (* ~name:(Fmt.str "Killing Qemu VM")
                * ~clean_up:[fail "kill_qemu_vm"] *)
             [ ( "Kill-qemu-vm"
               , if_seq
                   (file_exists (string "qemu.pid"))
                   ~t:
                     [ if_seq
                         (call [string "kill"; pid] |> succeeds)
                         ~t:[exec ["rm"; "qemu.pid"]]
                         ~e:
                           [ printf
                               (string
                                  "PID file here (PID: %s) but Kill failed, \
                                   deleting `qemu.pid`" )
                               [pid]; exec ["rm"; "qemu.pid"]; exec ["false"] ]
                     ]
                   ~e:[printf (string "No PID file") []; exec ["false"]] ) ]

  let configure : t -> Shell_script.t = function
    | {name; local_dependencies; _} ->
        let open Genspio.EDSL in
        let report = tmp_file "configure-report.md" in
        let there_was_a_failure = tmp_file "bool-failure" in
        let cmds =
          [ report#set (str "Configuration Report\n====================\n\n")
          ; there_was_a_failure#set (bool false |> Bool.to_string) ]
          @ List.map local_dependencies ~f:(function `Command name ->
                if_seq
                  (exec ["which"; name] |> silently |> succeeds)
                  ~t:[report#append (Fmt.kstr str "* `%s`: found.\n" name)]
                  ~e:
                    [ report#append (Fmt.kstr str "* `%s`: NOT FOUND!\n" name)
                    ; there_was_a_failure#set (bool true |> Bool.to_string) ] )
          @ [ call [string "cat"; report#path]
            ; if_seq
                (there_was_a_failure#get |> Bool.of_string)
                ~t:
                  [ exec ["printf"; "\\nThere were *failures* :(\\n"]
                  ; exec ["false"] ]
                ~e:[exec ["printf"; "\\n*Success!*\\n"]] ] in
        Shell_script.(make (Fmt.str "configure-%s" name))
        @@ check_sequence ~verbosity:`Output_all
             (List.mapi cmds ~f:(fun i c -> (Fmt.str "config-%s-%d" name i, c)))

  let make_dependencies = function
    | {vm= Qemu_amd46 {hda; _}; _} -> File.make_files [hda]
    | {vm= Qemu_arm {kernel; sd_card; initrd; _}; _} ->
        File.make_files
          ( [kernel; sd_card]
          @ Option.value_map initrd ~default:[] ~f:(fun x -> [x]) )

  let setup_dir_content tvm =
    let {root_password; setup; ssh_port; _} = tvm in
    let other_files = ref [] in
    let dependencies = make_dependencies tvm in
    let start_deps = List.map dependencies ~f:(fun (base, _, _) -> base) in
    let help_entries = ref [] in
    let make_entry ?doc ?(phony = false) ?(deps = []) target action =
      help_entries := (target, doc) :: !help_entries ;
      (if phony then [Fmt.str ".PHONY: %s" target] else [])
      @ [ Fmt.str "# %s: %s" target
            (Option.value_map
               ~f:(String.map ~f:(function '\n' -> ' ' | c -> c))
               doc ~default:"NOT DOCUMENTED" )
        ; Fmt.str "%s: %s" target (String.concat ~sep:" " deps)
        ; Fmt.str "\t@@%s" (Genspio.Compile.to_one_liner ~no_trap:true action)
        ] in
    let make_script_entry ?doc ?phony ?deps target script =
      let open Shell_script in
      let {files; call} = Shell_script.compile script in
      other_files := !other_files @ files ;
      make_entry ?doc ?phony ?deps target call in
    let setup_entries =
      List.mapi setup ~f:(fun idx ->
          let name = Fmt.str "setup-%d" idx in
          let deps = List.init idx ~f:(fun i -> Fmt.str "setup-%d" i) in
          function
          | Ssh_to_vm cmds ->
              ( name
              , make_script_entry ~phony:true name ~deps
                  (Ssh.script_over_ssh ?root_password ~ssh_port ~name
                     (Shell_script.make (Fmt.str "setup-%s" name) cmds) ) )
          | Copy_relative (src, dst) ->
              ( name
              , make_entry ~phony:true name ~deps
                  Genspio.EDSL.(
                    exec ["tar"; "c"; src]
                    ||> exec
                          ( Ssh.sshpass ?password:root_password
                          @@ ["ssh"; "-p"; Int.to_string ssh_port]
                          @ Ssh.ssh_options
                          @ [ "root@localhost"
                            ; Fmt.str "tar -x -f - ; mv %s %s" src dst ] )) ) )
    in
    let makefile =
      ["# Makefile genrated by Genspio's VM-Tester"]
      @ List.concat_map dependencies ~f:(fun (base, deps, cmd) ->
            Shell_script.(make (Fmt.str "get-%s" (sanitize_name base)) cmd)
            |> make_script_entry ~deps base )
      @ make_script_entry ~phony:true "configure" (configure tvm)
          ~doc:"Configure this local-host (i.e. check for requirements)."
      @ make_script_entry ~deps:start_deps ~phony:true "start"
          ~doc:"Start the Qemu VM (this grabs the terminal)."
          (start_qemu_vm tvm)
      @ make_script_entry ~phony:true "kill" (kill_qemu_vm tvm)
          ~doc:"Kill the Qemu VM."
      @ List.concat_map setup_entries ~f:snd
      @ make_entry ~phony:true "setup"
          ~deps:(List.map setup_entries ~f:fst)
          Genspio.EDSL.(seq [exec ["echo"; "Setup done"]])
          ~doc:
            "Run the “setup” recipe on the Qemu VM (requires the VM\n\
            \  started in another terminal)."
      @ make_entry ~phony:true "ssh" ~doc:"Display an SSH command"
          Genspio.EDSL.(
            let prefix =
              Ssh.sshpass ?password:root_password [] |> String.concat ~sep:" "
            in
            printf
              (Fmt.kstr string "%s ssh -p %d %s root@@localhost" prefix ssh_port
                 (String.concat ~sep:" " Ssh.ssh_options) )
              []) in
    let help =
      make_script_entry ~phony:true "help"
        Shell_script.(
          make "Display help message"
            Genspio.EDSL.(
              exec
                [ "printf"
                ; "\\nHelp\\n====\\n\\nThis a generated Makefile (by \
                   Genspio-VM-Tester):\\n\\n%s\\n\\n%s\\n"
                ; List.map
                    (("help", Some "Display this help message") :: !help_entries)
                    ~f:(function
                    | _, None -> ""
                    | target, Some doc -> Fmt.str "* `make %s`: %s\n" target doc )
                  |> String.concat ~sep:""
                ; Fmt.str
                    "SSH: the command `make ssh` *outputs* an SSH command \
                     (%s). Examples:\n\n\
                     $ `make ssh` uname -a\n\
                     $ tar c some/dir/ | $(make ssh) 'tar x'\n\n\
                     (may need to be `tar -x -f -` for BSD tar).\n"
                    (Option.value_map ~default:"No root-password" root_password
                       ~f:(Fmt.str "Root-password: %S") ) ])) in
    ("Makefile", ("all: help" :: makefile) @ help @ [""]) :: !other_files

  module Example = struct
    let qemu_arm_openwrt ~ssh_port more_setup =
      let setup =
        let open Genspio.EDSL in
        Setup.ssh_to_vm
          (check_sequence
             [ ("opkg-update", exec ["opkg"; "update"])
             ; ("install-od", exec ["opkg"; "install"; "coreutils-od"])
             ; ("install-make", exec ["opkg"; "install"; "make"]) ] )
        @ more_setup in
      let base_url =
        "https://downloads.openwrt.org/snapshots/trunk/realview/generic/" in
      qemu_arm "qemu_arm_openwrt" ~ssh_port ~machine:"realview-pbx-a9"
        ~kernel:(http (base_url // "openwrt-realview-vmlinux.elf"))
        ~sd_card:(http (base_url // "openwrt-realview-sdcard.img"))
        ~root_device:"/dev/mmcblk0p1" ~setup
        ~local_dependencies:[`Command "qemu-system-arm"]

    let qemu_arm_wheezy ~ssh_port more_setup =
      (*
         See {{:https://people.debian.org/~aurel32/qemu/armhf/}}.
      *)
      let aurel32 file =
        http ("https://people.debian.org/~aurel32/qemu/armhf" // file) in
      let setup =
        let open Genspio.EDSL in
        Setup.ssh_to_vm
          (check_sequence
             [("apt-get-make", exec ["apt-get"; "install"; "--yes"; "make"])] )
        @ more_setup in
      qemu_arm "qemu_arm_wheezy" ~ssh_port ~machine:"vexpress-a9"
        ~kernel:(aurel32 "vmlinuz-3.2.0-4-vexpress")
        ~sd_card:(aurel32 "debian_wheezy_armhf_standard.qcow2")
        ~initrd:(aurel32 "initrd.img-3.2.0-4-vexpress")
        ~root_device:"/dev/mmcblk0p2" ~root_password:"root" ~setup
        ~local_dependencies:[`Command "qemu-system-arm"; `Command "sshpass"]

    let qemu_amd64_freebsd ~ssh_port more_setup =
      let qcow =
        http ~act:`Xz
          (* This qcow2 was created following the instructions at
             https://wiki.qemu.org/Hosts/BSD#FreeBSD *)
          "https://www.dropbox.com/s/ni7u0k6auqh2lya/FreeBSD11-amd64-rootssh.qcow2.xz?raw=1"
      in
      let setup = more_setup in
      let root_password = "root" in
      qemu_amd46 "qemu_amd64_freebsd" ~hda:qcow ~setup ~root_password
        ~ui:`Curses
        ~local_dependencies:[`Command "qemu-system-x86_64"; `Command "sshpass"]
        ~ssh_port

    let qemu_amd64_darwin ~ssh_port more_setup =
      (*
         Made with these instructions: http://althenia.net/notes/darwin
         from http://www.opensource.apple.com/static/iso/darwinx86-801.iso.gz
      *)
      let qcow =
        http ~act:`Xz
          "https://www.dropbox.com/s/2oeuya0isvorsam/darwin-disk-20180730.qcow2.xz?raw=1"
      in
      let setup = more_setup in
      let root_password = "root" in
      qemu_amd46 "qemu_amd64_darwin" ~hda:qcow ~setup ~root_password
        ~ui:`No_graphic
        ~local_dependencies:[`Command "qemu-system-x86_64"; `Command "sshpass"]
        ~ssh_port
  end
end

let cmdf fmt =
  Fmt.kstr
    (fun cmd ->
      match Caml.Sys.command cmd with
      | 0 -> ()
      | other -> Fmt.kstr failwith "Command %S did not return 0: %d" cmd other
      )
    fmt

let write_lines p l =
  let open Caml in
  let o = open_out p in
  Base.List.iter l ~f:(Printf.fprintf o "%s\n") ;
  close_out o

let () =
  let fail fmt =
    Fmt.kstr
      (fun s ->
        Fmt.epr "Wrong CLI: %s\n%!" s ;
        Caml.exit 2 )
      fmt in
  let example = ref None in
  let path = ref None in
  let ssh_port = ref 20202 in
  let copy_directories = ref [] in
  let examples =
    [ ( "arm-owrt"
      , Run_environment.Example.qemu_arm_openwrt
      , "Qemu ARM VM with OpenWRT." )
    ; ( "arm-dw"
      , Run_environment.Example.qemu_arm_wheezy
      , "Qemu ARM with Debian Wheezy." )
    ; ( "amd64-fb"
      , Run_environment.Example.qemu_amd64_freebsd
      , "Qemu x86_64 with FreeBSD." )
    ; ( "amd64-dw"
      , Run_environment.Example.qemu_amd64_darwin
      , "Qemu x86_64 with Darwin 8 (old Mac OSX)." ) ] in
  let set_example arg =
    match !example with
    | Some _ -> fail "Too many arguments (%S)!" arg
    | None ->
        example :=
          Some
            ( match
                List.find_map examples ~f:(fun (e, v, _) ->
                    if String.(e = arg) then Some v else None )
              with
            | Some s -> s
            | None -> fail "Don't know VM %S" arg ) in
  let module Arg = Caml.Arg in
  let args =
    Arg.align
      [ ( "--ssh-port"
        , Arg.Int (fun s -> ssh_port := s)
        , Fmt.str "<int> Set the SSH-port (default: %d)." !ssh_port )
      ; ( "--vm"
        , Arg.String set_example
        , Fmt.str "<name> The Name of the VM, one of:\n%s"
            (String.concat ~sep:"\n"
               (List.map
                  ~f:(fun (n, _, d) ->
                    Fmt.str "%s* `%s`: %s" (String.make 25 ' ') n d )
                  examples ) ) )
      ; ( "--copy"
        , Arg.String
            (fun s ->
              let add p lp =
                let local_rel =
                  String.map p ~f:(function '/' -> '_' | c -> c) in
                copy_directories := (p, local_rel, lp) :: !copy_directories
              in
              match Base.String.split ~on:':' s with
              | [] | [_] -> fail "Error in --copy: need a `:` separator (%S)" s
              | [p; lp] -> add p lp
              | p :: more -> add p (String.concat ~sep:":" more) )
        , "<p-src:p-dst> Copy <p-src> in the output directory and add its \
           upload to the VM to the `make setup` target as a relative path \
           <p-dst>." ) ] in
  let usage = Fmt.str "vm-tester --vm <vm-name> <path>" in
  let anon arg =
    match !path with
    | Some _ -> fail "Too many arguments (%S)!" arg
    | None -> path := Some arg in
  Arg.parse args anon usage ;
  let more_setup =
    List.map !copy_directories ~f:(fun (_, locrel, hostp) ->
        Run_environment.Setup.copy (`Relative locrel) (`Relative hostp) ) in
  let re =
    match !example with
    | Some e -> e ~ssh_port:!ssh_port more_setup
    | None -> fail "Missing VM name\nUsage: %s" usage in
  let content = Run_environment.setup_dir_content re in
  let path =
    match !path with
    | Some p -> p
    | None -> fail "Missing path!\nUsage: %s" usage in
  List.iter content ~f:(fun (filepath, content) ->
      let full = path // filepath in
      cmdf "mkdir -p %s" (Filename.dirname full) ;
      write_lines full content ) ;
  List.iter !copy_directories ~f:(fun (p, local_rel, _) ->
      cmdf "rsync -az %s %s/%s" p path local_rel )
