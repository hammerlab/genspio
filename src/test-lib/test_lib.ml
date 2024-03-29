open! Base

let ( // ) = Caml.Filename.concat

module Filename = Caml.Filename
module Sys = Caml.Sys

module Test = struct
  type t =
    | Exits of
        { no_trap: bool
        ; name: string
        ; args: string list
        ; returns: int
        ; script: unit Genspio.Language.t }

  let exits ?(no_trap = false) ?name ?(args = []) returns script =
    let name = Option.value name ~default:(Fmt.str "no-name-%d" returns) in
    [Exits {no_trap; name; args; returns; script}]
end

open Test

module Shell = struct
  type t =
    { name: string
    ; command: string -> string list -> string list
    ; get_version: string }

  let make_shell name ~command ~get_version = {name; command; get_version}
  let to_string t = t.name

  let known_shells () =
    (* let exec l = *)
    (*   List.map ~f:Filename.quote l |> String.concat ~sep:" " in *)
    let dash_like bin ~get_version =
      make_shell (Filename.basename bin)
        ~command:(fun s args -> [bin; "-x"; s] @ args)
        ~get_version in
    let busybox =
      make_shell "busybox"
        ~command:(fun s args -> ["busybox"; "ash"; "-x"; s] @ args)
        ~get_version:"busybox | head -n 1" in
    let package_version package =
      (* for when there is no `--version`, `-V`, etc. we go the “debian” way *)
      Fmt.str "dpkg -s %s | grep ^Version" package in
    [ dash_like "dash" ~get_version:(package_version "dash")
    ; dash_like "bash" ~get_version:"bash --version | head -n 1"
    ; dash_like "sh" ~get_version:(package_version "sh"); busybox
    ; dash_like "ksh" ~get_version:"ksh --version 2>&1"
    ; dash_like "mksh" ~get_version:(package_version "mksh")
    ; dash_like "posh" ~get_version:(package_version "posh")
    ; dash_like "zsh" ~get_version:"zsh --version" ]
end

module Shell_directory = struct
  type t =
    { shell: Shell.t
    ; compilation: [`Std_one_liner | `Std_multi_line | `Slow_stack]
    ; optimization_passes: [`Cst_prop] list
    ; verbose: bool }

  let name t =
    let opti =
      List.map t.optimization_passes ~f:(function `Cst_prop -> "-cp")
      |> String.concat ~sep:"" in
    Fmt.str "%s-%s%s" (Shell.to_string t.shell)
      ( match t.compilation with
      | `Std_multi_line -> "StdML"
      | `Std_one_liner -> "Std1L"
      | `Slow_stack -> "SlowFlow" )
      opti

  let unique_name = function
    | Exits {no_trap; name; args; returns; script} ->
        Fmt.str "test-%s-%s-A%d-R%d-%s"
          (if no_trap then "noT" else "T")
          (let long =
             String.map name ~f:(function
               | ('a' .. 'z' | 'A' .. 'Z' | '0' .. '9') as c -> c
               | _ -> '_' ) in
           if String.length long > 30 then String.sub long ~pos:0 ~len:30
           else long )
          (List.length args) returns
          Caml.(
            Marshal.to_string script [Marshal.Closures]
            |> Digest.string |> Digest.to_hex
            |> fun s -> String.sub s 0 10)

  (*
let optimize : type a. _ -> a Genspio.Language.t -> _ =
   fun t script ->
    List.fold t.optimization_passes ~init:script ~f:(fun prev -> function
      | `Cst_prop -> Genspio.Transform.Constant_propagation.process prev )
 *)

  let script_path test = "script" // Fmt.str "%s-script.sh" (unique_name test)

  let run_test_path test =
    "script" // Fmt.str "%s-run-test.sh" (unique_name test)

  let script_display test =
    "script" // Fmt.str "%s-display.scm" (unique_name test)

  let script_opti_display test =
    "script" // Fmt.str "%s-opti-display.scm" (unique_name test)

  let success_path test = Fmt.str "_success/%s.txt" @@ unique_name test
  let failure_path test = Fmt.str "_failure/%s.txt" @@ unique_name test
  let stdout_path test = Fmt.str "_log/%s/stdout.txt" @@ unique_name test
  let stderr_path test = Fmt.str "_log/%s/stderr.txt" @@ unique_name test

  let display_script _t = function
    | Exits {script; _} -> Genspio.Compile.to_string_hum script

  let display_opti_script t = function
    | Exits {script; _} ->
        List.fold t.optimization_passes ~init:script ~f:(fun prev -> function
          | `Cst_prop -> Genspio.Transform.Constant_propagation.process prev )
        |> Genspio.Compile.to_string_hum

  let run_test_script t =
    let test_name = name t in
    function
    | Exits {name; args; returns; _} as test ->
        let fill_result_file which =
          let echos =
            [ Fmt.str "- Returns $RRR (expected: %d)." returns
            ; Fmt.str "- Script: \\`%s\\`" (script_path test)
            ; Fmt.str "- Pretty-printed: \\`%s\\`" (script_display test)
            ; Fmt.str "- Pretty-printed after optimizations: \\`%s\\`"
                (script_opti_display test)
            ; Fmt.str "- Test-runner: \\`%s\\`" (run_test_path test)
            ; Fmt.str "- STDOUT: \\`%s\\`" (stdout_path test)
            ; Fmt.str "- STDERR: \\`%s\\`" (stderr_path test) ] in
          let file, first_line =
            match which with
            | `OK ->
                ( success_path test
                , Fmt.str "- **OK**: \\`%s\\`" (unique_name test) )
            | `KO ->
                ( failure_path test
                , Fmt.str "- **KO**: \\`%s\\`" (unique_name test) ) in
          let lines =
            Fmt.str "printf -- \"%s\\n\" > %s" first_line file
            :: List.map echos ~f:(fun l ->
                   Fmt.str "printf -- \"    %s\\n\" >> %s" l file ) in
          String.concat ~sep:"\n" lines in
        Fmt.str
          "mkdir -p _success _failure %s\n\
           export TMPDIR=$PWD/_tmp/%s\n\
           mkdir -p ${TMPDIR}\n\
           %s > %s 2> %s\n\
           export RRR=$?\n\
           if [ $RRR -eq %d ] ; then\n\
           %s\n\
           else\n\
           %s\n\
          \               %s\n\
           fi\n"
          (stdout_path test |> Filename.dirname)
          (unique_name test)
          ( t.shell.Shell.command (script_path test) args
          |> List.map ~f:Filename.quote |> String.concat ~sep:" " )
          (stdout_path test) (stderr_path test) returns
          (fill_result_file `OK)
          (fill_result_file `KO)
          ( if t.verbose then
            Fmt.str "printf 'Test %s with [%s] FAILED\\n' >&2" name test_name
          else "" )

  let script_content t = function
    | Exits {no_trap; script; _} -> (
        let script =
          List.fold t.optimization_passes ~init:script ~f:(fun prev -> function
            | `Cst_prop -> Genspio.Transform.Constant_propagation.process prev )
        in
        match t.compilation with
        | `Std_one_liner -> Genspio.Compile.to_one_liner ~no_trap script
        | `Std_multi_line -> Genspio.Compile.to_many_lines ~no_trap script
        | `Slow_stack ->
            Genspio.Compile.To_slow_flow.compile script
              ~trap:(if no_trap then `None else `Exit_with 77)
            |> Fmt.str "%a\n" Genspio.Compile.To_slow_flow.Script.pp_posix )

  let make_report_path _ = "script" // "make_report.sh"

  let make_report_content t testlist =
    (let open Genspio.EDSL_v0 in
    let count_files dir =
      if_then_else
        (exec ["test"; "-d"; dir] |> succeeds)
        (exec ["ls"; "-1"; dir] ||> exec ["wc"; "-l"])
        (exec ["echo"; "No-dir"])
      ||> exec ["tr"; "-d"; "\\n"]
      |> get_stdout |> Byte_array.to_c in
    seq
      [ exec
          [ "printf"
          ; Fmt.str "* Shell: %s, compilation; %s, opti: %s, total tests: %d\\n"
              (Shell.to_string t.shell)
              ( match t.compilation with
              | `Std_one_liner -> "Standard-one-liner"
              | `Std_multi_line -> "Standard-multi-line"
              | `Slow_stack -> "Slow-stack" )
              ( List.map t.optimization_passes ~f:(function `Cst_prop ->
                    "cst-prop" )
              |> String.concat ~sep:"→" )
              (List.length testlist) ]
      ; call
          [ string "printf"; string "    * Failures: %s.\\n"
          ; count_files "_failure/" ]
      ; call
          [ string "printf"; string "    * Successes: %s.\\n"
          ; count_files "_success" ] ])
    |> Genspio.Compile.to_many_lines

  let makefile t testlist =
    Fmt.str ".PHONY: all clean report check\nall: %s\n\n"
      (List.map testlist ~f:success_path |> String.concat ~sep:" ")
    :: Fmt.str "clean:\n\trm -fr _success _failure _log _tmp *.md\n\n"
    :: Fmt.str
         "failures.md:\n\t@@{ cat _failure/* ; echo '' ; } > failures.md\n\n"
    :: Fmt.str
         "successes.md:\n\t@@{ cat _success/* ; echo '' ; } > successes.md\n\n"
    :: Fmt.str "report: failures.md successes.md\n\t@@sh %s > report.md\n\n"
         (make_report_path t)
    :: Fmt.str "check:\n\t@@%s\n\n"
         ( List.map testlist ~f:(fun tst ->
               Fmt.str "test -f '%s'" (success_path tst) )
         |> String.concat ~sep:" \\\n      && " )
    :: List.map testlist ~f:(fun test ->
           Fmt.str "# Test %s with %s\n%s:\n\t%ssh %s" (unique_name test)
             (Shell.to_string t.shell) (success_path test)
             (if t.verbose then "" else "@")
             (run_test_path test) )
    |> String.concat ~sep:"\n"

  let scripts t testlist =
    List.concat_map testlist ~f:(fun test ->
        [ (script_path test, script_content t test)
        ; (run_test_path test, run_test_script t test)
        ; (script_display test, display_script t test)
        ; (script_opti_display test, display_opti_script t test) ] )

  let contents t ~path testlist =
    let test_path = path in
    let makefile_path = Fmt.str "%s/Makefile" test_path in
    [ `Directory test_path; `Directory (test_path // "script")
    ; `File (makefile_path, makefile t testlist)
    ; `File (test_path // make_report_path t, make_report_content t testlist) ]
    @ List.map (scripts t testlist) ~f:(fun (spath, content) ->
          `File (Fmt.str "%s/%s" test_path spath, content) )
end

module Test_directory = struct
  type t =
    { shell_tests: Shell_directory.t list
    ; important_shells: string list
    ; verbose: bool }

  let help t =
    let shell_names = List.map t.shell_tests ~f:Shell_directory.name in
    let code_list l =
      List.map l ~f:(Fmt.str "`%s`") |> String.concat ~sep:", " in
    Fmt.str
      "Genspio Tests Master Makefile\n\
       =============================\n\n\
       Type `make` to see this help.\n\n\
       Other targets include:\n\n\
       * `make run-<shell-test>` where `shell-name` can be one of:\n\
      \  %s.\n\
       * `make run-all` to attempt to run all the tests on all the shells.\n\
       * `make report` generate the `report.md` file.\n\
       * `make check`: check the success of the test for all the important \n\
      \  shells (%s).\n"
      (code_list shell_names)
      (code_list t.important_shells)

  let makefile t =
    let shell_reports =
      List.map t.shell_tests ~f:(fun sh ->
          Shell_directory.name sh // "report.md" )
      |> String.concat ~sep:" " in
    let shell_names = List.map t.shell_tests ~f:Shell_directory.name in
    let shell_run_targets =
      List.map shell_names ~f:(Fmt.str "run-%s") |> String.concat ~sep:" " in
    [ Fmt.str ".PHONY: run-all all clean clean-reports report check %s\n"
        shell_run_targets; Fmt.str "all:\n\t@@cat help.md"
    ; Fmt.str "check: %s\n"
        ( List.filter_map t.shell_tests ~f:(fun sht ->
              if
                List.mem t.important_shells ~equal:String.equal
                  (sht.Shell_directory.shell |> Shell.to_string)
              then Some (Fmt.str "check-%s" (Shell_directory.name sht))
              else None )
        |> String.concat ~sep:" " ); "report: report.md"
    ; Fmt.str "report.md: %s\n\tcat %s > report.md" shell_reports shell_reports
    ; Fmt.str "clean-reports:\n\t@@rm report.md %s" shell_reports
    ; Fmt.str "clean: clean-reports\n\t@@%s"
        ( List.map shell_names ~f:(Fmt.str "( cd %s ; $(MAKE) clean ; )")
        |> String.concat ~sep:" ; " ); Fmt.str "run-all: %s" shell_run_targets
    ]
    @ List.concat_map t.shell_tests ~f:(fun shtest ->
          let dir = Shell_directory.name shtest in
          [ Fmt.str "%s/report.md:\n\t@@ ( cd %s ; $(MAKE) report ; )" dir dir
          ; Fmt.str "run-%s:\n\t@@ ( cd %s ; $(MAKE) ; )" dir dir
          ; Fmt.str "check-%s:\n\t@@ ( cd %s ; $(MAKE) check ; )" dir dir ] )
    |> String.concat ~sep:"\n"

  let contents t ~path testlist =
    [ `Directory path; `File (path // "help.md", help t)
    ; `File (path // "Makefile", makefile t) ]
    @ List.concat_map t.shell_tests ~f:(fun shtest ->
          (* let comp = Shell_directory.{ shell; verbose = t.verbose } in *)
          Shell_directory.contents shtest
            ~path:(path // Shell_directory.name shtest)
            testlist )
end

module Example = struct
  type t =
    | EDSL :
        { name: string
        ; description: string
        ; code: 'a Genspio.EDSL.t
        ; ocaml_code: string
        ; show: [`Stdout | `Stderr | `Pretty_printed | `Compiled] list }
        -> t

  let make ?(show = [`Pretty_printed]) ~ocaml name description code =
    EDSL {name; description; code; show; ocaml_code= ocaml}

  let default_demo_url =
    "https://smondet.gitlab.io/genspio-web-demo/genspio-master/index.html"

  let run fmt =
    let ff = Fmt.pf in
    function
    | EDSL {code; description; ocaml_code; name; show} ->
        let md_code_block lang code =
          let fence = String.make 50 '`' in
          ff fmt "%s%s@\n%s@\n%s@\n@\n" fence lang (String.strip code) fence
        in
        let if_show s f =
          if List.mem show s ~equal:Poly.equal then f () else () in
        let try_url =
          let base =
            try Sys.getenv "genspio_demo_url" with _ -> default_demo_url in
          Fmt.str "%s?input=%s" base (Uri.pct_encode ocaml_code) in
        ff fmt "@\n%s@\n%s@\n@\n%s@ [[Try-Online](%s)]@\n@\n" name
          (String.map name ~f:(fun _ -> '-'))
          description try_url ;
        md_code_block "ocaml" ocaml_code ;
        if_show `Pretty_printed (fun () ->
            ff fmt "Pretty-printed:@\n@\n" ;
            md_code_block "scheme" (Genspio.Compile.to_string_hum code) ) ;
        ( match Genspio.Compile.To_posix.(string ~options:multi_line) code with
        | Ok script ->
            let tmp = Filename.temp_file "genspio-example" ".sh" in
            let o = Caml.open_out tmp in
            Caml.Printf.fprintf o "\n%s\n" script ;
            Caml.close_out o ;
            (* ff fmt "@[<hov 2>* Compiled:@ `%s`@ (%d bytes)@]@\n" tmp (String.length script); *)
            let out = Filename.temp_file "genspio-example" ".out" in
            let err = Filename.temp_file "genspio-example" ".err" in
            let result =
              Sys.command (Fmt.str "bash %s > %s 2> %s" tmp out err) in
            (* ff fmt "    *@[<hov 2> Std-OUT:@ `%s`@]@\n" out; *)
            (* ff fmt "    *@[<hov 2> Std-ERR:@ `%s`@]@\n" err; *)
            let show_file name path =
              let fence = String.make 50 '`' in
              ff fmt "@\n%s:@\n@\n%s@\n" name fence ;
              let i = Caml.open_in path in
              let rec loop () =
                try
                  ff fmt "%c" @@ Caml.input_char i ;
                  loop ()
                with _ -> () in
              loop () ; ff fmt "@\n%s@\n@\n" fence in
            if_show `Compiled (fun () ->
                ff fmt "Compiled to POSIX (%d bytes):@\n@\n"
                  (String.length script) ;
                md_code_block "shell" script ) ;
            ff fmt "@[<hov 2>Running@ *it*@ " ;
            ( match result with
            | 0 -> ff fmt "**succeeds**."
            | other -> ff fmt "**fails**;@ returns %d." other ) ;
            ff fmt "@]@\n@\n" ;
            if_show `Stdout (fun () -> show_file "Standard output" out) ;
            if_show `Stderr (fun () -> show_file "Standard error" err)
        | Error e ->
            ff fmt "Compilation **fails** with:@\n@\n" ;
            md_code_block "" (Genspio.Compile.To_posix.error_to_string e) ) ;
        ff fmt "%!"
end
