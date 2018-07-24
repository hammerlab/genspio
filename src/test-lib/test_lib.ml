open Nonstd
module String = Sosa.Native_string

let ( // ) = Filename.concat

module Test = struct
  type t =
    | Exits of
        { no_trap: bool
        ; name: string
        ; args: string list
        ; returns: int
        ; script: unit Genspio.Language.t }

  let exits ?(no_trap= false) ?name ?(args= []) returns script =
    let name = Option.value name ~default:(sprintf "no-name-%d" returns) in
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
        ~get_version
    in
    let busybox =
      make_shell "busybox"
        ~command:(fun s args -> ["busybox"; "ash"; "-x"; s] @ args)
        ~get_version:"busybox | head -n 1"
    in
    let package_version package =
      (* for when there is no `--version`, `-V`, etc. we go the “debian” way *)
      sprintf "dpkg -s %s | grep ^Version" package
    in
    [ dash_like "dash" ~get_version:(package_version "dash")
    ; dash_like "bash" ~get_version:"bash --version | head -n 1"
    ; dash_like "sh" ~get_version:(package_version "sh")
    ; busybox
    ; dash_like "ksh" ~get_version:"ksh --version 2>&1"
    ; dash_like "mksh" ~get_version:(package_version "mksh")
    ; dash_like "posh" ~get_version:(package_version "posh")
    ; dash_like "zsh" ~get_version:"zsh --version" ]
end

module Shell_directory = struct
  type t =
    { shell: Shell.t
    ; compilation: [`Std_one_liner | `Std_multi_line]
    ; verbose: bool }

  let name t =
    sprintf "%s-%s" (Shell.to_string t.shell)
      ( match t.compilation with
      | `Std_multi_line -> "StdML"
      | `Std_one_liner -> "Std1L" )

  let unique_name = function
    | Exits {no_trap; name; args; returns; script} ->
        sprintf "test-%s-%s-A%d-R%d-%s"
          (if no_trap then "noT" else "T")
          (let long =
             String.map name ~f:(function
               | ('a'..'z' | 'A'..'Z' | '0'..'9') as c -> c
               | _ -> '_' )
           in
           if String.length long > 30 then
             String.sub_exn long ~index:0 ~length:30
           else long)
          (List.length args) returns
          ( Marshal.to_string script [Marshal.Closures]
          |> Digest.string |> Digest.to_hex
          |> String.sub_exn ~index:0 ~length:10 )

  let script_path test = "script" // sprintf "%s-script.sh" (unique_name test)

  let run_test_path test =
    "script" // sprintf "%s-run-test.sh" (unique_name test)

  let script_display test =
    "script" // sprintf "%s-display.scm" (unique_name test)

  let success_path test = sprintf "_success/%s.txt" @@ unique_name test

  let failure_path test = sprintf "_failure/%s.txt" @@ unique_name test

  let stdout_path test = sprintf "_log/%s/stdout.txt" @@ unique_name test

  let stderr_path test = sprintf "_log/%s/stderr.txt" @@ unique_name test

  let display_script t = function
    | Exits {no_trap; name; args; returns; script} ->
        Genspio.Compile.to_string_hum script

  let run_test_script t =
    let test_name = name t in
    function
    | Exits {no_trap; name; args; returns; script} as test ->
        let fill_result_file which =
          let echos =
            [ sprintf "- Returns $RRR (expected: %d)." returns
            ; sprintf "- Script: \\`%s\\`" (script_path test)
            ; sprintf "- Pretty-printed: \\`%s\\`" (script_display test)
            ; sprintf "- Test-runner: \\`%s\\`" (run_test_path test)
            ; sprintf "- STDOUT: \\`%s\\`" (stdout_path test)
            ; sprintf "- STDERR: \\`%s\\`" (stderr_path test) ]
          in
          let file, first_line =
            match which with
            | `OK ->
                ( success_path test
                , sprintf "- **OK**: \\`%s\\`" (unique_name test) )
            | `KO ->
                ( failure_path test
                , sprintf "- **KO**: \\`%s\\`" (unique_name test) )
          in
          let lines =
            sprintf "printf -- \"%s\\n\" > %s" first_line file
            :: List.map echos ~f:(fun l ->
                   sprintf "printf -- \"    %s\\n\" >> %s" l file )
          in
          String.concat ~sep:"\n" lines
        in
        sprintf
          "mkdir -p _success _failure %s\n\
           %s > %s 2> %s\n\
           export RRR=$?\n\
           if [ $RRR -eq %d ] ; then\n\
           %s\n\
           else\n\
           %s\n               \
           %s\n\
           fi\n"
          (stdout_path test |> Filename.dirname)
          ( t.shell.Shell.command (script_path test) args
          |> List.map ~f:Filename.quote |> String.concat ~sep:" " )
          (stdout_path test) (stderr_path test) returns
          (fill_result_file `OK)
          (fill_result_file `KO)
          ( if t.verbose then
            sprintf "printf 'Test %s with [%s] FAILED\\n' >&2" name test_name
          else "" )

  let script_content t = function
    | Exits {no_trap; name; args; returns; script} ->
      match t.compilation with
      | `Std_one_liner -> Genspio.Compile.to_one_liner ~no_trap script
      | `Std_multi_line -> Genspio.Compile.to_many_lines ~no_trap script

  let make_report_path t = "script" // "make_report.sh"

  let make_report_content t testlist =
    (let open Genspio.EDSL in
    let count_files dir =
      if_then_else
        (exec ["test"; "-d"; dir] |> succeeds)
        (exec ["ls"; "-1"; dir] ||> exec ["wc"; "-l"])
        (exec ["echo"; "No-dir"])
      ||> exec ["tr"; "-d"; "\\n"]
      |> get_stdout |> Byte_array.to_c
    in
    seq
      [ exec
          [ "printf"
          ; sprintf "* Shell: %s, compilation; %s, total tests: %d\\n"
              (Shell.to_string t.shell)
              ( match t.compilation with
              | `Std_one_liner -> "Standard-one-liner"
              | `Std_multi_line -> "Standard-multi-line" )
              (List.length testlist) ]
      ; call
          [ string "printf"
          ; string "    * Failures: %s.\\n"
          ; count_files "_failure/" ]
      ; call
          [ string "printf"
          ; string "    * Successes: %s.\\n"
          ; count_files "_success" ] ])
    |> Genspio.Compile.to_many_lines

  let makefile t testlist =
    sprintf ".PHONY: all clean report check\nall: %s\n\n"
      (List.map testlist ~f:success_path |> String.concat ~sep:" ")
    :: sprintf "clean:\n\trm -fr _success _failure _log\n\n"
    :: sprintf
         "failures.md:\n\t@{ cat _failure/* ; echo '' ; } > failures.md\n\n"
    :: sprintf
         "successes.md:\n\t@{ cat _success/* ; echo '' ; } > successes.md\n\n"
    :: sprintf "report: failures.md successes.md\n\t@sh %s > report.md\n\n"
         (make_report_path t)
    :: sprintf "check:\n\t@%s\n\n"
         ( List.map testlist ~f:(fun tst ->
               sprintf "test -f '%s'" (success_path tst) )
         |> String.concat ~sep:" \\\n      && " )
    :: List.map testlist ~f:(fun test ->
           sprintf "# Test %s with %s\n%s:\n\t%ssh %s" (unique_name test)
             (Shell.to_string t.shell) (success_path test)
             (if t.verbose then "" else "@")
             (run_test_path test) )
    |> String.concat ~sep:"\n"

  let scripts t testlist =
    List.concat_map testlist ~f:(fun test ->
        [ (script_path test, script_content t test)
        ; (run_test_path test, run_test_script t test)
        ; (script_display test, display_script t test) ] )

  let contents t ~path testlist =
    let test_path = path in
    let makefile_path = sprintf "%s/Makefile" test_path in
    [ `Directory test_path
    ; `Directory (test_path // "script")
    ; `File (makefile_path, makefile t testlist)
    ; `File (test_path // make_report_path t, make_report_content t testlist)
    ]
    @ List.map (scripts t testlist) ~f:(fun (spath, content) ->
          `File (sprintf "%s/%s" test_path spath, content) )
end

module Test_directory = struct
  type t =
    { shell_tests: Shell_directory.t list
    ; important_shells: string list
    ; verbose: bool }

  let help t =
    let shell_names = List.map t.shell_tests ~f:Shell_directory.name in
    let code_list l =
      List.map l ~f:(sprintf "`%s`") |> String.concat ~sep:", "
    in
    sprintf
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
      |> String.concat ~sep:" "
    in
    let shell_names = List.map t.shell_tests ~f:Shell_directory.name in
    let shell_run_targets =
      List.map shell_names ~f:(sprintf "run-%s") |> String.concat ~sep:" "
    in
    [ sprintf ".PHONY: run-all all clean clean-reports report check %s\n"
        shell_run_targets
    ; sprintf "all:\n\t@cat help.md"
    ; sprintf "check: %s\n"
        ( List.filter_map t.shell_tests ~f:(fun sht ->
              if
                List.mem ~set:t.important_shells
                  (sht.Shell_directory.shell |> Shell.to_string)
              then Some (sprintf "check-%s" (Shell_directory.name sht))
              else None )
        |> String.concat ~sep:" " )
    ; "report: report.md"
    ; sprintf "report.md: %s\n\tcat %s > report.md" shell_reports shell_reports
    ; sprintf "clean-reports:\n\t@rm report.md %s" shell_reports
    ; sprintf "clean: clean-reports\n\t@%s"
        ( List.map shell_names ~f:(sprintf "( cd %s ; $(MAKE) clean ; )")
        |> String.concat ~sep:" ; " )
    ; sprintf "run-all: %s" shell_run_targets ]
    @ List.concat_map t.shell_tests ~f:(fun shtest ->
          let dir = Shell_directory.name shtest in
          [ sprintf "%s/report.md:\n\t@ ( cd %s ; $(MAKE) report ; )" dir dir
          ; sprintf "run-%s:\n\t@ ( cd %s ; $(MAKE) ; )" dir dir
          ; sprintf "check-%s:\n\t@ ( cd %s ; $(MAKE) check ; )" dir dir ] )
    |> String.concat ~sep:"\n"

  let contents t ~path testlist =
    [ `Directory path
    ; `File (path // "help.md", help t)
    ; `File (path // "Makefile", makefile t) ]
    @ List.concat_map t.shell_tests ~f:(fun shtest ->
          (* let comp = Shell_directory.{ shell; verbose = t.verbose } in *)
          Shell_directory.contents shtest
            ~path:(path // Shell_directory.name shtest)
            testlist )
end

module Example = struct
  type t =
    | EDSL:
        { name: string
        ; description: string
        ; code: 'a Genspio.EDSL.t
        ; ocaml_code: string
        ; show: [`Stdout | `Stderr | `Pretty_printed | `Compiled] list }
        -> t

  let make ?(show= [`Pretty_printed]) ~ocaml name description code =
    EDSL {name; description; code; show; ocaml_code= ocaml}

  let default_demo_url =
    "https://smondet.gitlab.io/genspio-web-demo/genspio-master/index.html"

  let run fmt =
    let ff = Format.fprintf in
    function
    | EDSL {code; description; ocaml_code; name; show} ->
        let md_code_block lang code =
          let fence = String.make 50 '`' in
          ff fmt "%s%s@\n%s@\n%s@\n@\n" fence lang (String.strip code) fence
        in
        let if_show s f = if List.mem s show then f () else () in
        let try_url =
          let base =
            try Sys.getenv "genspio_demo_url" with _ -> default_demo_url
          in
          sprintf "%s?input=%s" base (Uri.pct_encode ocaml_code)
        in
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
            let o = open_out tmp in
            Printf.fprintf o "\n%s\n" script ;
            close_out o ;
            (* ff fmt "@[<hov 2>* Compiled:@ `%s`@ (%d bytes)@]@\n" tmp (String.length script); *)
            let out = Filename.temp_file "genspio-example" ".out" in
            let err = Filename.temp_file "genspio-example" ".err" in
            let result =
              Sys.command (sprintf "bash %s > %s 2> %s" tmp out err)
            in
            (* ff fmt "    *@[<hov 2> Std-OUT:@ `%s`@]@\n" out; *)
            (* ff fmt "    *@[<hov 2> Std-ERR:@ `%s`@]@\n" err; *)
            let show_file name path =
              let fence = String.make 50 '`' in
              ff fmt "@\n%s:@\n@\n%s@\n" name fence ;
              let i = open_in path in
              let rec loop () =
                try
                  ff fmt "%c" @@ input_char i ;
                  loop ()
                with _ -> ()
              in
              loop () ; ff fmt "@\n%s@\n@\n" fence
            in
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
