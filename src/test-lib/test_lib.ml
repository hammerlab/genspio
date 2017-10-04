
open Nonstd
module String = Sosa.Native_string
let (//) = Filename.concat

module Test = struct
  type t =
    | Exits of {
        no_trap: bool;
        name: string;
        args: string list;
        returns: int;
        script: unit Genspio.Language.t;
      }

  let exits ?(no_trap = false) ?name ?(args = []) returns script =
    let name =
      Option.value name ~default:(sprintf "no-name-%d" returns) in
    [Exits {no_trap; name; args; returns; script}]

end
open Test

module Shell = struct
  type t = {
    executable: string;
    command: string -> string list -> string list;
    get_version: string;
  }
  let make_shell executable ~command ~get_version =
    {executable; command; get_version}

  let to_string t = t.executable

  let known_shells () =
    (* let exec l = *)
    (*   List.map ~f:Filename.quote l |> String.concat ~sep:" " in *)
    let dash_like bin ~get_version =
      make_shell bin
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
      sprintf "dpkg -s %s | grep ^Version" package in
    [
      dash_like "dash" ~get_version:(package_version "dash");
      dash_like "bash" ~get_version:"bash --version | head -n 1";
      dash_like "sh" ~get_version:(package_version "sh");
      busybox;
      dash_like "ksh" ~get_version:"ksh --version 2>&1";
      dash_like "mksh" ~get_version:(package_version "mksh");
      dash_like "posh" ~get_version:(package_version "posh");
      dash_like "zsh" ~get_version:"zsh --version";
    ]
end

module Shell_directory = struct
  type t = {
    shell: Shell.t;
    verbose: bool;
  }

  let unique_name =
    function
    | Exits { no_trap; name; args; returns; script } ->
      sprintf "test-%s-%s-A%d-R%d-%s"
        (if no_trap then "noT" else "T")
        (let long =
           String.map name ~f:begin function
          | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' as c -> c
          | _ -> '_'
          end in
         if String.length long > 30
         then String.sub_exn long ~index:0 ~length:30
         else long)
        (List.length args)
        returns
        (Marshal.to_string script [Marshal.Closures] |> Digest.string
         |> Digest.to_hex |> String.sub_exn ~index:0 ~length:10)

  let script_path test = "script" // sprintf "%s-script.sh" (unique_name test)
  let run_test_path test = "script" // sprintf "%s-run-test.sh" (unique_name test)

  let success_path test = sprintf "_success/%s.txt" @@ unique_name test
  let failure_path test = sprintf "_failure/%s.txt" @@ unique_name test
  let stdout_path test = sprintf "_log/%s/stdout.txt" @@ unique_name test
  let stderr_path test = sprintf "_log/%s/stderr.txt" @@ unique_name test

  let run_test_script t =
    function
    | Exits { no_trap; name; args; returns; script } as test ->
      let fill_result_file which =
        let echos = [
          sprintf "- Returns $RRR (expected: %d)." returns;
          sprintf "- Script: \\`%s\\`" (script_path test);
          sprintf "- Test-runner: \\`%s\\`" (run_test_path test);
          sprintf "- STDOUT: \\`%s\\`" (stdout_path test);
          sprintf "- STDERR: \\`%s\\`" (stderr_path test);
        ] in
        let file, first_line =
          match which with
          | `OK -> success_path test, sprintf "- **OK**: \\`%s\\`" (unique_name test)
          | `KO -> failure_path test, sprintf "- **KO**: \\`%s\\`" (unique_name test)
        in
        let lines =
          sprintf "echo \"%s\" > %s" first_line file
          ::
          List.map echos ~f:(fun l -> sprintf "echo \"    %s\" >> %s" l file)
        in
        String.concat ~sep:"\n" lines in
      sprintf "mkdir -p _success _failure %s\n\
               %s > %s 2> %s\n\
               export RRR=$?\n\
               if [ $RRR -eq %d ] ; then\n\
               %s\n\
               else\n\
               %s
               %s\n\
               fi\n"
        (stdout_path test |> Filename.dirname)
        (t.shell.Shell.command (script_path test) args
         |> List.map ~f:Filename.quote |> String.concat ~sep:" ")
        (stdout_path test)
        (stderr_path test)
        returns
        (fill_result_file `OK)
        (fill_result_file `KO)
        (if t.verbose
         then
           sprintf "printf 'Test %s with [%s] FAILED\\n' >&2"
             name (Shell.to_string t.shell)
         else "")

  let script_content =
    function
    | Exits { no_trap; name; args; returns; script } ->
      Genspio.Compile.to_many_lines ~no_trap script

  let make_report_path t = "script" // "make_report.sh"
  let make_report_content t testlist =
    Genspio.EDSL.(
      let count_files dir =
        if_then_else (exec ["test"; "-d"; dir] |> succeeds)
          (exec ["ls"; "-1"; dir] ||> exec ["wc"; "-l"])
          (exec ["echo"; "No-dir"])
        ||> exec ["tr"; "-d"; "\\n"]
        |> output_as_string |> to_c_string
      in
      seq [
        exec ["printf";
              sprintf "* Shell: %s, total tests: %d\\n"
                (Shell.to_string t.shell) (List.length testlist)];
        call [string "printf";
              string "    * Failures: %s.\\n";
              count_files "_failure/"];
        call [string "printf";
              string "    * Successes: %s.\\n";
              count_files "_success"];
      ]
    )
    |> Genspio.Compile.to_many_lines

  let makefile t testlist =
    sprintf ".PHONY: all clean report check\nall: %s\n\n"
      (List.map testlist ~f:(success_path)
       |> String.concat ~sep:" ")
    :: sprintf "clean:\n\trm -fr _success _failure _log\n\n"
    :: sprintf "failures.md:\n\t@{ cat _failure/* ; echo '' ; } > failures.md\n\n"
    :: sprintf "successes.md:\n\t@{ cat _success/* ; echo '' ; } > successes.md\n\n"
    :: sprintf "report: failures.md successes.md\n\t@sh %s > report.md\n\n"
      (make_report_path t)
    :: sprintf "check:\n\t@%s\n\n"
      (List.map testlist ~f:(fun tst -> sprintf "test -f '%s'" (success_path tst))
       |> String.concat ~sep:" \\\n      && ")
    ::
    (List.map testlist ~f:(fun test ->
         sprintf "# Test %s with %s\n%s:\n\t%ssh %s"
           (unique_name test) (Shell.to_string t.shell)
           (success_path test)
           (if t.verbose then "" else "@")
           (run_test_path test))
    )
    |> String.concat ~sep:"\n"

  let scripts t testlist =
    List.concat_map testlist ~f:(fun test ->
        [
          script_path test, script_content test;
          run_test_path test, run_test_script t test;
        ])

  let contents t ~path testlist =
    let test_path = path in
    let makefile_path = sprintf "%s/Makefile" test_path in
    [
      `Directory test_path;
      `Directory (test_path // "script");
      `File (makefile_path, makefile t testlist);
      `File (test_path // make_report_path t, make_report_content t testlist);
    ]
    @ List.map (scripts t testlist) ~f:(fun (spath, content) ->
        `File (sprintf "%s/%s" test_path spath, content))

end

module Test_directory = struct
  type t = {
    shells: Shell.t list;
    important_shells: string list;
    verbose: bool;
  }

  let help t =
    let shell_names = List.map t.shells ~f:Shell.to_string in
    sprintf
    "Genspio Tests Master Makefile\n\
     =============================\n\n\
     Type `make` to see this help.\n\n\
     Other targets include:\n\n\
     * `make run-<shell-name>` where `shell-name` can be one of %s.\n\
     * `make run-all` to attempt to run all the tests on all the shells.\n\
     * `make report` generate the `report.md` file.\n\
    "
    (String.concat ~sep:", " shell_names)

  let makefile t =
    let shell_reports =
      List.map t.shells ~f:(fun sh -> Shell.to_string sh // "report.md")
      |> String.concat ~sep:" " in
    let shell_names = List.map t.shells ~f:Shell.to_string in
    let shell_run_targets =
      List.map shell_names ~f:(sprintf "run-%s")|> String.concat ~sep:" " in
    [
      sprintf ".PHONY: run-all all clean clean-reports report check %s\n" shell_run_targets;
      sprintf "all:\n\t@cat help.md";
      sprintf "check: %s\n"
        (List.map t.important_shells ~f:(sprintf "check-%s")
         |> String.concat ~sep:" ");
      "report: report.md";
      sprintf "report.md: %s\n\tcat %s > report.md"
        shell_reports shell_reports;
      sprintf "clean-reports:\n\t@rm report.md %s" shell_reports;
      sprintf "clean: clean-reports\n\t@%s"
        (List.map shell_names ~f:(sprintf "( cd %s ; make clean ; )")
         |> String.concat ~sep:" ; ");
      sprintf "run-all: %s" shell_run_targets;
    ]
    @ List.concat_map t.shells ~f:(fun sh ->
        let dir =  Shell.to_string sh in
        [
          sprintf "%s/report.md:\n\t@ ( cd %s ; make report ; )" dir dir;
          sprintf "run-%s:\n\t@ ( cd %s ; make ; )" dir dir;
          sprintf "check-%s:\n\t@ ( cd %s ; make check ; )" dir dir;
        ])
    |> String.concat ~sep:"\n"

  let contents t ~path testlist =
    [
      `Directory path;
      `File (path // "help.md", help t);
      `File (path // "Makefile", makefile t);
    ]
    @
    List.concat_map t.shells ~f:(fun shell ->
        let comp = Shell_directory.{ shell; verbose = t.verbose } in
        Shell_directory.contents
          comp ~path:(path // Shell.to_string shell) testlist)

end
