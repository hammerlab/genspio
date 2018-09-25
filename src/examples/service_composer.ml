(*md This example creates a family of scripts using the `EDSL` API,
and in particular the `Dispatcher_script` and `Script_with_describe`
modules.

A simple way to generate and install the scripts is:

    genspio_service_composer=_build/default/src/examples/service_composer.exe
    jbuilder build $genspio_service_composer
    $genspio_service_composer --name cosc --output-path $BINNPATH
  
The `cosc*` scripts will be installed and ready to use in `$BINPATH`
as long as the path is part of the `$PATH` variable.
Then one can just try:

    cosc --help

Quite a few scripts will have been created:

    $BINPATH/cosc
    $BINPATH/cosc-manual
    $BINPATH/cosc-version
    $BINPATH/cosc-attach
    $BINPATH/cosc-example
    $BINPATH/cosc-logs
    $BINPATH/cosc-configuration
    $BINPATH/cosc-configuration-initialize
    $BINPATH/cosc-configuration-display
    $BINPATH/cosc-configuration-addjob
    $BINPATH/cosc-configuration-removejob
    $BINPATH/cosc-configuration-destroy
    $BINPATH/cosc-start
    $BINPATH/cosc-status
    $BINPATH/cosc-kill

The scripts generated with `Dispatcher_script` also know about aliases, e.g.
`cosc config show` is actually able to call `cosc-configuration-display`.


*)
open Nonstd
module String = Sosa.Native_string

let ( // ) = Filename.concat

let msg fmt = ksprintf (eprintf "%s\n%!") fmt

module Gedsl = Genspio.EDSL

let cmdf fmt =
  ksprintf
    (fun s ->
      match Sys.command s with
      | 0 -> ()
      | other -> ksprintf failwith "CMD: %S failed with %d" s other )
    fmt

module Version = struct
  let version =
    lazy
      Unix.(
        gettimeofday () |> gmtime
        |> fun { tm_sec
               ; tm_min
               ; tm_hour
               ; tm_mday
               ; tm_mon
               ; tm_year
               ; tm_wday
               ; tm_yday
               ; tm_isdst } ->
        sprintf "%4d%02d%02d.%02d%02d%02d" (1900 + tm_year) (1 + tm_mon)
          tm_mday tm_hour tm_min tm_sec)

  let get () = Lazy.force version

  let str () = Gedsl.str (get ())
end

(*md A lot of (too much?) attention has been spent making the “root”
name of the scripts parametrizable (the string `cosc` in the example
above).

The `Script` module wraps the scripts as “relative” paths,
descriptions, and the actual script contents.


 *)
module Script = struct
  type t =
    { relative_path: string list
    ; description: string
    ; make: root:string -> unit Gedsl.t }

  let make relative_path ~description make = {relative_path; description; make}

  (*md
See below what `relative_path` means:

```ocaml
let path =
  output_path // String.concat ~sep:"-" (root :: t.relative_path)
```

The function `write` is the only real I/O of this whole OCaml program.
 *)
  let write ?(compiler = `Slow_flow) t ~output_path ~root =
    let path =
      output_path // String.concat ~sep:"-" (root :: t.relative_path)
    in
    let o = open_out path in
    msg "Outputting “%s” to %s\n%!" t.description path ;
    ( match compiler with
    | `Slow_flow ->
        Format.(
          fprintf
            (formatter_of_out_channel o)
            "#!/bin/sh\n\n%a\n" Genspio.Compile.To_slow_flow.Script.pp_posix
            (Genspio.Compile.To_slow_flow.compile
               (t.make ~root |> Genspio.Transform.Constant_propagation.process)))
    | `Standard ->
        fprintf o "#!/bin/sh\n\n%s\n"
          (Genspio.Compile.to_many_lines (t.make ~root)) ) ;
    close_out o ; cmdf "chmod +x %s" path
end

(*md Configuration of the scripts is bootstrapped with an environment
variable, which gives the script a root-path to start from. Then, the
remaining configuration lies in files within the root path, it is
editable by the scripts (e.g. with `cosc config init`,
`cosc config addjob`, etc.).

 *)
module Environment = struct
  type t =
    { prefix: string
    ; default_screen_name: string option
    ; default_configuration_path: string }

  let make ?default_screen_name
      ?(default_configuration_path = "/tmp/service_composer_config.d") prefix =
    {default_screen_name; default_configuration_path; prefix}

  open Gedsl

  (*md
The function `posixish_hash` creates a script that uses POSIX's
[cksum](http://pubs.opengroup.org/onlinepubs/009695299/utilities/cksum.html)
to output a stronger hash.
*)
  let posixish_hash path =
    let cksum = call [str "cksum"] in
    seq
      [ call [str "cat"; path] ||> cksum
      ; call [str "cat"; path]
        ||> exec ["tr"; "0123456789a-z"; "98765A-Z43210"]
        ||> cksum ]
    ||> exec ["tr"; "-d"; "\\n "]

  let env_or s default_value =
    let g = getenv (str s) in
    get_stdout
      (if_then_else
         Str.(g =$= str "")
         (exec ["printf"; "%s"; default_value])
         (call [str "printf"; str "%s"; g]))

  let var_configuration_path t = t.prefix ^ "_root"

  let configuration_path t =
    env_or (var_configuration_path t) t.default_configuration_path

  let make_default_screen_name t =
    match t.default_screen_name with
    | Some s -> str s
    | None ->
        let tmp = tmp_file "make-default-screen-name" in
        seq
          [ printf (str "session-") []
          ; write_stdout ~path:tmp#path
              (seq
                 [ printf (str "%s\\n%s\\n%s\\n")
                     [ str t.prefix
                     ; str t.default_configuration_path
                     ; configuration_path t ] ])
          ; posixish_hash tmp#path ]
        |> get_stdout_one_line

  let screen_name_path t = configuration_path t /// str "screen-session-name"

  let init ?screen_name t =
    check_sequence
      [ ("mkdir-path", mkdir_p @@ configuration_path t)
      ; ( "set-screen-name"
        , write_stdout ~path:(screen_name_path t)
            (printf (str "%s\\n")
               [Option.value screen_name ~default:(make_default_screen_name t)])
        ) ]

  let is_initialized t =
    call [str "test"; str "-s"; screen_name_path t] |> succeeds_silently

  let ensure_init t =
    if_then_else (is_initialized t) nop
      (seq
         [ say "Configuration is not initialized (%s)" [configuration_path t]
         ; fail "ERROR: Not-initialized" ])

  let screen_name t =
    get_stdout_one_line
      (seq [ensure_init t; call [string "cat"; screen_name_path t]])

  let on_jobs t f =
    let open Gedsl in
    call
      [str "find"; configuration_path t; str "-name"; str "*.job"; str "-print"]
    ||> on_stdin_lines f

  let display t =
    let open Gedsl in
    let env_var v default =
      say " * `%s`, value: '%s' (default: %s)"
        [str (v t); getenv (str (v t)); str default]
    in
    seq
      [ say "Environment variables: " []
        (* ; env_var var_screen_name t.default_screen_name *)
      ; env_var var_configuration_path t.default_configuration_path
      ; if_seq (is_initialized t)
          ~t:[say "Screen session name: '%s'" [screen_name t]]
          ~e:[say "Screen session name not initialized." []] ]
end

(*md The output of the `cosc manual` command is the processed content of the
`Manual._global_` variable; a list of Markdown strings, accumulated
throughout this file.
*)
module Manual = struct
  type item =
    | Raw of string
    | Root_env of (root:string -> Environment.t -> item list)
    | Extended of {yes: item list; no: item list}

  let _global_ : item list ref = ref []

  let add l = _global_ := !_global_ @ l

  let raw s = Raw s

  let from f = [Root_env f]

  let extended ?(no = []) yes = [Extended {yes; no}]

  let raws l = List.map l ~f:raw

  let title s = raws [s; String.make (String.length s) '='; ""]

  let section s = raws [s; String.make (String.length s) '-'; ""]

  let wrap ?(indent = 0) ?(columns = 72) s =
    let buf = Buffer.create 42 in
    let indentation = String.make indent ' ' in
    let rec assemble col = function
      | [] -> ()
      | one :: more ->
          let potential = col + String.length one + 1 in
          if potential > columns then (
            Buffer.add_string buf ("\n" ^ indentation ^ one) ;
            assemble (String.length one) more )
          else (
            Buffer.add_string buf ((if col = 0 then "" else " ") ^ one) ;
            assemble potential more )
    in
    let words =
      String.split s ~on:(`Character ' ')
      |> List.map ~f:String.strip
      |> List.filter ~f:(( <> ) "")
    in
    assemble 0 words ; Buffer.contents buf

  let par s = raws [wrap s; ""]

  let code_block s = raws (["```"] @ s @ ["```"; ""])

  let list l =
    raws (List.map l ~f:(fun p -> sprintf "* %s" (wrap ~indent:2 p)) @ [""])

  (* `StringLabels.uppercase` is deprecated but
     `StringLabels.uppercase_ascii` is not available in OCaml 4.03.0
     which will still support.
  *)
  [@@@warning "-3"]

  let pre_title root = StringLabels.uppercase root

  [@@@warning "+3"]

  let () =
    add
    @@ from (fun ~root env ->
           ksprintf title "%s: Compose Processes With Screen" (pre_title root)
       )
    @ from (fun ~root env ->
          ksprintf par
            "The `%s*` scripts are a family of POSIX shell executables that \
             manage a set of long running processes in a GNU-Screen session. \
             Current version is `%s`."
            root
            Version.(get ())
          @ ksprintf par
              "The  configuration is stored in a directory: the root path can \
               be itself configured with the `$%s` environment variable \
               (default value: `%s`). One edits the configuration by calling \
               `%s config {addjob,initialize,destroy,…}`, and displays it \
               with `%s config show`."
              (Environment.var_configuration_path env)
              env.Environment.default_configuration_path root root
          @ par
              "The scripts are generated by an OCaml program which uses the \
               [Genspio](https://smondet.gitlab.io/genspio-doc) EDSL/library. \
               The code generator serves as one of the usage examples of the \
               library, see its \
               [implementation](https://smondet.gitlab.io/genspio-doc/master/service-composer-example.html)."
          @ ksprintf par
              "The code generator can also be used to change a few parameters \
               like the “name-prefix” (`%s` here), or the default value \
               of the configuration path (`%s`). This can be useful to build \
               custom/project-specific scripts that can remain independent \
               from each other without setting an environment variable."
              root env.Environment.default_configuration_path )
    @ extended
        ( section "Installation"
        @ from (fun ~root env ->
              ksprintf par
                "Simply copy `%s*` to somewhere in your `$PATH`, the scripts \
                 depend on a reasonably valid version of `/bin/sh` and GNU \
                 Screen."
                root
              @ ksprintf par
                  "If you are using the code-generator, you can just point \
                   the `--output-path` option at the right directory." ) )
    @ section "Usage"
    @ from (fun ~root env ->
          let intro fmt =
            ksprintf
              (ksprintf par
                 "The basic manual is obtained from the `%s man` command.%s"
                 root)
              fmt
          in
          extended
            (intro
               " The, present, “`README.md`” version is the result of \
                `%s man --extended`."
               root)
            ~no:(intro "")
          @ ksprintf par
              "Then, see `%s --help` first, or for any sub-command try \
               `%s <command> --help`."
              root root )
    @ section "Screen Session Isolation"
    @ from (fun ~root env ->
          ksprintf par
            "`%s` isolates Screen sessions by using their session name." root
          @ ksprintf par
              "The screen session name can be configured a 2 levels:"
          @ list
              [ sprintf
                  "At script-generation time, one can set the default-value \
                   (with the option `--screen-name`)."
              ; sprintf
                  "At configuration time, one can overwrite the value with \
                   `-S`, see `%s config init --help`."
                  root ]
          @ ksprintf par
              "If none of those two options is provided, `%s config init` \
               will generate a name, which is function of the root path and \
               generation parameters and tries to ensure that the session is \
               unique on the host."
              root )
    @ extended
        ( section "Docker Image For the Generator"
        @ from (fun ~root env ->
              let image = "smondet/genspio-doc-dockerfiles:apps406" in
              ksprintf par
                "If you have [`opam`](https://opam.ocaml.org), setting up the \
                 genspio repository is easy (only simple, pure OCaml \
                 dependencies), if not, or if you just like Docker™, the \
                 generator is available in the `%s` image, see:"
                image
              @ code_block
                  [ sprintf "docker run -it %s genspio-service-composer --help"
                      image ] ) )

  let output ~root ~env extended =
    let open Gedsl in
    let rec one = function
      | Raw s -> printf (str "%s\\n") [str s]
      | Root_env f -> seq @@ List.map ~f:one (f ~root env)
      | Extended {yes; no} ->
          if_seq extended ~t:(List.map ~f:one yes) ~e:(List.map ~f:one no)
    in
    seq (List.map !_global_ ~f:one)
end

(*md The `Job` module provides Genspio expressions to uniformly define
the notion of “job:” a process attached to a given `screen` window,
with potential log-keeping.
  
Within the “root” configuration path, a give job “`TheJob`” is
attached to a few files:

* `TheJob.job`: contains the shell command to execute.
* `TheJob.log`: contains the logs (mostly empty if `--no-log` is set).
* `TheJob.options`: contains the options, one per line in
  shell-variable-syntax (e.g. `no_log=true`)
* `TheJob.pid`: stores the PID of the job once started.
* `TheJob-run.sh`: script that is generated to run the job (the PID is
  actually the one of this script, i.e. the parent of all potential
  processes created by the job).
*)
module Job = struct
  open Gedsl

  let name path =
    call [str "basename"; path]
    ||> exec ["sed"; "s/.job$//"]
    |> get_stdout_one_line

  let command path = call [str "cat"; path] |> get_stdout_one_line

  let job_path env name =
    Environment.configuration_path env /// (name ^$^ str ".job")

  let run_path env name =
    Environment.configuration_path env /// (name ^$^ str "-run.sh")

  let log_path env name =
    Environment.configuration_path env /// (name ^$^ str ".log")

  let pid_path env name =
    Environment.configuration_path env /// (name ^$^ str ".pid")

  module Options = struct
    let path env name =
      Environment.configuration_path env /// (name ^$^ str ".options")

    let write ~no_log env name =
      seq
        [ say "Writing options: %s" [path env name]
        ; write_stdout ~path:(path env name)
            (seq
               [ if_then_else no_log
                   (printf (str "no_log=true\\n") [])
                   (printf (str "no_log=false\\n") []) ]) ]

    let no_log env name =
      greps_to (str "no_log=true") @@ call [str "cat"; path env name]
  end

  let get_pid env name =
    let pid = pid_path env name in
    call [str "cat"; pid] |> get_stdout_one_line

  let ps env name ~o = call [str "ps"; str "-q"; get_pid env name; str "-o"; o]

  let ps_stat_exec env name = ps env name ~o:(str "stat=")

  let ps_stat env name = ps_stat_exec env name |> get_stdout_one_line

  let ps_stat_or_fail env name =
    let pid = pid_path env name in
    if_then_else
      (file_exists pid &&& succeeds_silently (ps_stat_exec env name))
      (ps_stat_exec env name)
      (seq [printf (str "None") []; exit 2])

  let ps_cpu env name = ps ~o:(str "cpu=") env name |> get_stdout_one_line

  let is_running env name = succeeds_silently (ps_stat_or_fail env name)

  let run_script env name =
    let runner = run_path env name in
    let mk =
      seq
        [ write_stdout ~path:runner
            (seq
               [ printf (str "#!/bin/sh\\n") []
               ; printf (str "# Script generated by %s\\n") [getenv (str "0")]
               ; printf (str "printf \"$$\" > %s\\n") [pid_path env name]
               ; printf
                   (str "printf \"# Starting on $(date)\\n\" > %s\\n")
                   [log_path env name]
               ; if_seq (Options.no_log env name)
                   ~t:[printf (str "sh %s\\n") [job_path env name]]
                   ~e:
                     [ printf
                         (str "{ sh %s 2>&1 ; } | tee -a %s\\n")
                         [job_path env name; log_path env name] ] ]) ]
    in
    (mk, runner)

  let delete env name =
    let rm p = verbose_call ~prefix:"  -> " [str "rm"; str "-f"; p] in
    seq
      (List.map ~f:rm
         [ job_path env name
         ; log_path env name
         ; pid_path env name
         ; run_path env name
         ; Options.path env name ])
end

(*md The `Screen` module contains Genspio expressions to manipulate a
GNU-Screen session,
see the relevant
[manual](https://www.gnu.org/software/screen/manual/screen.html).
 *)
module Screen = struct
  (*
  *)
  open Gedsl

  let ls env = call [str "screen"; str "-ls"; Environment.screen_name env]

  let is_on env = ls env |> succeeds_silently

  let call ?verbose env l =
    verbose_call ?verbose
      ([str "screen"; str "-S"; Environment.screen_name env] @ l)

  let window_name job = str "J:" ^$^ job

  let ensure_running env =
    if_seq (is_on env)
      ~t:[say "Screen session is running." []]
      ~e:[call env [str "-d"; str "-m"]]
end

(*md

## The Scripts

All the `*_script` modules define one actual script to be generated.


*)
module Configuration_script = struct
  let description = "Manage the configuration."

  let name = "configuration"

  let make () =
    Script.make [name] ~description (fun ~root ->
        Gedsl.Dispatcher_script.make
          ~aliases:
            Gedsl.
              [ (str "show", str "display")
              ; (str "rmjob", str "removejob")
              ; (str "init", str "initialize") ]
          ~name:(sprintf "%s-%s" root name)
          ~description () )
end

module Manual_script = struct
  include Gedsl.Script_with_describe (struct
    let name = "manual"

    let description = "Show the manual."
  end)

  let make ~env () =
    Script.make [name] ~description (fun ~root ->
        let open Gedsl in
        let open Command_line in
        let opts =
          let open Arg in
          flag ["--extended"; "-X"] ~doc:"Provide extra information."
          & flag ["--no-pager"] ~doc:"Do not use a pager."
          & describe_option_and_usage ()
        in
        parse opts (fun ~anon extended no_pager describe ->
            deal_with_describe describe
              [Manual.output ~root ~env extended ||> pager ~disable:no_pager ()]
        ) )
end

module Version_script = struct
  include Gedsl.Script_with_describe (struct
    let name = "version"

    let description = "Show the version information."
  end)

  let make ~env () =
    Script.make [name] ~description (fun ~root ->
        let open Gedsl in
        let open Command_line in
        let opts =
          let open Arg in
          flag ["--extended"; "-X"] ~doc:"Provide extra information"
          & describe_option_and_usage ()
        in
        parse opts (fun ~anon extended describe ->
            deal_with_describe describe
              [ if_seq extended
                  ~t:
                    [ say "%s %s (Genspio %s)"
                        [str root; Version.str (); str Genspio.Meta.version] ]
                  ~e:[say "%s" [Version.str ()]] ] ) )
end

module Init_script = struct
  include Gedsl.Script_with_describe (struct
    let name = "initialize"

    let description = "Initialize the configuration."
  end)

  let make ~env () =
    Script.make ["configuration"; name] ~description (fun ~root ->
        let open Gedsl in
        let open Command_line in
        let opts =
          let open Arg in
          string
            ["--screen-session-name"; "-S"]
            ~doc:
              (sprintf
                 "Set the screen session name (the default is a function of \
                  the root path and other constants of the script)")
            ~default:(Environment.make_default_screen_name env)
          & describe_option_and_usage ()
        in
        parse opts (fun ~anon screen_name describe ->
            deal_with_describe describe
              [Environment.init ~screen_name env; say "Done." []] ) )
end

module Add_job_script = struct
  include Gedsl.Script_with_describe (struct
    let name = "addjob"

    let description = "Add a job to the configuration."
  end)

  let make ~env () =
    Script.make ["configuration"; name] ~description (fun ~root ->
        let open Gedsl in
        let open Command_line in
        let default_none = str "--none--" in
        let opts =
          let open Arg in
          string ["--name"] ~doc:"Job name" ~default:default_none
          & string ["--command"; "-c"] ~doc:"Job command" ~default:default_none
          & string ["--interpreter"; "-i"]
              ~doc:"Job interpreter (default: sh -c)" ~default:(str "sh -c")
          & flag ["--no-log"]
              ~doc:
                "Don't save logs (useful for commands that grab the terminal \
                 like `top`)"
          & describe_option_and_usage ()
        in
        parse opts (fun ~anon name shell_command interpreter no_log describe ->
            let jpath = Job.job_path env name in
            deal_with_describe describe
              [ if_then
                  Str.(name =$= default_none)
                  (fail "option --name is mandatory")
              ; if_then
                  Str.(shell_command =$= default_none)
                  (fail "option --command is mandatory")
              ; mkdir_p @@ Environment.configuration_path env
              ; Job.Options.write ~no_log env name
              ; say "Creating %s" [jpath]
              ; write_stdout ~path:jpath
                  (seq
                     [ interpreter >> exec ["cat"]
                     ; str " '" >> exec ["cat"]
                     ; shell_command >> exec ["sed"; "s/'/'\\\\''/g"]
                     ; str "'\n" >> exec ["cat"] ])
              ; say "Done." [] ] ) )
end

module Remove_job_script = struct
  include Gedsl.Script_with_describe (struct
    let name = "removejob"

    let description = "Remove one or more jobs from the configuration."
  end)

  let make ~env () =
    Script.make ["configuration"; name] ~description (fun ~root ->
        let open Gedsl in
        let open Command_line in
        let opts =
          let open Arg in
          describe_option_and_usage ()
        in
        parse opts (fun ~anon describe ->
            deal_with_describe describe
              [ Elist.iter anon ~f:(fun name ->
                    seq
                      [ (let path = Job.job_path env (name ()) in
                         if_seq (file_exists path)
                           ~t:
                             [ say "Removing %s..." [name ()]
                             ; Job.delete env (name ()) ]
                           ~e:[say "Job %s does not seem to exist..." [name ()]])
                      ] )
              ; say "Done." [] ] ) )
end

module Start_script = struct
  include Gedsl.Script_with_describe (struct
    let name = "start"

    let description = "Start all or a given list of jobs."
  end)

  let make ~env () =
    Script.make [name] ~description (fun ~root ->
        let open Gedsl in
        let open Command_line in
        let opts =
          let open Arg in
          flag ["--all"] ~doc:"Start all jobs"
          & describe_option_and_usage ()
              ~more_usage:
                [ "Use"
                ; sprintf "  %s %s --all" env.Environment.prefix name
                ; "or"
                ; sprintf "  %s %s Job1 .. JobN" env.Environment.prefix name ]
        in
        let start_one name =
          if_then_else (Job.is_running env name)
            (say "* Job '%s' is already running!" [name])
            (let mk, runpath = Job.run_script env name in
             seq
               [ mk
               ; if_seq
                   (file_exists (Job.job_path env name))
                   ~t:
                     [ say "* Starting '%s' in Screen window: '%s'"
                         [name; Screen.window_name name]
                     ; Screen.call env
                         [ str "-X"
                         ; str "screen"
                         ; str "-t"
                         ; Screen.window_name name
                         ; str "sh"
                         ; runpath ] ]
                   ~e:[say "* Job '%s' is not configured!" [name]] ])
        in
        parse opts (fun ~anon all describe ->
            deal_with_describe describe
              [ Screen.ensure_running env
              ; if_seq all
                  ~t:
                    [ say "Starting all jobs from %s"
                        [Environment.configuration_path env]
                    ; Environment.on_jobs env (fun path ->
                          let name = Job.name path in
                          start_one name ) ]
                  ~e:
                    [ Elist.iter anon ~f:(fun item ->
                          let name = item () in
                          seq [say "Starting job '%s':" [name]; start_one name]
                      ) ]
              ; say "Done." [] ] ) )
end

module Configuration_display_script = struct
  include Gedsl.Script_with_describe (struct
    let name = "display"

    let description = "Show the configuration."
  end)

  let make ~env () =
    Script.make ["configuration"; name] ~description (fun ~root ->
        let open Gedsl in
        let open Command_line in
        let opts =
          let open Arg in
          describe_option_and_usage ()
        in
        parse opts (fun ~anon describe ->
            let path = Environment.configuration_path env in
            deal_with_describe describe
              [ say "Configuration path: %s" [path]
              ; Environment.display env
              ; if_seq (is_directory path)
                  ~t:
                    [ Environment.on_jobs env (fun path ->
                          printf
                            (str
                               "Job: '%s'\\n |-> Command: [%s]\\n |-> \
                                Options: %s\\n")
                            [ Job.name path
                            ; Job.command path
                            ; call
                                [ str "cat"
                                ; Job.Options.path env (Job.name path) ]
                              ||> exec ["tr"; "\\n"; ","]
                              |> get_stdout ] ) ]
                  ~e:[say "Configuration is empty (not even a directory)" []]
              ] ) )
end

module Configuration_destroy_script = struct
  include Gedsl.Script_with_describe (struct
    let name = "destroy"

    let description = "Destroy the configuration."
  end)

  let make ~env () =
    Script.make ["configuration"; name] ~description (fun ~root ->
        let open Gedsl in
        let open Command_line in
        let opts =
          let open Arg in
          describe_option_and_usage ()
        in
        parse opts (fun ~anon describe ->
            let path = Environment.configuration_path env in
            deal_with_describe describe
              [ say "Configuration path: %s" [path]
              ; if_seq (is_directory path)
                  ~t:[verbose_call [str "rm"; str "-fr"; path]]
                  ~e:
                    [ say "Configuration is not even a directory: %s" [path]
                    ; fail "FAILURE" ] ] ) )
end

module Attach_script = struct
  include Gedsl.Script_with_describe (struct
    let name = "attach"

    let description = "Attach to the Screen being managed."
  end)

  let go env create =
    let open Gedsl in
    seq
      [ if_seq (Screen.is_on env)
          ~t:
            [ say "Attaching to screen: %s" [Environment.screen_name env]
            ; Screen.call env [str "-x"] ]
          ~e:
            [ if_seq create
                ~t:
                  [ say "Creating screen: %s" [Environment.screen_name env]
                  ; Screen.call env [] ]
                ~e:
                  [ say "There is no screen: %s" [Environment.screen_name env]
                  ; fail "STOPPING" ] ] ]

  let make ~env () =
    Script.make [name] ~description (fun ~root ->
        let open Gedsl in
        let open Command_line in
        let opts =
          let open Arg in
          flag ["--create"] ~doc:"Create if it doesn't exist."
          & describe_option_and_usage ()
        in
        parse opts (fun ~anon create describe ->
            deal_with_describe describe [go env create] ) )
end

module Kill_script = struct
  include Gedsl.Script_with_describe (struct
    let name = "kill"

    let description = "Kill Jobs or the whole Screen session (-a)."
  end)

  let make ~env () =
    Script.make [name] ~description (fun ~root ->
        let open Gedsl in
        let open Command_line in
        let opts =
          let open Arg in
          flag ["--all"; "-a"] ~doc:"Kill everything, incl. the Screen session"
          & describe_option_and_usage ()
        in
        let kills = tmp_file "kill-list" in
        parse opts (fun ~anon kill_em_all describe ->
            deal_with_describe describe
              [ if_seq kill_em_all
                  ~t:[Screen.call env [str "-X"; str "quit"]]
                  ~e:
                    [ kills#set (str "")
                    ; Elist.iter anon ~f:(fun item ->
                          seq
                            [ say "## Processing %s" [item ()]
                            ; if_seq
                                ( Screen.call env
                                    [ str "-Q"
                                    ; str "-p"
                                    ; Screen.window_name (item ())
                                    ; str "-X"
                                    ; str "info" ]
                                |> succeeds_silently )
                                ~t:
                                  [ say "-> Window found, killing now." []
                                  ; Screen.call env
                                      [ str "-p"
                                      ; Screen.window_name (item ())
                                      ; str "-X"
                                      ; str "kill" ]
                                  ; kills#set (str "yes") ]
                                ~e:
                                  [ say "-> Window for job '%s' not found!"
                                      [item ()] ] ] )
                    ; if_seq
                        Str.(kills#get =$= str "")
                        ~t:[say "Nothing was killed …" []] ] ] ) )
end

module Logs_script = struct
  include Gedsl.Script_with_describe (struct
    let name = "logs"

    let description = "Show logs for one or more jobs."
  end)

  let make ~env () =
    Script.make [name] ~description (fun ~root ->
        let open Gedsl in
        let open Command_line in
        let opts =
          let open Arg in
          flag ["--path"] ~doc:"Only output a path on stdout"
          & flag ["--screen"]
              ~doc:
                "Get the screen window dump instead of the (potential) log file"
          & describe_option_and_usage ()
        in
        let cat_file job lp =
          if_seq (file_exists lp)
            ~t:[call [str "cat"; lp]]
            ~e:[say "No logs available for %s" [job]]
        in
        let screen_file job show_path =
          let tmp = tmp_file "screen-dump" in
          seq
            [ Screen.call env
                [ str "-p"
                ; Screen.window_name job
                ; str "-X"
                ; str "hardcopy"
                ; str "-h"
                ; tmp#path ]
            ; if_seq show_path
                ~t:[printf (str "%s\\n") [tmp#path]]
                ~e:[cat_file job tmp#path] ]
        in
        parse opts (fun ~anon just_path screen describe ->
            deal_with_describe describe
              [ Elist.iter anon ~f:(fun name ->
                    let job = name () in
                    let lp = Job.log_path env job in
                    if_seq screen
                      ~t:[screen_file job just_path]
                      ~e:
                        [ if_seq just_path
                            ~t:[printf (str "%s\\n") [lp]]
                            ~e:
                              [ if_seq
                                  (Job.Options.no_log env job)
                                  ~t:
                                    [ say
                                        "Job %s is configured to have no \
                                         logs, try --screen"
                                        [job] ]
                                  ~e:[cat_file job lp] ] ] ) ] ) )
end

module Status_script = struct
  include Gedsl.Script_with_describe (struct
    let name = "status"

    let description = "Get the status(es) of the processes."
  end)

  let make ~env () =
    Script.make [name] ~description (fun ~root ->
        let open Gedsl in
        let open Command_line in
        let opts =
          let open Arg in
          flag ["--short"; "-s"] ~doc:"Don't output a ton of info"
          & describe_option_and_usage ()
        in
        parse opts (fun ~anon short describe ->
            let prefix_output = exec ["sed"; "s/^/    |    /"] in
            deal_with_describe describe
              [ if_seq (Screen.is_on env)
                  ~t:
                    [ say "Screen in ON" []
                    ; if_seq short ~t:[]
                        ~e:
                          [ Screen.ls env ||> prefix_output
                          ; say "  * Windows: %s"
                              [ Screen.call ~verbose:(bool false) env
                                  [str "-Q"; str "-X"; str "windows"]
                                |> get_stdout_one_line ] ] ]
                  ~e:[say "Screen is OFF" []]
              ; Environment.on_jobs env (fun jobpath ->
                    let job = Job.name jobpath in
                    if_seq (Job.is_running env job)
                      ~t:
                        [ say "Job `%s`: PID: %s, CPU: %s, STAT: %s"
                            [ job
                            ; Job.get_pid env job
                            ; Job.ps_cpu env job
                            ; Job.ps_stat env job ]
                        ; if_then (not short)
                            ( call
                                [ str "ps"
                                ; str "f"
                                ; str "-g"
                                ; Job.get_pid env job ]
                            ||> prefix_output ) ]
                      ~e:
                        [ say "Job `%s` is not running (stat: %s)"
                            [ job
                            ; Job.ps_stat_or_fail env job
                              |> get_stdout_one_line ] ] ) ] ) )
end

module Example_script = struct
  let basic env root =
    let call s = sprintf "%s %s" root s in
    let conf = "/tmp/example-basic.d" in
    let cmt fmt = sprintf ("# " ^^ fmt) in
    ( "basic"
    , [ cmt "We setup the configuration root path:"
      ; sprintf "export %s=%s" (Environment.var_configuration_path env) conf
      ; cmt "Show the current configuration:"
      ; call "config show"
      ; cmt "OK, let's initialize configuration:"
      ; call "config init"
      ; cmt "Let's configure a few jobs:"
      ; call
          {sh|config addjob --name DMesg --no-log -c "watch -c -d -n 30 'dmesg -P'"|sh}
      ; call {sh|config addjob --name Top --no-log -c top|sh}
      ; call
          "config addjob --name Dummy --interpreter 'bash -c' \\\n\
          \    -c 'while true ; do sleep 3 ; echo \"$(date)\" ; done'"
      ; cmt "Show the updated configuration:"
      ; call "config show"
      ; cmt "Show the current status:"
      ; call "status"
      ; cmt "Start everything:"
      ; call "start --all"
      ; cmt "Show the updated status:"
      ; call "status"
      ; cmt "Stop everything:"
      ; call "kill --all"
      ; cmt "Show the updated (short) status:"
      ; call "status --short"
      ; cmt "Destroy the configuration:"
      ; call "config destroy" ] )

  let to_script l =
    let prefix = "#####" in
    let add_prefix pre s =
      String.split ~on:(`Character '\n') s
      |> String.concat ~sep:(sprintf "\n%s" pre)
    in
    let prefix_indent = prefix ^ "  " in
    List.concat_map l ~f:(function
      | s when String.strip s |> String.is_prefix ~prefix:"#" ->
          [ sprintf "printf '%s%s\\n'" prefix (String.make 74 '#')
          ; sprintf "printf '%s %%s\\n' %s" prefix
              (Filename.quote (add_prefix prefix_indent s))
          ; sprintf "printf '%s\n'" prefix ]
      | s ->
          [ sprintf "printf '%s >> %%s\\n' %s" prefix
              (Filename.quote (add_prefix prefix_indent s))
          ; s ] )
    |> String.concat ~sep:"\n"

  include Gedsl.Script_with_describe (struct
    let name = "example"

    let description = "Show or run a full example."
  end)

  let make ~env () =
    Script.make [name] ~description (fun ~root ->
        let open Gedsl in
        let open Command_line in
        let default_example = "basic" in
        let opts =
          let open Arg in
          flag ["--run"] ~doc:"Also run the example."
          & string ["--name"; "-n"]
              ~doc:
                (sprintf "Choose the example (default: %S)." default_example)
              ~default:(str default_example)
          & describe_option_and_usage ()
        in
        let run_or_show run example =
          let do_run () =
            let tmp = tmp_file "example-script" in
            seq
              [ tmp#set (to_script example |> str)
              ; say "Running as %s" [tmp#path]
              ; call [str "sh"; tmp#path] ]
          in
          if_seq run ~t:[do_run ()]
            ~e:
              [ printf (str "Example:\\n\\n") []
              ; seq
                  (List.map example ~f:(fun s ->
                       printf (str "    %s\\n") [str s] )) ]
        in
        parse opts (fun ~anon run example describe ->
            deal_with_describe describe
              [ switch
                  ( List.map [basic env root] ~f:(fun (n, cl) ->
                        case Str.(example =$= str n) [run_or_show run cl] )
                  @ [ default
                        [say "Unknown example: %s" [example]; fail "Stopping"]
                    ] ) ] ) )

  let () =
    let first_sentence =
      "The distribution comes with runnable examples, try \
       `cosc example --help`."
    in
    Manual.(
      add
        ( section "Examples"
        @ extended
            ( ksprintf par "%s Here is the “basic” example:" first_sentence
            @ from (fun ~root env -> code_block (basic env root |> snd)) )
            ~no:(par first_sentence) ))
end

module Base_script = struct
  let description =
    "Script that is a bit like Docker-compose but with GNU-Screen"

  let make () =
    Script.make [] ~description (fun ~root ->
        Gedsl.Dispatcher_script.make
          ~aliases:
            Gedsl.
              [(str "config", str "configuration"); (str "man", str "manual")]
          ~name:root ~description () )
end

let () =
  Manual.(
    add
      (extended
         ( section "Authors"
         @ ksprintf par "[Seb Mondet](https://seb.mondet.org)."
         @ section "License"
         @ par
             "The code generator is covered by the Apache 2.0 \
              [license](http://www.apache.org/licenses/LICENSE-2.0), the \
              scripts are ISC [licensed](https://opensource.org/licenses/ISC)."
         )))

(*md
The `make` function drives the generation of the list of scripts.

 *)

let make ?default_configuration_path ?default_screen_name ~name ~output_path ()
    =
  let env =
    Environment.make ?default_screen_name ?default_configuration_path name
  in
  let scripts =
    [ Base_script.make ()
    ; Configuration_script.make ()
    ; Configuration_display_script.make ~env ()
    ; Configuration_destroy_script.make ~env ()
    ; Add_job_script.make ~env ()
    ; Init_script.make ~env ()
    ; Remove_job_script.make ~env ()
    ; Start_script.make ~env ()
    ; Logs_script.make ~env ()
    ; Attach_script.make ~env ()
    ; Kill_script.make ~env ()
    ; Manual_script.make ~env ()
    ; Version_script.make ~env ()
    ; Example_script.make ~env ()
    ; Status_script.make ~env () ]
  in
  cmdf "mkdir -p %s" output_path ;
  List.iter scripts ~f:(Script.write ~output_path ~root:name) ;
  msg "Done."

(*md Finally the “main” program, uses the venerable `Arg` module to
call `make`. *)
let () =
  let anon = ref [] in
  let anon_fun p = anon := p :: !anon in
  let usage = sprintf "%s [-help] <path>" Sys.argv.(0) in
  let name = ref None in
  let output_path = ref None in
  let config_path = ref None in
  let screen_name = ref None in
  let output_readme = ref false in
  let args =
    Arg.align
      [ ( "--name"
        , Arg.String (fun s -> name := Some s)
        , sprintf "<script-name> Name of the script." )
      ; ( "--configuration-path"
        , Arg.String (fun s -> config_path := Some s)
        , sprintf "<path> Path to the default configuration root." )
      ; ( "--screen-name"
        , Arg.String (fun s -> screen_name := Some s)
        , sprintf "<name> Force the default screen-session name." )
      ; ( "--output-readme"
        , Arg.Set output_readme
        , sprintf " Output the manual to a `README.md`." )
      ; ( "--output-path"
        , Arg.String (fun s -> output_path := Some s)
        , sprintf "<script-name> Where to write the scripts." ) ]
  in
  Arg.parse args anon_fun usage ;
  List.iter !anon ~f:(msg "Ignoring %s") ;
  let die () = exit 2 in
  let need opt = function
    | Some o -> o
    | None ->
        msg "Option `%s` is mandatory" opt ;
        die ()
  in
  let output_path = need "--output-path" !output_path in
  let name = need "--name" !name in
  make ~name ?default_configuration_path:!config_path
    ?default_screen_name:!screen_name ~output_path () ;
  if !output_readme then (
    msg "Outputting manual to %s/README.md" output_path ;
    cmdf "%s/%s-manual --extended > %s/README.md" output_path name output_path )
