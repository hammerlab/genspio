(*md This example creates scripts using the `EDSL` API...

  A simple way to generate and install the scripts is:

      genspio_multigit=_build/default/src/examples/multigit.exe
      dune build $genspio_multigit
      $genspio_multigit $BINNPATH
*)
open! Base
module Filename = Caml.Filename

let ( // ) = Filename.concat
let msg fmt = Fmt.kstr (Fmt.epr "%s\n%!") fmt

(*md We rename the EDSL locally to, e.g., be able to add functions. *)
module Gedsl = struct
  include Genspio.EDSL

  let get_count u =
    get_stdout_one_line (u ||> exec ["wc"; "-l"] ||> exec ["tr"; "-d"; " \\n"])
end

module Git_config = struct
  let paths_option = "multi-git.paths"

  let paths_help () =
    [ "Default paths to explore can be set in Git's configuration:"; ""
    ; "    git config --global --add multi-git.paths /path/to/repos1"
    ; "    git config --global --add multi-git.paths /path/to/repos2"; "" ]

  let all_paths () =
    let open Gedsl in
    exec ["git"; "config"; "--get-all"; paths_option]
end

module Repository = struct
  let list_all paths =
    let open Gedsl in
    call
      ( [str "find"] @ paths
      @ strs ["-maxdepth"; "2"; "-type"; "d"; "-name"; ".git"] )
    ||> exec ["sed"; "s:/\\.git::"]
    ||> exec ["sort"]

  (** Get the 4-letter “kind” of the current repository. *)
  let get_kind ?remote () =
    let open Gedsl in
    let remote_greps g o =
      case
        (succeeds_silently
           ( ( match remote with
             | None -> exec ["git"; "remote"; "-v"]
             | Some s -> call (strs ["git"; "remote"; "get-url"] @ [s]) )
           ||> exec ["grep"; g] ) )
        [printf (str o) []] in
    switch
      [ remote_greps "gpg_remote" "GGPG"; remote_greps "github.com" "GHub"
      ; remote_greps "gitlab" "GLab"; remote_greps "bitbucket" "BBkt"
      ; default [printf (str "Unkn") []] ]
    |> get_stdout_one_line
end

let version_string =
  Fmt.str "%s (Genspio: %s)"
    (try Caml.Sys.getenv "multigit_version" with _ -> "0")
    Genspio.Meta.version

module Multi_status = struct
  include Gedsl.Script_with_describe (struct
    let name = "git-multi-status"
    let description = "Show the status of a bunch of Git repositories"
  end)

  let long_description () =
    [ Fmt.str "This is `%s` version %s." name version_string
    ; Fmt.str "Description: “%s”" description ]

  let extra_help () =
    [ ""; "Use `git multi-status /path/to/repos1 /path/to/repos2` to display"
    ; "a compact report of all the git repositories found in the folders "
    ; "`/path/to/repos1` and `/path/to/repos2`."; "" ]
    @ Git_config.paths_help ()

  module Git = struct
    open Gedsl

    let branches_vv = exec ["git"; "branch"; "-v"; "-v"]

    let list_ahead_branches =
      branches_vv ||> exec ["grep"; "-E"; {re|ahead *[0-9]+\]|re}]

    let list_local_branches ?not_merged_in () =
      exec
        ( ["git"; "branch"; "-vv"]
        @ Option.value_map not_merged_in ~default:[] ~f:(fun br ->
              ["--no-merged"; br] ) )
      ||> exec ["grep"; "-v"; "\\["]

    (*md

      If you have `git`, you have a shell line wrapper:

          $ git log --pretty=format:"%w(42,4,2)$(fortune)%n" -1
            DISCLAIMER: Use of this advanced
          computing technology does not imply an
          endorsement of Western industrial
          civilization.

      Pretty cool, right:
    *)
    let wrap_string_hack ~columns ~first_line_indent ~other_lines_indent
        ~final_newline str_value =
      call
        [ str "git"; str "log"
        ; Str.concat_list
            [ Fmt.kstr str "--pretty=format:%%w(%d,%d,%d)" columns
                first_line_indent other_lines_indent; str_value
            ; (if final_newline then str "%n" else str "") ]; str "-1" ]
  end

  module Columns = struct
    let all () =
      let open Gedsl in
      let open Git in
      let grep s = exec ["grep"; s] in
      let counts =
        [ ( "Untrk"
          , exec ["git"; "status"; "-s"; "-uall"] ||> exec ["egrep"; "^\\?\\?"]
          , "Untracked files" )
        ; ("Modf", exec ["git"; "status"; "-s"; "-uno"], "Modified files")
        ; ("Ahd", list_ahead_branches, "Branches ahead of their remote")
        ; ( "Bhd"
          , branches_vv ||> grep "behind"
          , "Branches behind on their remote" )
        ; ( "Umr"
          , exec ["git"; "branch"; "--no-merged"; "HEAD"]
          , "Branches not merged in `HEAD`" )
        ; ("Lcl", list_local_branches (), "Not-remote-tracking local branches")
        ; ( "L/H"
          , list_local_branches ~not_merged_in:"HEAD" ()
          , "Not-remote-tracking local branches not merged in `HEAD`" )
          (* git branch -vv --no-merged HEAD | grep -v '\[' *) ] in
      List.map ~f:(fun (t, e, d) -> (t, get_count e, d)) counts

    let title (c, _, _) = c
    let code (_, c, _) = c
    let description (_, _, c) = c

    let top_row () =
      Fmt.str "| %s |" (String.concat ~sep:" | " (List.map (all ()) ~f:title))

    let row () =
      let open Gedsl in
      printf
        (Fmt.kstr str "| %s |\\n"
           ( List.map ~f:title (all ())
           |> List.map ~f:(fun t -> Fmt.str "%%-%ds" (String.length t))
           |> String.concat ~sep:" | " ) )
        (List.map (all ()) ~f:code)

    let help () =
      let longest =
        List.map (all ()) ~f:(fun c -> title c |> String.length)
        |> List.fold ~init:0 ~f:max in
      [Fmt.str "The table shows %d columns:" (List.length (all ())); ""]
      @ List.map (all ()) ~f:(fun c ->
            Fmt.str "* `%s`%s-> %s." (title c)
              (String.make (longest - (title c |> String.length)) ' ')
              (description c) )
  end

  let msgf fmt l =
    let open Gedsl in
    eprintf (Fmt.kstr str "%s: %s\\n" name fmt) l

  let script () =
    let open Gedsl in
    let out f l = printf (Fmt.kstr str "%s\n" f) l in
    let modified_files = exec ["git"; "status"; "-s"; "-uno"] in
    let repo_name p = call [str "basename"; p] |> get_stdout_one_line in
    let to_list_of_names u =
      u
      ||> exec ["sed"; {sed|s/^\*\? *\([^ ]\+\).*$/\1/|sed}]
      ||> exec ["tr"; "\\n"; ","]
      ||> exec ["sed"; {|s/,$/./|}]
      ||> exec ["sed"; {|s/,/, /g|}] in
    let total_width = 86 in
    let display_section ~show_modified ~show_ahead ~show_local ~show_all path =
      let topdir = tmp_file "topdir" in
      let if_show show count t =
        if_seq (show_all ||| show &&& Str.(get_count count <$> str "0")) ~t
      in
      seq
        [ printf (str "#=== ") []
        ; printf
            (Fmt.kstr str "%%-%ds\n" (total_width - 5))
            [Str.concat_list [path; str ":"]]
          ||> exec ["sed"; "s/ /=/g"]
        ; out (Fmt.str "%s %s" (String.make 40 ' ') (Columns.top_row ())) []
        ; topdir#set (getenv (str "PWD"))
        ; Repository.list_all [path]
          ||> on_stdin_lines (fun line ->
                  seq
                    [ call [str "cd"; topdir#get]; call [str "cd"; line]
                    ; printf (str "%-41s")
                        [ Str.concat_list
                            [Repository.get_kind (); str "::"; repo_name line]
                        ]
                      ||> exec ["sed"; "s/ /./g"]
                      ||> exec ["tr"; "-d"; "\\n"]; Columns.row ()
                    ; if_show show_modified modified_files
                        [ out "  |- Modified:" []
                        ; modified_files ||> exec ["sed"; "s:^ M:  |    -:"] ]
                    ; if_show show_ahead Git.list_ahead_branches
                        [ printf (str "  |- Ahead: ") []
                        ; Git.wrap_string_hack ~columns:(total_width - 10)
                            ~first_line_indent:0 ~other_lines_indent:12
                            ~final_newline:true
                          @@ get_stdout
                               (Git.list_ahead_branches |> to_list_of_names) ]
                    ; if_show show_local
                        (Git.list_local_branches ())
                        [ printf (str "  |- Local: ") []
                        ; Git.wrap_string_hack ~columns:(total_width - 10)
                            ~first_line_indent:0 ~other_lines_indent:12
                            ~final_newline:true
                          @@ get_stdout
                               (Git.list_local_branches () |> to_list_of_names)
                        ] ] ) ] in
    let open Command_line in
    let opts =
      let open Arg in
      flag ["--show-modified"] ~doc:"Show the list of modified files."
      & flag ["--show-ahead"] ~doc:"Show the list of ahead branches."
      & flag ["--show-local"] ~doc:"Show the list of local branches."
      & flag ["--show-all-extras"]
          ~doc:"Like all the `--show-*` options together."
      & flag ["--no-config"]
          ~doc:
            (Fmt.str "Do not look at the `%s` git-config option."
               Git_config.paths_option )
      & flag ["--version"] ~doc:"Show version information."
      & describe_option_and_usage ()
          ~more_usage:(extra_help () @ [""] @ Columns.help ()) in
    parse opts
      (fun
        ~anon
        show_modified
        show_ahead
        show_local
        show_all
        no_config
        version
        describe
      ->
        let do_section =
          display_section ~show_modified ~show_ahead ~show_local ~show_all in
        deal_with_describe describe
          [ if_seq version
              ~t:[out (Fmt.str "%s: %s" name version_string) []]
              ~e:
                [ setenv ~var:(str "PAGER") (str "cat")
                ; Elist.iter anon ~f:(fun p -> do_section (p ()))
                ; if_seq no_config ~t:[]
                    ~e:
                      [ Git_config.all_paths ()
                        ||> on_stdin_lines (fun line -> do_section line) ] ] ] )
end

module Activity_report = struct
  include Gedsl.Script_with_describe (struct
    let name = "git-activity-report"

    let description =
      "Make a report of developments in a bunch of Git repositories"
  end)

  let long_description () =
    [ Fmt.str "This is `%s` version %s." name version_string
    ; Fmt.str "Description: “%s”" description ]

  let extra_help () =
    [ ""
    ; "Use `git activity-report --since 2018-10-31 /path/to/repos1 \
       /path/to/repos2` to display"
    ; "a detailed “recent happenings” report of all the git repositories found"
    ; "in the folders `/path/to/repos1` and `/path/to/repos2`."; "" ]
    @ Git_config.paths_help ()

  let script () =
    let open Gedsl in
    let open Command_line in
    let default_since = "Nooooooooooooooo-since" in
    let default_section_base = "###" in
    let opts =
      let open Arg in
      flag ["--no-config"]
        ~doc:
          (Fmt.str "Do not look at the `%s` git-config option."
             Git_config.paths_option )
      & string ["--since"]
          ~doc:
            "Date to get the logs/information since (default: “last sunday”)."
          ~default:(str default_since)
      & string ["--section-base"]
          ~doc:
            (Fmt.str "The base markdown section ('##', '###', etc. default: %s)"
               default_section_base )
          ~default:(str default_section_base)
      & flag ["--fetch"]
          ~doc:(Fmt.str "Run `git fetch --all` before showing a repository.")
      & flag ["--version"] ~doc:"Show version information."
      & describe_option_and_usage () ~more_usage:(extra_help ()) in
    let out f l = printf (Fmt.kstr str "%s\n" f) l in
    let repo_name p = call [str "basename"; p] |> get_stdout_one_line in
    let display_section ~section_base ~since ~fetch_before path =
      let topdir = tmp_file "topdir" in
      seq
        [ out "\n%s In `%s`" [section_base; path]
        ; topdir#set (getenv (str "PWD"))
        ; Repository.list_all [path]
          ||> on_stdin_lines (fun line ->
                  let since_opt = Str.concat_list [str "--since="; since] in
                  let git_log l = call (strs ["git"; "--no-pager"; "log"] @ l) in
                  let commit_number l =
                    git_log ([since_opt; str "--oneline"] @ l) |> get_count
                  in
                  let list_report branch =
                    git_log
                      ( [since_opt]
                      @ strs ["--reverse"; "--pretty=tformat:- %s.  %n  %b"]
                      @ [branch] )
                    ||> exec ["grep"; "-Ev"; "^ *$"] in
                  let fence () = out (String.make 80 '`') [] in
                  seq
                    [ call [str "cd"; topdir#get]
                    ; call [str "cd"; line] (* | egrep -v '^$' *)
                    ; if_seq fetch_before
                        ~t:
                          [ eprintf (str "Fetching...\n") []
                          ; with_redirections
                              (exec ["git"; "fetch"; "--all"])
                              [to_fd (int 1) (int 2)] ]
                    ; if_seq
                        Str.(commit_number [] <$> str "0")
                        ~t:
                          [ out "\\n%s# %s: %s\\n"
                              [ section_base; Repository.get_kind ()
                              ; repo_name line ]; out "\\nWorking tree:\\n" []
                          ; fence ()
                          ; exec
                              [ "git"; "status"; "--short"; "--branch"
                              ; "--show-stash" ]; fence ()
                          ; out "\\nGraph:\\n" []; fence ()
                          ; git_log
                              ( [since_opt]
                              @ strs
                                  [ "--graph"; "--decorate"
                                  ; "--pretty=tformat:%w(72,0,2)%d %s"; "--all"
                                  ] ); fence ()
                          ; git_log
                              ( [since_opt]
                              @ strs
                                  [ "--simplify-by-decoration"
                                  ; "--pretty=tformat:%D" ] )
                            ||> on_stdin_lines (fun line ->
                                    if_seq
                                      Str.(line <$> str "")
                                      ~t:
                                        [ out "\\n%s## On `%s`\\n"
                                            [section_base; line]
                                        ; list_report
                                            (get_stdout_one_line
                                               ( line
                                               >> exec ["sed"; "s/[ ,].*$//"] ) )
                                        ] ) ] ] ) ] in
    parse opts
      (fun ~anon no_config since section_base fetch_before version describe ->
        let tmp_since = tmp_file "gar-since" in
        deal_with_describe describe
          [ if_seq version
              ~t:[out (Fmt.str "%s: %s" name version_string) []]
              ~e:
                [ tmp_since#set since
                ; if_seq
                    Str.(since =$= str default_since)
                    ~t:
                      (let date_format = "+%Y-%m-%d" in
                       let today = exec ["date"; date_format] in
                       let today_nth = exec ["date"; "+%u"] in
                       let last_sunday =
                         call
                           [ str "date"; str "-d"
                           ; Str.concat_list
                               [ get_stdout_one_line today; str " -"
                               ; get_stdout_one_line today_nth; str " days" ]
                           ; str date_format ] in
                       [ eprintf
                           (str "Last Sunday was %s.\\n")
                           [get_stdout_one_line last_sunday]
                       ; tmp_since#set (get_stdout_one_line last_sunday) ] )
                ; Elist.iter anon ~f:(fun p ->
                      display_section ~section_base ~since:tmp_since#get
                        ~fetch_before (p ()) )
                ; if_seq no_config ~t:[]
                    ~e:
                      [ Git_config.all_paths ()
                        ||> on_stdin_lines (fun line ->
                                display_section ~since:tmp_since#get
                                  ~section_base ~fetch_before line ) ] ] ] )
end

module Fetch_all = struct
  include Gedsl.Script_with_describe (struct
    let name = "git-fetch-all"
    let description = "Call git fetch a bunch of Git repositories"
  end)

  let long_description () =
    [ Fmt.str "This is `%s` version %s." name version_string
    ; Fmt.str "Description: “%s”" description ]

  let extra_help () =
    [ ""; "Use `git fetch-all /path/to/repos1 /path/to/repos2` to run"
    ; "`git fetch --all` in the folders `/path/to/repos1` and \
       `/path/to/repos2`."; "" ]
    @ Git_config.paths_help ()

  let script () =
    let open Gedsl in
    let out f l = printf (Fmt.kstr str "%s\n" f) l in
    let repo_name p = call [str "basename"; p] |> get_stdout_one_line in
    let fetch_remote errors_file repo line =
      let log =
        Str.concat_list
          [str "/tmp/multi-fetch-"; repo; str "-"; line; str ".log"] in
      seq
        [ if_seq
            (succeeds
               (with_redirections
                  (call [str "git"; str "fetch"; line])
                  [to_file (int 2) log; to_fd (int 1) (int 2)] ) )
            ~t:
              [ printf (str "[%s:%s OK]")
                  [Repository.get_kind ~remote:line (); line] ]
            ~e:
              [ printf (str "[%s:%s ERROR]")
                  [Repository.get_kind ~remote:line (); line]
              ; errors_file#append
                  (get_stdout
                     (seq
                        [ printf
                            (str "* Repository `%s`, remote `%s`:\\n")
                            [repo; line]
                        ; call [str "tail"; str "-n"; str "3"; log]
                          ||> exec ["sed"; "s/^/    * > /"]
                        ; printf (str "    * See also `%s`.\\n") [log] ] ) ) ]
        ] in
    let fetch_in ~errors_file path =
      let topdir = tmp_file "topdir" in
      let ls_remotes = exec ["git"; "remote"] in
      seq
        [ topdir#set (getenv (str "PWD"))
        ; Repository.list_all [path]
          ||> on_stdin_lines (fun line ->
                  let repo = repo_name (getenv (str "PWD")) in
                  seq
                    [ call [str "cd"; topdir#get]; call [str "cd"; line]
                    ; printf (str ">> Repository %s: ") [repo]
                    ; if_seq
                        Str.(get_count ls_remotes =$= str "0")
                        ~t:[printf (str "No remotes.") []]
                        ~e:
                          [ ls_remotes
                            ||> on_stdin_lines (fetch_remote errors_file repo)
                          ]; printf (str "\\n") [] ] ) ] in
    let open Command_line in
    let opts =
      let open Arg in
      flag ["--no-config"]
        ~doc:
          (Fmt.str "Do not look at the `%s` git-config option."
             Git_config.paths_option )
      & flag ["--version"] ~doc:"Show version information."
      & describe_option_and_usage () ~more_usage:(extra_help ()) in
    parse opts (fun ~anon no_config version describe ->
        let errors = tmp_file "fetch-all-errors" in
        let go = fetch_in ~errors_file:errors in
        deal_with_describe describe
          [ if_seq version
              ~t:[out (Fmt.str "%s: %s" name version_string) []]
              ~e:
                [ errors#set (str ""); Elist.iter anon ~f:(fun p -> go (p ()))
                ; if_seq no_config ~t:[]
                    ~e:
                      [ Git_config.all_paths ()
                        ||> on_stdin_lines (fun line -> go line) ]
                ; if_seq
                    Str.(errors#get <$> str "")
                    ~t:[out "\\n## Errors:" []; call [str "cat"; errors#path]]
                ] ] )
end

let cmdf fmt =
  Fmt.kstr
    (fun s ->
      match Caml.Sys.command s with
      | 0 -> ()
      | other -> Fmt.kstr failwith "CMD: %S failed with %d" s other )
    fmt

module Meta_repository = struct
  let wrap ?(newline = "\n") ?(indent = 0) ?(columns = 72) s =
    let buf = Buffer.create 42 in
    let indentation = String.make indent ' ' in
    let rec assemble col = function
      | [] -> ()
      | one :: more ->
          let potential = col + String.length one + 1 in
          if potential > columns then (
            Buffer.add_string buf (newline ^ indentation ^ one) ;
            assemble (String.length one) more )
          else (
            Buffer.add_string buf ((if col = 0 then "" else " ") ^ one) ;
            assemble potential more ) in
    let words =
      String.split s ~on:' ' |> List.map ~f:String.strip
      |> List.filter ~f:String.(( <> ) "") in
    assemble 0 words ; Buffer.contents buf

  let cmd_to_string_list cmd =
    let i = Unix.open_process_in cmd in
    let rec loop acc =
      try loop (Caml.input_line i :: acc)
      with _ -> Caml.close_in i ; List.rev acc in
    loop []

  let readme_md ~path:_ ~output =
    let o = Caml.open_out output in
    let open Caml.Format in
    let fmt = formatter_of_out_channel o in
    let out f = fprintf fmt f in
    let sec c s = out "%s\n%s\n\n" s (String.make (String.length s) c) in
    let title = sec '=' in
    let section = sec '-' in
    let par f = Fmt.kstr (fun s -> out "%s\n\n" (wrap s)) f in
    let cmd_lines s = cmd_to_string_list ("PATH=%s:$PATH " ^ s) in
    let cmd_output s = cmd_lines s |> String.concat ~sep:"\n" in
    let see_output_of_help fmt =
      Fmt.kstr
        (fun s ->
          let lines =
            cmd_output (s ^ " | grep -E -v '^usage:' | sed 's/->/→/'") in
          out "%s\n" lines ; out "\n\n" ; out "See also `%s`.\n\n" s )
        fmt in
    title "Git: Multi-Repository" ;
    par
      "This project provides a couple of scripts which handle multiple Git \
       repositories at once. One can provide a list of directories to scan for \
       repositories (non-recursively) on the command line or through Git's \
       configuration mechanism." ;
    par "The scripts provided as of now are:" ;
    let describe s =
      let lines = cmd_output (s ^ " --describe") in
      out "- `%s`: %s.\n" s lines in
    describe "git-multi-status" ;
    describe "git-activity-report" ;
    describe "git-fetch-all" ;
    par "" ;
    par
      "It may be interesting for the user to also alias them in \
       `~/.gitconfig`, for instance:" ;
    out
      {code|
    [alias]
        mst = multi-status --show-all-extras
        arfd = activity-report --since
|code} ;
    par "" ;
    par "See below for detailed usage information ⮷." ;
    section "Usage: Git-multi-status" ;
    see_output_of_help "git-multi-status --help" ;
    section "Usage: Git-activity-report" ;
    see_output_of_help "git-activity-report --help" ;
    par "**Current Limitations:**" ;
    par
      "- Often, there are redundancies between branches that the script does \
       not detect." ;
    section "Usage: Git-fetch-all" ;
    see_output_of_help "git-fetch-all --help" ;
    par "" ;
    section "Authors / Making-of" ;
    par
      "This repository is generated by an OCaml program which itself was \
       written by [Seb Mondet](https://seb.mondet.org), it uses the \
       [Genspio](https://smondet.gitlab.io/genspio-doc/) EDSL library, and \
       serves as one of its examples of usage, see also its \
       [implementation](https://github.com/hammerlab/genspio/tree/master/src/examples/multigit.ml). " ;
    par
      "Similarly, you may check out the <https://github.com/smondet/cosc> \
       repository, which is also a bunch of shell scripts maintained by an \
       OCaml program." ;
    section "Example Session / Demo" ;
    let git_repos_top = "/tmp/git-repos-example" in
    let git_repos_hammerlab = git_repos_top // "hammerlab" in
    let git_repos_smondet = git_repos_top // "smondet" in
    let git_repos_bitbucket = git_repos_top // "bitbucket" in
    let all_git_repo_tops =
      [git_repos_hammerlab; git_repos_smondet; git_repos_bitbucket] in
    let hammerlabs = ["ketrew"; "biokepi"; "genspio"; "coclobas"] in
    let smondets = ["genspio-doc"; "vecosek"] in
    let example_cmd ?(wrap_display = true) ?(with_fence = `Yes)
        ?(ignore_output = false) f =
      Fmt.kstr
        (fun s ->
          let lines = cmd_lines s in
          let w =
            if wrap_display then wrap ~newline:" \\\n" ~indent:6 ~columns:70
            else fun e -> e in
          out "    $ %s\n" (w s) ;
          if Poly.(lines <> []) || ignore_output then
            let fence = String.make 72 '`' in
            match with_fence with
            | `Yes ->
                out "\n%sok-output\n" fence ;
                List.iter lines ~f:(out "    %s\n") ;
                out "%s\n\n" fence
            | `Quote ->
                out "\n" ;
                List.iter lines ~f:(out "> %s\n") )
        f in
    par
      "Let's see a sequence of examples to demo the scripts. First, we prepare \
       a set of *“test”* repositories in `%s`:"
      git_repos_top ;
    (* Silent command: *) cmdf "rm -fr %s" git_repos_top ;
    List.iter all_git_repo_tops ~f:(example_cmd "mkdir -p %s") ;
    let clone repos uri_prefix path =
      List.iter repos ~f:(fun r ->
          example_cmd "git clone %s%s.git %s/%s" uri_prefix r path r ) in
    clone hammerlabs "https://github.com/hammerlab/" git_repos_hammerlab ;
    clone smondets "https://gitlab.com/smondet/" git_repos_smondet ;
    clone ["nonstd"] "https://bitbucket.org/smondet/" git_repos_bitbucket ;
    par "" ;
    par
      "For now, we haven't changed anything to the repositories so the \
       “multi-status” is full of zeros (we use the `--no-config` option to get \
       consistent output w.r.t. users' configuration):" ;
    let on_all f cmd =
      Fmt.kstr f "%s --no-config %s" cmd
        (String.concat ~sep:" " all_git_repo_tops) in
    on_all (example_cmd "%s") "git multi-status" ;
    par "" ;
    par
      "The activity-report is, for now, more interesting, and it outputs \
       directly Markdown:" ;
    on_all
      (example_cmd ~with_fence:`Quote "%s")
      "git activity-report --section-base '####' --since 2018-10-31" ;
    par "" ;
    par
      "The script `git-fetch-all` is the simplest it just provides a nice \
       display even when working with ≥ 30 repositories, and with **errors**, \
       so we are going to add a couple of wrong remotes to spice things up." ;
    example_cmd
      "git -C %s/ketrew remote add wrong-http \
       https://example.com/dadams/h2g2.git"
      git_repos_hammerlab ;
    example_cmd
      "git -C %s/vecosek remote add wrong-ssh \
       git@@gitlab.com/i-don-t-have-access/to-this.git"
      git_repos_smondet ;
    par "" ;
    par "And *now,* we call `git-fetch-all`:" ;
    on_all (example_cmd "%s") "git fetch-all" ;
    par "" ;
    par "Let's do some modifications:" ;
    example_cmd "echo 'This is Great!' >> %s/biokepi/README.md"
      git_repos_hammerlab ;
    example_cmd "echo 'More lawyery stuff' >> %s/biokepi/LICENSE"
      git_repos_hammerlab ;
    example_cmd "echo 'This is tracked' >> %s/coclobas/README.md"
      git_repos_hammerlab ;
    example_cmd "echo 'This is *not* tracked' >> %s/coclobas/NOT-TRACKED.md"
      git_repos_hammerlab ;
    example_cmd "echo 'This is Great' >> %s/ketrew/README.md"
      git_repos_hammerlab ;
    example_cmd "git -C %s/ketrew/ checkout -b new-branch-that-tracks -t master"
      git_repos_hammerlab ;
    example_cmd "git -C %s/ketrew/ commit -a -m 'Add greatness to the README'"
      git_repos_hammerlab ;
    example_cmd "git -C %s/ketrew/ checkout -b new-branch-more-local"
      git_repos_hammerlab ;
    par "" ;
    par
      "Now in the multi-status we can see the modified files, the untracked \
       counts, and one branch is “ahead” (since we used `-t master` while \
       creating, it has a remote to define it):" ;
    on_all (example_cmd "%s") "git multi-status --show-all-extras" ;
    par "" ;
    par "Let's concentrate the activity-report on `%s` and on the past 3 days:"
      git_repos_hammerlab ;
    example_cmd
      "git activity-report --no-config --since $(date -d '-3 days' \
       +%%Y-%%m-%%d) %s"
      git_repos_hammerlab ;
    par "" ;
    par "We can see the new commit in the new branch appears in the report ⮵." ;
    par "And that's all for today ☺ !" ;
    section "License" ;
    par
      "The code generator is covered by the Apache 2.0 \
       [license](http://www.apache.org/licenses/LICENSE-2.0), the scripts are \
       ISC [licensed](https://opensource.org/licenses/ISC)." ;
    ()
end

(*md
  The command-line interface is, for now, just about writing the scripts
  to a directory.

  To write the scripts in a `./bin` directory and add a `README.md` just
  set the environment variable: `repomode=true`.
*)

let () =
  let path = Caml.Sys.argv.(1) in
  cmdf "mkdir -p %s" (Filename.quote path) ;
  let repomode =
    try String.(Caml.Sys.getenv "repomode" = "true") with _ -> false in
  let output filename script long_description =
    let gms = if repomode then path // "bin" // filename else path // filename in
    msg "Outputting %S" gms ;
    cmdf "mkdir -p %s" Filename.(quote (dirname gms)) ;
    let o = Caml.open_out gms in
    Caml.Format.(
      fprintf
        (formatter_of_out_channel o)
        "#!/bin/sh\n\n%s\n\n%a\n"
        ( long_description ()
          @ [ "The following is generated by an OCaml program using the \
               Genspio EDSL."; "See <https://smondet.gitlab.io/genspio-doc/>."
            ]
        |> List.map ~f:(Fmt.str "# %s")
        |> String.concat ~sep:"\n" )
        Genspio.Compile.To_slow_flow.Script.pp_posix
        (Genspio.Compile.To_slow_flow.compile
           (script () |> Genspio.Transform.Constant_propagation.process) )) ;
    Caml.close_out o ;
    cmdf "chmod +x %s" (Filename.quote gms) in
  Multi_status.(output name script long_description) ;
  Activity_report.(output name script long_description) ;
  Fetch_all.(output name script long_description) ;
  if repomode then
    Meta_repository.readme_md ~path:(path // "bin") ~output:(path // "README.md") ;
  Fmt.pr "Done.\n%!"
