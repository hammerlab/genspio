(*md This example creates scripts using the `EDSL` API...

A simple way to generate and install the scripts is:

    genspio_multigit=_build/default/src/examples/multigit.exe
    jbuilder build $genspio_multigit
    $genspio_multigit $BINNPATH

*)
open Nonstd
module String = Sosa.Native_string

let ( // ) = Filename.concat

let msg fmt = ksprintf (eprintf "%s\n%!") fmt

(*md We rename the EDSL locally to, e.g., be able to add functions. *)
module Gedsl = Genspio.EDSL

module Git_config = struct
  let paths_option = "multi-git.paths"

  let paths_help () =
    [ "Default paths to explore can be set in Git's configuration:"
    ; ""
    ; "    git config --global --add multi-git.paths /path/to/repos1"
    ; "    git config --global --add multi-git.paths /path/to/repos2"
    ; "" ]

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
  let get_kind () =
    let open Gedsl in
    let remote_greps g o =
      case
        (succeeds_silently (exec ["git"; "remote"; "-v"] ||> exec ["grep"; g]))
        [printf (str o) []]
    in
    switch
      [ remote_greps "gpg_remote" "GGPG"
      ; remote_greps "github.com" "GHub"
      ; remote_greps "gitlab" "GLab"
      ; remote_greps "bitbucket" "BBkt"
      ; default [printf (str "Git?") []] ]
    |> get_stdout_one_line
end

module Multi_status = struct
  let version_string = "0.0.0-dev"

  include Gedsl.Script_with_describe (struct
    let name = "git-multi-status"

    let description = "Show the status of a bunch of Git repositories"
  end)

  let long_description () =
    [ sprintf "This is `%s` version %s." name version_string
    ; sprintf "Description: “%s”" description ]

  let extra_help () =
    [ ""
    ; "Use `git multi-status /path/to/repos1 /path/to/repos2` to display"
    ; "a compact report of all the git repositories found in the folders "
    ; "`/path/to/repos1` and `/path/to/repos2`."
    ; "" ]
    @ Git_config.paths_help ()

  let script () =
    let open Gedsl in
    let open Command_line in
    let opts =
      let open Arg in
      flag ["--show-modified"] ~doc:"Show the list of modified files."
      & flag ["--no-config"]
          ~doc:
            (sprintf "Do not look at the `%s` git-config option."
               Git_config.paths_option)
      & flag ["--version"] ~doc:"Show version information."
      & describe_option_and_usage () ~more_usage:(extra_help ())
    in
    let out f l = printf (ksprintf str "%s\n" f) l in
    let untracked_files =
      exec ["git"; "status"; "-s"; "-uall"] ||> exec ["egrep"; "^\\?\\?"]
    in
    let modified_files = exec ["git"; "status"; "-s"; "-uno"] in
    let branches_vv = exec ["git"; "branch"; "-v"; "-v"] in
    let grep s = exec ["grep"; s] in
    let ahead_branches = branches_vv ||> grep "ahead" in
    let behind_branches = branches_vv ||> grep "behind" in
    let get_count u = get_stdout_one_line (u ||> exec ["wc"; "-l"]) in
    let repo_name p = call [str "basename"; p] |> get_stdout_one_line in
    let display_section ~show_modified path =
      seq
        [ out (sprintf "%s\n>> %%-28s" (String.make 80 '-')) [path]
        ; Repository.list_all [path]
          ||> on_stdin_lines (fun line ->
                  seq
                    [ call [str "cd"; line]
                    ; out
                        "%s: %-30s | U: %-6s | M: %-4s | Ahead: %-4s | \
                         Behind: %-4s"
                        ( Repository.get_kind () :: repo_name line
                        :: List.map ~f:get_count
                             [ untracked_files
                             ; modified_files
                             ; ahead_branches
                             ; behind_branches ] )
                    ; if_seq
                        ( show_modified
                        &&& Str.(get_count modified_files <$> str "0") )
                        ~t:
                          [ out "  |- Modified:" []
                          ; modified_files ||> exec ["sed"; "s:^ M:  |    -:"]
                          ] ] ) ]
    in
    parse opts (fun ~anon show_modified no_config version describe ->
        deal_with_describe describe
          [ if_seq version
              ~t:[out (sprintf "%s: %s" name version_string) []]
              ~e:
                [ Elist.iter anon ~f:(fun p ->
                      display_section ~show_modified (p ()) )
                ; if_seq no_config ~t:[]
                    ~e:
                      [ Git_config.all_paths ()
                        ||> on_stdin_lines (fun line ->
                                display_section ~show_modified line ) ] ] ] )
end

module Activity_report = struct
  let version_string = Multi_status.version_string

  include Gedsl.Script_with_describe (struct
    let name = "git-activity-report"

    let description =
      "Make a report of developments in a bunch of Git repositories"
  end)

  let long_description () =
    [ sprintf "This is `%s` version %s." name version_string
    ; sprintf "Description: “%s”" description ]

  let extra_help () =
    [ ""
    ; "Use `git activity-report --since 2018-10-23 /path/to/repos1 \
       /path/to/repos2` to display"
    ; "a detailed “recent happenings” report of all the git repositories \
       found"
    ; "in the folders `/path/to/repos1` and `/path/to/repos2`."
    ; "" ]
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
          (sprintf "Do not look at the `%s` git-config option."
             Git_config.paths_option)
      & string ["--since"] ~doc:"Date to get the logs/information from"
          ~default:(str default_since)
      & string ["--section-base"]
          ~doc:
            (sprintf
               "The base markdown section ('##', '###', etc. default: %s)"
               default_section_base)
          ~default:(str default_section_base)
      & flag ["--version"] ~doc:"Show version information."
      & describe_option_and_usage () ~more_usage:(extra_help ())
    in
    let out f l = printf (ksprintf str "%s\n" f) l in
    let get_count u = get_stdout_one_line (u ||> exec ["wc"; "-l"]) in
    let repo_name p = call [str "basename"; p] |> get_stdout_one_line in
    let display_section ~section_base ~since path =
      seq
        [ out "\n%s In `%-28s`" [section_base; path]
        ; Repository.list_all [path]
          ||> on_stdin_lines (fun line ->
                  let since_opt = Str.concat_list [str "--since="; since] in
                  let git_log l =
                    call (strs ["git"; "--no-pager"; "log"] @ l)
                  in
                  let commit_number l =
                    git_log ([since_opt; str "--oneline"] @ l) |> get_count
                  in
                  let list_report branch =
                    git_log
                      ( [since_opt]
                      @ strs ["--reverse"; "--pretty=tformat:- %s.%n%b"]
                      @ [branch] )
                    ||> exec ["grep"; "-Ev"; "^$"]
                  in
                  seq
                    [ call [str "cd"; line] (* | egrep -v '^$' *)
                    ; if_seq
                        Str.(commit_number [] <$> str "0")
                        ~t:
                          [ out "\\n%s# %s: %-30s\\n\\n```"
                              [ section_base
                              ; Repository.get_kind ()
                              ; repo_name line ]
                          ; git_log
                              ( [since_opt]
                              @ strs
                                  [ "--graph"
                                  ; "--decorate"
                                  ; "--pretty=tformat:%D"
                                  ; "--all"
                                  ; "--simplify-by-decoration" ] )
                          ; out "```\n\n%s## On `master`:" [section_base]
                          ; list_report (str "master")
                          ; exec ["git"; "branch"; "--no-merged"; "master"]
                            ||> exec ["sed"; "s/*//"]
                            ||> on_stdin_lines (fun branch ->
                                    let treeish =
                                      Str.concat_list [str "master.."; branch]
                                    in
                                    if_seq
                                      Str.(commit_number [treeish] <$> str "0")
                                      ~t:
                                        [ out "\n%s## On `%s`\n"
                                            [section_base; branch]
                                        ; list_report treeish ] ) ] ] ) ]
    in
    parse opts (fun ~anon no_config since section_base version describe ->
        deal_with_describe describe
          [ if_seq version
              ~t:[out (sprintf "%s: %s" name version_string) []]
              ~e:
                [ if_then
                    Str.(since =$= str default_since)
                    (fail "ERROR: Option --since is for now mandatory!")
                ; Elist.iter anon ~f:(fun p ->
                      display_section ~section_base ~since (p ()) )
                ; if_seq no_config ~t:[]
                    ~e:
                      [ Git_config.all_paths ()
                        ||> on_stdin_lines (fun line ->
                                display_section ~since ~section_base line ) ]
                ] ] )
end

(*md
The command-line interface is, for now, just about writing the scripts
to a directory.

 *)
let cmdf fmt =
  ksprintf
    (fun s ->
      match Sys.command s with
      | 0 -> ()
      | other -> ksprintf failwith "CMD: %S failed with %d" s other )
    fmt

let () =
  let path = Sys.argv.(1) in
  cmdf "mkdir -p %s" (Filename.quote path) ;
  let output filename script long_description =
    let gms = path // filename in
    msg "Outputting %S" gms ;
    let o = open_out gms in
    Format.(
      fprintf
        (formatter_of_out_channel o)
        "#!/bin/sh\n\n%s\n\n%a\n"
        ( long_description ()
          @ [ "The following is generated by an OCaml program using the \
               Genspio EDSL."
            ; "See <https://smondet.gitlab.io/genspio-doc/>." ]
        |> List.map ~f:(sprintf "# %s")
        |> String.concat ~sep:"\n" )
        Genspio.Compile.To_slow_flow.Script.pp_posix
        (Genspio.Compile.To_slow_flow.compile
           (script () |> Genspio.Transform.Constant_propagation.process))) ;
    close_out o ;
    cmdf "chmod +x %s" (Filename.quote gms)
  in
  Multi_status.(output name script long_description) ;
  Activity_report.(output name script long_description) ;
  printf "Done.\n%!"
