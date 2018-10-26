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

module Multi_status = struct
  let version_string = "0.0.0-dev"

  include Gedsl.Script_with_describe (struct
    let name = "git-multi-status"

    let description = "Show the status of a bunch of Git repositories"
  end)

  let paths_git_config_option = "multi-git.paths"

  let long_description () =
    [ sprintf "This is `%s` version %s." name version_string
    ; sprintf "Description: “%s”" description ]

  let extra_help () =
    [ ""
    ; "Use `git multi-status /path/to/repos1 /path/to/repos2` to display"
    ; "a compact report of all the git repositories found in the folders "
    ; "`/path/to/repos1` and `/path/to/repos2`."
    ; ""
    ; "Default paths to explore can be set in Git's configuration:"
    ; ""
    ; "    git config --global --add multi-git.paths /path/to/repos1"
    ; "    git config --global --add multi-git.paths /path/to/repos2"
    ; "" ]

  let script () =
    let open Gedsl in
    let open Command_line in
    let opts =
      let open Arg in
      flag ["--show-modified"] ~doc:"Show the list of modified files."
      & flag ["--no-config"]
          ~doc:
            (sprintf "Do not look at the `%s` git-config option."
               paths_git_config_option)
      & flag ["--version"] ~doc:"Show version information."
      & describe_option_and_usage () ~more_usage:(extra_help ())
    in
    let out f l = printf (ksprintf str "%s\n" f) l in
    let list_repos paths =
      call
        ( [str "find"] @ paths
        @ strs ["-maxdepth"; "2"; "-type"; "d"; "-name"; ".git"] )
      ||> exec ["sed"; "s:/\\.git::"]
      ||> exec ["sort"]
    in
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
    let repo_kind p =
      let remote_greps g o =
        case
          (succeeds_silently (exec ["git"; "remote"; "-v"] ||> grep g))
          [out o []]
      in
      switch
        [ remote_greps "gpg_remote" "GGPG"
        ; remote_greps "github.com" "GHub"
        ; remote_greps "gitlab" "GLab"
        ; remote_greps "bitbucket" "BBkt"
        ; default [out "Git?" []] ]
      |> get_stdout_one_line
    in
    let display_section ~show_modified path =
      seq
        [ out (sprintf "%s\n>> %%-28s" (String.make 80 '-')) [path]
        ; list_repos [path]
          ||> on_stdin_lines (fun line ->
                  seq
                    [ call [str "cd"; line]
                    ; out
                        "%s: %-30s | U: %-6s | M: %-4s | Ahead: %-4s | \
                         Behind: %-4s"
                        ( repo_kind line :: repo_name line
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
    let from_config =
      exec ["git"; "config"; "--get-all"; paths_git_config_option]
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
                      [ from_config
                        ||> on_stdin_lines (fun line ->
                                display_section ~show_modified line ) ] ] ] )
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
  let gms = path // "git-multi-status" in
  msg "Outputting %S" gms ;
  let o = open_out gms in
  Format.(
    fprintf
      (formatter_of_out_channel o)
      "#!/bin/sh\n\n%s\n\n%a\n"
      ( Multi_status.long_description ()
        @ [ "The following is generated by an OCaml program using the Genspio \
             EDSL."
          ; "See <https://smondet.gitlab.io/genspio-doc/>." ]
      |> List.map ~f:(sprintf "# %s")
      |> String.concat ~sep:"\n" )
      Genspio.Compile.To_slow_flow.Script.pp_posix
      (Genspio.Compile.To_slow_flow.compile
         ( Multi_status.script ()
         |> Genspio.Transform.Constant_propagation.process ))) ;
  close_out o ;
  cmdf "chmod +x %s" (Filename.quote gms)
