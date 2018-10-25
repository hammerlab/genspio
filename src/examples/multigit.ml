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

let cmdf fmt =
  ksprintf
    (fun s ->
      match Sys.command s with
      | 0 -> ()
      | other -> ksprintf failwith "CMD: %S failed with %d" s other )
    fmt

let multi_status () =
  let open Gedsl in
  let open Command_line in
  let version_string = "0.0.0-dev" in
  let module D = Gedsl.Script_with_describe (struct
    let name = "git-multi-status"

    let description = "Show the status of a bunch of Git repositories."
  end) in
  let opts =
    let open Arg in
    flag ["--show-modified"] ~doc:"Show modified files"
    & flag ["--version"] ~doc:"Show version number"
    & D.describe_option_and_usage ()
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
  let display_all ~show_modified list_of_paths =
    Elist.iter list_of_paths ~f:(fun p ->
        seq
          [ out (sprintf "%s\n>> %%-28s" (String.make 80 '-')) [p ()]
          ; list_repos [p ()]
            ||> on_stdin_lines (fun line ->
                    seq
                      [ call [str "cd"; line]
                      ; out
                          "%-30s | U: %-6s | M: %-4s | Ahead: %-4s | Behind: \
                           %-4s"
                          ( repo_name line
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
                            ] ] ) ] )
  in
  parse opts (fun ~anon show_modified version describe ->
      D.deal_with_describe describe
        [ if_seq version
            ~t:[out (sprintf "%s: %s" D.name version_string) []]
            ~e:[display_all ~show_modified anon] ] )

let () =
  let path = Sys.argv.(1) in
  cmdf "mkdir -p %s" (Filename.quote path) ;
  let gms = path // "git-multi-status" in
  msg "Outputting %S" gms ;
  let o = open_out gms in
  Format.(
    fprintf
      (formatter_of_out_channel o)
      "#!/bin/sh\n\n%a\n" Genspio.Compile.To_slow_flow.Script.pp_posix
      (Genspio.Compile.To_slow_flow.compile
         (multi_status () |> Genspio.Transform.Constant_propagation.process))) ;
  close_out o ;
  cmdf "chmod +x %s" (Filename.quote gms)
