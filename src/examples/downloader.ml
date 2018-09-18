open Nonstd
module String = Sosa.Native_string

let downloader () =
  let open Genspio.EDSL in
  let say strings =
    let sayone ?(prompt = false) s =
      let prompt = if prompt then "downloader: " else "" in
      call [string "printf"; string (prompt ^ "%s"); s]
    in
    match strings with
    | [] -> nop
    | s :: more ->
        seq
          ( (sayone ~prompt:true s :: List.map more ~f:sayone)
          @ [sayone (string "\n")] )
  in
  let sayf fmt = ksprintf (fun s -> say [string s]) fmt in
  let fail l = seq [say (string "ERROR: " :: l); fail "fail-list"] in
  let failf fmt = ksprintf (fun s -> fail [string s]) fmt in
  let ( // ) = Filename.concat in
  let silent ~name unit =
    object (self)
      method stdout = "/tmp" // sprintf "output-of-%s-%s" name "out" |> string

      method stderr = "/tmp" // sprintf "output-of-%s-%s" name "err" |> string

      method exec =
        seq
          [ (* say [string "Silent "; string name; self#stdout; self#stderr;]; *)
            write_output (seq unit) ~stdout:self#stdout ~stderr:self#stderr ]

      method succeed_or_fail =
        if_seq (self#exec |> succeeds) ~t:[sayf "%s: Success" name]
          ~e:
            [ sayf "Expression %s failed!" name
            ; call [string "cat"; self#stderr]
            ; failf "Fatal failure of %s" name ]
    end
  in
  let silence ~name unit =
    let s = silent ~name [unit] in
    s#exec
  in
  let succeed_in_silence_or_fail ~name units =
    let s = silent ~name units in
    s#succeed_or_fail
  in
  let download ~url ~output =
    let try_help ?(opt = "--help") cmd =
      exec [cmd; opt] |> silence ~name:(cmd ^ opt) |> succeeds
    in
    let do_call exec args =
      [ sayf "Using `%s`." exec
      ; succeed_in_silence_or_fail ~name:exec [call (string exec :: args)] ]
    in
    switch
      [ case (try_help "wget")
          (do_call "wget" [url; string "--output-document"; output])
      ; case (try_help "curl")
          (do_call "curl" [string "-L"; string "-o"; output; url])
      ; default [failf "Can't find a downloading application"] ]
  in
  let string_matches_any string regexp_list =
    (* Cf. http://pubs.opengroup.org/onlinepubs/009695399/utilities/grep.html *)
    let options = List.concat_map regexp_list ~f:(fun r -> ["-e"; r]) in
    string >> exec (["grep"; "-q"] @ options) |> succeeds
  in
  let no_newline_sed ~input expr =
    let with_potential_newline =
      Str.concat_list [input; string "\n"] >> exec ["sed"; expr] |> get_stdout
    in
    with_potential_newline >> exec ["tr"; "-d"; "\\n"] |> get_stdout
  in
  let module Unwrapper = struct
    type cmd = unit t

    type t = {extension: string; verb: string; commands: file -> cmd list}

    let make ~ext ~verb commands = {extension= ext; verb; commands}

    let remove_suffix v suf =
      no_newline_sed ~input:v (sprintf "s:^\\(.*\\)%s$:\\1:" suf)

    let to_switch name_variable t_list =
      let make_case t =
        case
          (string_matches_any name_variable#get [sprintf "\\.%s$" t.extension])
          [ say [ksprintf string "%s: " t.verb; name_variable#get]
          ; succeed_in_silence_or_fail
              ~name:(sprintf "%s-%s" t.verb t.extension)
              (t.commands name_variable)
          ; name_variable#set
              (remove_suffix name_variable#get (sprintf "\\.%s" t.extension))
          ]
      in
      seq
        [ say [string "Extract loop: "; name_variable#get]
        ; switch (List.map t_list ~f:make_case) ]

    let to_loop name_variable t_list =
      loop_while
        (string_matches_any name_variable#get
           (List.map t_list (fun t -> sprintf "\\.%s$" t.extension)))
        ~body:(to_switch name_variable t_list)

    let all =
      [ make ~ext:"gz" ~verb:"Gunzipping" (fun current_name ->
            [call [string "gunzip"; string "-f"; current_name#get]] )
      ; make ~ext:"bz2" ~verb:"Bunzip2-ing" (fun current_name ->
            [call [string "bunzip2"; string "-f"; current_name#get]] )
      ; make ~ext:"zip" ~verb:"Unzipping" (fun current_name ->
            [call [string "unzip"; current_name#get]] )
      ; make ~ext:"tar" ~verb:"Untarring" (fun current_name ->
            [call [string "tar"; string "xf"; current_name#get]] )
      ; make ~ext:"tgz" ~verb:"Untar-gzip-ing" (fun name ->
            [call [string "tar"; string "zxf"; name#get]] )
      ; make ~ext:"tbz2" ~verb:"Untar-bzip2-ing" (fun name ->
            [call [string "tar"; string "xfj"; name#get]] )
      ; make ~ext:"gpg" ~verb:"Decyphering" (fun name ->
            [ call
                [ string "gpg"
                ; string "--output"
                ; remove_suffix name#get "\\.gpg"
                ; string "-d"
                ; name#get ] ] ) ]
  end in
  let no_value = sprintf "none_%x" (Random.int 100_000) |> string in
  let cli_spec =
    let open Command_line.Arg in
    string ~doc:"The URL to the stuff" ["-u"; "--url"] ~default:no_value
    & flag ["-c"; "--all-in-tmp"] ~doc:"Do everything in the temp-dir"
    & string ["-f"; "--local-filename"]
        ~doc:"Override the downloaded file-name" ~default:no_value
    & string ["-t"; "--tmp-dir"] ~doc:"Use <dir> as temp-dir"
        ~default:(str "/tmp/genspio-downloader-tmpdir")
    & usage
        "Download archives and decrypt/unarchive them.\n\
         ./downloader -u URL [-c] [-f <file>] [-t <tmpdir>]"
  in
  Command_line.parse cli_spec (fun ~anon url all_in_tmp filename_ov tmp_dir ->
      let current_name = tmp_file ~tmp_dir "current-name" in
      let set_output_of_download () =
        if_seq
          Str.(filename_ov =$= no_value)
          ~t:
            (let filename =
               no_newline_sed ~input:url "s/.*\\/\\([^?\\/]*\\).*/\\1/"
             in
             let output_path =
               Str.concat_list [tmp_dir; string "/"; filename]
             in
             [current_name#set output_path])
          ~e:
            (let output_path =
               Str.concat_list [tmp_dir; string "/"; filename_ov]
             in
             [current_name#set output_path])
      in
      seq
        [ call [string "mkdir"; string "-p"; tmp_dir]
        ; if_then all_in_tmp
            (seq [sayf "Going to the tmpdir"; call [string "cd"; tmp_dir]])
        ; if_then Str.(url =$= no_value) (failf "Argument URL is mandatory")
        ; if_then_else
            (string_matches_any url ["^http://"; "^https://"; "^ftp://"])
            (seq
               [ set_output_of_download ()
               ; download ~url ~output:current_name#get
               ; say [string "Downloaded "; current_name#get]
               ; Unwrapper.to_loop current_name Unwrapper.all ])
            (seq
               [ fail
                   [ string "URL: "
                   ; url
                   ; string " -> not HTTP(s) or FTP: NOT IMPLEMENTED" ] ]) ] )

let () =
  match Sys.argv |> Array.to_list |> List.tl_exn with
  | ["make"; path] -> (
      let script = Genspio.Compile.to_many_lines (downloader ()) in
      let content =
        sprintf "#!/bin/sh\n\n# Generated by Genspio Example Tests\n\n%s\n%!"
          script
      in
      match path with
      | "-" -> printf "\n`````\n%s`````\n%!" content
      | other ->
          let o = open_out other in
          fprintf o "%s%!" content ; close_out o )
  | other ->
      eprintf "Wrong command line: [%s]\n"
        (List.map ~f:(sprintf "%S") other |> String.concat ~sep:"; ") ;
      eprintf "Usage:\n%s make <path>\n\   Create the downloader script.\n%!"
        Sys.argv.(0) ;
      exit 1
