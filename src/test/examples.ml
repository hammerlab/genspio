
open Nonstd
module String = Sosa.Native_string

let downloader () =
  let open Genspio.EDSL in
  let say strings =
    let sayone ?(prompt = false) s =
      let prompt =
        if prompt then "downloader: " else "" in
      call (string "printf" :: string (prompt ^ "%s") :: s :: []) in
    match strings with
    | [] -> nop
    | s :: more ->
      seq (
        sayone ~prompt:true s :: List.map more ~f:sayone @ [
          sayone (string "\n");
        ]
      )
  in
  let sayf fmt = ksprintf (fun s -> say [string s]) fmt in
  let fail l = seq [say (string "ERROR: " :: l); fail] in
  let failf fmt = ksprintf (fun s -> fail [string s]) fmt in
  let (//) = Filename.concat in
  let module If = struct
    let make ~t ?e c =
      match e with
      | None -> if_then c (seq t)
      | Some f -> if_then_else c (seq t) (seq f) 
  end in
  let silent ~name unit =
    object (self)
      method stdout = "/tmp" // sprintf "output-of-%s-%s" name "out" |> string
      method stderr = "/tmp" // sprintf "output-of-%s-%s" name "err" |> string
      method exec =
        seq [
          (* say [string "Silent "; string name; self#stdout; self#stderr;]; *)
          write_output (seq unit) ~stdout:self#stdout ~stderr:self#stderr;
        ]
      method succeed_or_fail =
        If.(make
              (self#exec|> succeeds)
              ~t:[sayf "%s: Success" name]
              ~e:[
                sayf "Expression %s failed!" name;
                call [string "cat"; self#stderr];
                failf "Fatal failure of %s" name;
              ])
    end in
  let silence ~name unit =
    let s = silent ~name [unit] in
    s#exec in
  let succeed_in_silence_or_fail ~name units =
    let s = silent ~name units in
    s#succeed_or_fail in
  let download ~url ~output =
    let try_help ?(opt = "--help") cmd =
      exec [cmd; opt] |> silence ~name:(cmd ^ opt) |> succeeds in
    let do_call exec args = [
      sayf "Using `%s`." exec;
      succeed_in_silence_or_fail ~name:exec [
        call (string exec :: args);
      ]
    ] in
    switch [
      case (try_help "wget")
        (do_call "wget" [url; string "--output-document"; output]);
      case (try_help "curl")
        (do_call "wget" [url; string "--output-document"; output]);
      default [failf "Can't find a downloading application"];
    ]
  in
  let string_matches_any string regexp_list =
    (* Cf. http://pubs.opengroup.org/onlinepubs/009695399/utilities/grep.html *)
    let options = List.concat_map regexp_list ~f:(fun r -> ["-e"; r]) in
    string >> exec (["grep"; "-q"] @ options) |> succeeds in
  let no_value = sprintf "none_%x" (Random.int 100_000) |> string in
  parse_command_line
    Option_list.(
      string
        ~doc:"The URL to the stuff" 'u'
        ~default:no_value
      & flag 'c' ~doc:"Do everything in the temp-dir"
      & string 'f'
        ~doc:"Override the downloaded file-name"
        ~default:no_value
      & string 't'
        ~doc:"Use <dir> as temp-dir"
        ~default:(Genspio.EDSL.string "/tmp/genspio-downloader-tmpdir")
      & usage "$0 -u URL [-c]"
    )
    begin fun url all_in_tmp filename_ov tmp_dir ->
      let current_name = tmp_file ~tmp_dir "current-name" in
      let set_output_of_download () =
        If.make (filename_ov =$= no_value)
          ~t:begin
            let filename =
              url >> exec ["sed"; "s:.*/\\([^\\?\\/]*\\)\\?.*:\\1:"]
              |> output_as_string
            in
            let output_path =
              string_concat [tmp_dir; string "/"; filename] in
            [current_name#set output_path]
          end
          ~e:begin
            let output_path =
              string_concat [tmp_dir; string "/"; filename_ov] in
            [current_name#set output_path]
          end
      in
      let remove_suffix v suf =
        v >> exec ["sed"; sprintf "s:^\\(.*\\)%s$:\\1:" suf]
        |> output_as_string in
      seq [
        call [string "mkdir"; string "-p"; tmp_dir];
        if_then all_in_tmp
          (seq [sayf "Going to the tmpdir"; call [string "cd"; tmp_dir]]);
        if_then (url =$= no_value)
          (failf "Argument URL is mandatory");
        if_then_else
          (string_matches_any url ["^http://"; "^https://"; "^ftp://"])
          (seq [
              set_output_of_download ();
              download ~url ~output:current_name#get;
              say [string "Downloaded "; current_name#get];
              loop_while
                (string_matches_any current_name#get
                   ["\\.gpg$"; "\\.tgz$"; "\\.tar$"; "\\.gz$"])
                ~body:begin
                  let make_case ~ext ~verb commands =
                    case (string_matches_any
                            current_name#get [sprintf "\\.%s$" ext]) [
                      say [ksprintf string "%s: " verb; current_name#get];
                      succeed_in_silence_or_fail
                        ~name:(sprintf "%s-%s" verb ext) commands;
                      current_name#set
                        (remove_suffix current_name#get (sprintf "\\.%s" ext));
                    ] in
                  seq [
                    say [string "Extract loop: "; current_name#get];
                    switch [
                      make_case ~ext:"gz" ~verb:"Gunzipping" [
                        call [string "gunzip"; string "-f"; current_name#get];
                      ];
                      make_case ~ext:"tar" ~verb:"Untarring" [
                        call [string "tar"; string "xf"; current_name#get];
                      ];
                      make_case ~ext:"tgz" ~verb:"Untar-gzip-ing" [
                        call [string "tar"; string "zxf"; current_name#get];
                      ];
                      make_case ~ext:"gpg" ~verb:"Decyphering" [
                        call [string "gpg";
                              string "--output";
                              (remove_suffix current_name#get "\\.gpg");
                              string "-d"; current_name#get;];
                      ];
                      default [
                        fail [
                          string "File: "; current_name#get;
                          string " didn't match any option???"
                        ];
                      ];
                    ];
                  ]
                end
            ])
          (seq [
              fail [
                string "URL: "; url;
                string " -> not HTTP(s) or FTP: NOT IMPLEMENTED";
              ]
            ])
      ]
    end


let () =
  match Sys.argv |> Array.to_list |> List.tl_exn with
  | "dl" :: path :: [] ->
    let script = Genspio.Language.to_many_lines (downloader ()) in
    let content =
      sprintf
        "#!/bin/sh\n\n# Generated by Genspio Example Tests\n\n%s\n%!" script in
    begin match path with
    | "-" -> printf "\n`````\n%s`````\n%!" content
    | other ->
      let o = open_out other in
      fprintf o "%s%!" content;
      close_out o
    end
  | other ->
    eprintf "Wrong command line: [%s]\n"
      (List.map ~f:(sprintf "%S") other |> String.concat ~sep:"; ");
    eprintf "Usage:\n\
             %s dl <path>\n\
            \   Create the downloader script.\n%!" Sys.argv.(0);
    exit 1
