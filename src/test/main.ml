
open Nonstd
module String = Sosa.Native_string
module Test = Tests.Test_lib

module Compile = Genspio.Language
module Construct = Genspio.EDSL

let exits ?name ?args n c = [
  Test.command ?name:(Option.map name (sprintf "%s; one-liner"))
    ?args (Compile.to_one_liner c) ~verifies:[`Exits_with n];
  Test.command ?name:(Option.map name (sprintf "%s; multi-liner"))
    ?args (Compile.to_many_lines c) ~verifies:[`Exits_with n];
]

let tests =
  let exit n = Construct.exec ["exit"; Int.to_string n] in
  let return n = Construct.exec ["sh"; "-c"; sprintf "exit %d" n] in
  let printf fmt = ksprintf (fun s -> Construct.exec ["printf"; "%s"; s]) fmt in
  List.concat [
    exits 0 Construct.(
        succeeds (exec ["ls"])
        &&& returns ~value:18 (seq [
            exec ["ls"];
            exec ["sh"; "-c"; "exit 18"]])
      );
    exits 23 Construct.(
        seq [
          if_then_else (file_exists (string "/etc/passwd"))
            (exit 23)
            (exit 1);
          exit 2;
        ]
      );
    exits 23 Construct.(
        seq [
          if_then_else (file_exists (string "/etc/passwd") |> not)
            (exit 1)
            (exit 23);
          exit 2;
        ]
      );
    exits 20 Construct.(
        make_switch ~default:(return 18) [
          file_exists @@ string "/djlsjdseij", return 19;
          file_exists @@ string "/etc/passwd", return 20;
          file_exists @@ string "/djlsjdseij", return 21;
        ]
      );
    exits 0 Construct.(
        let path = string "/tmp/bouh" in
        seq [
          if_then (file_exists path)
            begin
              call [string "rm"; string "-f"; path]
            end;
          write_stdout ~path (seq [
              printf "bouh";
              exec ["ls"; "-la"];
            ]);
          if_then (file_exists path |> not)
            begin
              exit 1
            end;
        ]);
    exits 42 Construct.(
        (* Many variation on `write_output` *)
        let stdout = string "/tmp/p1_out" in
        let stderr = string "/tmp/p1_err" in
        let return_value = string "/tmp/p1_ret" in
        seq [
          write_output
            ~stdout ~stderr ~return_value
            (seq [
                printf "%s" "hello";
                exec ["sh"; "-c"; "printf \"olleh\" 1>&2"];
                return 12;
              ]);
          write_output
            ~stderr ~return_value
            (seq [
                printf "%s" "hello";
                exec ["sh"; "-c"; "printf \"olleh\" 1>&2"];
                return 12;
              ]);
          write_output
            ~return_value
            (seq [
                printf "%s" "hello";
                exec ["sh"; "-c"; "printf \"olleh\" 1>&2"];
                return 12;
              ]);
          write_output ~stdout
            (seq [
                printf "%s" "hello";
                exec ["sh"; "-c"; "printf \"olleh\" 1>&2"];
                return 12;
              ]);
          write_output
            (seq [
                printf "%s" "hello";
                exec ["sh"; "-c"; "printf \"olleh\" 1>&2"];
                return 12;
              ]);
          return 42;
        ]
      );
    exits 11 Construct.(
        let stdout = string "/tmp/p1_out" in
        let stderr = string "/tmp/p1_err" in
        let return_value_path = string "/tmp/p1_ret" in
        let return_value_value = 31 in
        let will_be_escaped =
          "newline:\n tab: \t \x42\b" in
        let will_not_be_escaped =
          "spaces, a;c -- ' - '' \\  ''' # ''''  @ ${nope} & ` ~" in
        seq [
          call [string "rm"; string "-f"; stdout; stderr; return_value_path];
          write_output
            ~stdout ~stderr ~return_value:return_value_path
            (seq [
                printf "%s" will_be_escaped;
                printf "%s" will_not_be_escaped;
                exec ["sh"; "-c"; "printf \"err\\t\\n\" 1>&2"];
                return return_value_value;
              ]);
          if_then_else (
            output_as_string (call [string "cat"; stdout])
            =$= string (will_be_escaped ^ will_not_be_escaped)
          )
            (
              if_then_else
                (output_as_string (call [string "cat"; stderr]) <$> string "err")
                (
                  if_then_else
                    (output_as_string (call [string "cat"; return_value_path])
                     =$= ksprintf string "%d\n" return_value_value)
                    (return 11)
                    (return 22)
                )
                (return 23)
            )
            (return 24);
        ]);
    exits 12 Construct.( (* This looks dumb but finding an encoding of
                            strings that makes this work was pretty hard
                            a CRAZIX shell *)
        if_then_else (
          string "some" =$= string "some\n"
        )
          (return 11)
          (return 12)
      );
    exits 11 Construct.(
        if_then_else (
          string "some" =$= 
          (output_as_string (
              (if_then_else (string "bouh\n" =$= string "bouh")
                 (printf "nnnooo")
                 (printf "some"))
            ))
        )
          (return 11)
          (return 12)
      );
    exits 10 Construct.(
        if_then_else
          (
            (string "b\x00ouh\nbah\n" >> exec ["cat"] |> output_as_string)
            =$=
            string "b\x00ouh\nbah\n"
          )
          (return 10)
          (return 11)
      );
    exits 13 Construct.(
        let tmp = "/tmp/test_loop_while" in
        let cat_potentially_empty =
          if_then_else (exec ["cat"; tmp] |> succeeds)
            nop
            (printf "") in
        seq [
          exec ["rm"; "-f"; tmp];
          exec ["rm"; "-f"; tmp];
          loop_while
            (cat_potentially_empty |> output_as_string <$> string "nnnn")
            ~body:begin
              exec ["sh"; "-c"; sprintf "printf n >> %s" tmp];
            end;
          return 13;
        ];
      );
    begin
      let minus_f = "one \nwith \\ spaces and \ttabs -dashes -- " in
      let make ret minus_g single =
        exits ret
          ~args:["-f"; minus_f; single; "-g"; minus_g ]
          Construct.(
            parse_command_line
              Option_list.(
                string ~doc:"String one" 'f'
                & string ~doc:"String two" 'g'
                & flag ~doc:"Bool one" 'v'
                & usage "Usage string\nwith bunch of lines to\nexplain stuff")
              begin fun one two bone ->
                if_then_else
                  ((one =$= two) ||| bone)
                  (return 11)
                  (if_then_else
                     (one =$= string minus_f) (* Should be always true *)
                     (return 12) (* i.e. we're testing that weird characters have good escaping *)
                     (return 44))
              end
          ) in
      List.concat [
        make 11 minus_f "";
        make 12 "not-one" "";
        make 11 "not-one" "-v";
        make 12 minus_f "--"; (* the `--` should prevent the `-g one` from being parsed *)
        make 77 "not-one" "-x"; (* option does not exist, script uses `die` *)
        make 77 "not-one" "--v";
        make 77 "not-one" "-v j";
        make 11 "not \\ di $bouh one" "-v";
        make 12 "not \\ di $bouh one" " -- -v";
        make 12 "one \nwith spaces and \ttabs -dashes -- " "";
        make 12 "one \nwith  spaces and \ttabs -dashes -- " "";
        make 12 "one with \\ spaces and \ttabs -dashes -- " "";
        make 0 "not-one" "--help";
        make 0 "not-one" "-help";
        make 0 "not-one" "-h";
      ]
    end;
    exits 77 ~name:"die in a sequence" Construct.(
        seq [
          printf "Going to die";
          fail;
          return 42;
        ]
      );
    exits 77 ~name:"cannot capture death itself" Construct.(
        seq [
          write_output
            ~return_value:(string "/tmp/dieretval")
            (seq [
                printf "Going to die\n";
                fail;
                return 42;
              ]);
          return 23;
        ]
      );
    exits 77 ~name:"cannot poison death either" Construct.(
        seq [
          string "dj ijdedej j42 ijde - '' "
          >> seq [
            printf "Going to die\n";
            fail;
            return 42;
          ];
          return 23;
        ]
      );
    begin
      let gives = 11 in
      let does_not_give = 12 in
      let t cmd yn value =
        let name =
          sprintf "%s %s %d" cmd
            (if yn = gives then "returns" else "does not return") value in
        exits ~name yn Construct.(
            if_then_else
              (exec ["sh"; "-c"; cmd] |> returns ~value)
              (return gives)
              (return does_not_give)
          ) in
      List.concat [
        t "ls" gives 0;
        t "ls /deijdsljidisjeidje" does_not_give 0;
        t "ls /deijdsljidisjeidje" does_not_give 42;
        t "exit 2" gives 2;
        exits 21 ~name:"More complex return check" Construct.(
            if_then_else
              (seq [
                  printf "I aaam so complex!\n";
                  if_then_else (string "djsleidjs" =$=
                                output_as_string (printf "diejliejjj"))
                    (return 41)
                    (return 42);
                ]
               |> returns ~value:42)
              (return 21)
              (return 22)
          );
      ]
    end;
    exits 11 Construct.(
        let tmp = "/tmp/test_error_in_output_as_string" in
        let cat_tmp = exec ["cat"; tmp] in
        seq [
          exec ["rm"; "-f"; tmp];
          if_then_else
            (* cat <absent-file> |> to_string does not abort the script: *)
            (cat_tmp |> output_as_string =$= string "")
            (return 11)
            (return 12);
        ];
      );
    exits 11 Construct.(
        let tmp = "/tmp/test_error_in_output_as_string" in
        let cat_tmp = exec ["cat"; tmp] in
        seq [
          exec ["rm"; "-f"; tmp];
          if_then_else
            (seq [printf "aaa"; cat_tmp] |> output_as_string =$= string "aaa")
            (return 11)
            (return 12);
        ];
      );
    exits 77 Construct.(
        let tmp = "/tmp/test_error_in_output_as_string" in
        let cat_tmp = exec ["cat"; tmp] in
        let succeed_or_die ut =
          if_then_else (succeeds ut)
            nop
            (seq [
                printf "Failure !";
                fail;
              ]) in
        seq [
          exec ["rm"; "-f"; tmp];
          if_then_else
            (seq [printf "aaa"; cat_tmp] |> succeed_or_die
             |> output_as_string =$= string "aaa")
            (return 11)
            (return 12);
        ];
      );
    (* Use of the `call` constructor: *)
    exits 28 Construct.(
        if_then_else
          (call [string "cat"; output_as_string (printf "/does not exist")]
           |> succeeds)
          (return 11)
          (return 28);
      );
    exits 17 Construct.(
        if_then_else (bool true) (return 17) (return 16)
      );
    exits 16 Construct.(
        if_then_else (bool true &&& bool false) (return 17) (return 16)
      );
    exits 16 Construct.(
        if_then_else
          (bool true &&& not (bool false)) (return 16) (return 17)
      );
  ]


let posix_sh_tests = [
  Test.command "ls" [`Exits_with 0];
]



let () =
  let tests =
    posix_sh_tests
    @ tests
  in
  let important_shells =
    try Sys.getenv "important_shells" |> String.split ~on:(`Character ',')
    with _ -> ["bash"; "dash"] in
  let additional_shells =
    begin try
      Sys.getenv "add_shells"  |> String.split ~on:(`String "++")
      |> List.map ~f:(fun spec ->
          match
            String.split spec ~on:(`Character ',')
            |> List.map ~f:String.strip
          with
          | name :: "escape" :: cmd_arg :: cmd_format :: [] ->
            Test.make_shell name
              ~command:(fun c args ->
                  let fun_name = "askjdeidjiedjjjdjekjdeijjjidejdejlksi" in
                  let sep =
                    String.concat ~sep:" " (
                      [fun_name; "() {"; c ; " ; } ; "; fun_name ]
                      @ List.map ~f:Filename.quote args
                    )
                    |> Filename.quote
                  in
                  String.split cmd_format ~on:(`String cmd_arg)
                  |> String.concat ~sep
                )
              ~get_version:"", "Command-line"
          | other ->
            failwith "Nope"
        )
    with
      _ -> []
    end
  in
  begin match
    Lwt_main.run (Test.run ~important_shells ~additional_shells tests)
  with
  | `Ok (`Succeeded) -> printf "Success! \\o/.\n%!"; exit 0
  | `Ok (`Failed msg) -> printf "Test failed: %s.\n%!" msg; exit 5
  | `Error (`IO _ as e) ->
    eprintf "IO-ERROR:\n  %s\n%!" (Pvem_lwt_unix.IO.error_to_string e);
    exit 2
  | `Error (`Shell (s, `Exn e)) ->
    eprintf "SHELL-ERROR:\n  %s\n  %s\n%!" s (Printexc.to_string e);
    exit 3
  end
