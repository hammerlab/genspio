
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
  let return n =
    Construct.exec ["bash"; "-c"; sprintf "exit %d" n] in
  List.concat [
    exits 0 (Compile.Exec ["ls"]);
    exits 18 Construct.(
        ~$ (exec ["ls"])
        &&& succeed ~exit_with:18 (seq [
            exec ["ls"];
            exec ["bash"; "-c"; "exit 2"]])
      );
    exits 23 Construct.(
        seq [
          if_then_else (file_exists "/etc/passwd")
            (exit 23)
            (exit 1);
          exit 2;
        ]
      );
    exits 23 Construct.(
        seq [
          if_then_else (file_exists "/etc/passwd" |> not)
            (exit 1)
            (exit 23);
          exit 2;
        ]
      );
    exits 20 Construct.(
        switch ~default:(return 18) [
          file_exists "/djlsjdseij", return 19;
          file_exists "/etc/passwd", return 20;
          file_exists "/djlsjdseij", return 21;
        ]
      );
    exits 0 Construct.(
        let path = "/tmp/bouh" in
        seq [
          if_then (file_exists path)
            begin
              exec ["rm"; "-f"; path]
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
    exits 11 Construct.(
        let stdout = "/tmp/p1_out" in
        let stderr = "/tmp/p1_err" in
        let return_value = "/tmp/p1_ret" in
        let will_be_escaped =
          "newline:\n tab: \t \x42\b" in
        let will_not_be_escaped =
          "spaces, a;c -- ' - '' \\  ''' # ''''  @ ${nope} & ` ~" in
        seq [
          exec ["rm"; "-f"; stdout; stderr; return_value];
          write_output
            ~stdout ~stderr ~return_value
            (seq [
                printf "%s" will_be_escaped;
                printf "%s" will_not_be_escaped;
                exec ["bash"; "-c"; "printf \"err\\t\\n\" 1>&2"];
                return 11;
              ]);
          if_then_else (
            output_as_string (exec ["cat"; stdout])
            =$= string (will_be_escaped ^ will_not_be_escaped)
          )
            (
              if_then_else
                (output_as_string (exec ["cat"; stderr]) <$> string "err")
                (
                  if_then_else
                    (output_as_string (exec ["cat"; return_value]) =$= string "11\n")
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
    exits 0 Construct.(
        if_then_else (
          string "some" =$= 
          (output_as_string (
              (if_then_else (string "bouh\n" =$= string "bouh")
                 (printf "nnnooo")
                 (printf "some"))
            ))
        )
          (return 0)
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
    exits 10 Construct.(
        let tmp = "/tmp/test_loop_while" in
        let cat_potentially_empty =
          if_then_else (exec ["cat"; tmp] |> succeed)
            nop
            (printf "") in
        seq [
          exec ["rm"; "-f"; tmp];
          loop_while
            (cat_potentially_empty |> output_as_string <$> string "nnnn")
            ~body:begin
              exec ["bash"; "-c"; sprintf "printf n >> %s" tmp];
            end;
          return 10;
        ];
      );
    exits 77 Construct.( (* 77 Is the value of ~exit_with in the call
                            to with_trap *)
        let tmp = "/tmp/test_trapping" in
        let cat_tmp = exec ["cat"; tmp] in
        seq [
          exec ["rm"; "-f"; tmp];
          if_then_else
            (* cat <absent-file> |> to_string should abort the script: *)
            (cat_tmp |> output_as_string <$> string "nnnn")
            (return 11)
            (return 12);
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
    end
  ]


let posix_sh_tests = [
  Test.command "ls" [`Exits_with 0];
]



let () =
  let tests =
    posix_sh_tests
    @ tests
  in
  begin match Lwt_main.run (Test.run tests) with
  | `Ok () -> printf "Done.\n%!"
  | `Error (`IO _ as e) ->
    eprintf "IO-ERROR:\n  %s\n%!" (Pvem_lwt_unix.IO.error_to_string e);
    exit 2
  | `Error (`Shell (s, `Exn e)) ->
    eprintf "SHELL-ERROR:\n  %s\n  %s\n%!" s (Printexc.to_string e);
    exit 3
  end
