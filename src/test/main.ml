
open Nonstd
module String = Sosa.Native_string
module Test = Tests.Test_lib

module Compile = Genspio.Language
module Construct = Genspio.EDSL


let check_size ?(name = "") ~ret str =
  (*
     Got the magic number on Linux/Ubuntu 16.04.
     See `xargs --show-limits`.
  *)
  if String.length str > 131071
  then (
    eprintf "WARNING: Command %S (ret %d) is too big for `sh -c <>`\n%!" name ret;
    None
  ) else
    Some str


let exits ?no_trap ?name ?args n c =
  let one_liner =
    Compile.to_one_liner ?no_trap c |> check_size ?name ~ret:n in
  let script =
    Compile.to_many_lines ?no_trap c |> check_size ?name ~ret:n in
  let tests =
    [
      Option.map one_liner ~f:(fun cmd ->
          Test.command ?name:(Option.map name (sprintf "%s; one-liner"))
            ?args cmd ~verifies:[`Exits_with n];
        );
      Option.map script ~f:(fun cmd ->
          Test.command ?name:(Option.map name (sprintf "%s; multi-liner"))
            ?args cmd ~verifies:[`Exits_with n];
        );
    ]
    |> List.filter_opt
  in
  if tests = []
  then
    ksprintf failwith
      "Test %S (ret %d) got no testing at all because of size limit"
      (Option.value ~default:"No-name" name) n
  else
    tests

let tests =
  let exit n = Construct.exec ["exit"; Int.to_string n] in
  let return n = Construct.exec ["sh"; "-c"; sprintf "exit %d" n] in
  let printf fmt = ksprintf (fun s -> Construct.exec ["printf"; "%s"; s]) fmt in
  let comment fmt = ksprintf (fun s -> Construct.exec [":"; s]) fmt in
  let assert_or_fail name cond =
    let open Genspio.EDSL in
    if_then_else cond nop (seq [printf "Fail: %s\n" name; fail]) in
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
    exits 11 Construct.(
        if_then_else (int 42 |> Integer.to_string =$= string "42")
          (return 11)
          (return 13)
      );
    exits 12 Construct.(
        if_then_else (int 42 |> Integer.to_string
                      |> Integer.of_string |> Integer.to_string
                                              =$= string "42")
          (return 12)
          (return 13)
      );
    exits 12 Construct.(
        if_then_else (int (-42) |> Integer.to_string
                      |> Integer.of_string |> Integer.to_string
                                              =$= string "-42")
          (return 12)
          (return 13)
      );
    exits 77 Construct.( (* It's not a string representing an integer: *)
        if_then_else (string "87732b" |> Integer.of_string |> Integer.to_string
                                                              =$= string "8877732")
          (return 12)
          (return 13)
      );
    exits 12 Construct.(
        if_then_else (Integer.(int 22 + int 20) |> Integer.to_string
                                                   =$= string "42")
          (return 12)
          (return 13)
      );
    exits 12 Construct.(
        if_then_else (Integer.(int 2 * (int 22 - int 20)) |> Integer.to_string
                                                             =$= string "4")
          (return 12)
          (return 13)
      );
    exits 12 Construct.(
        let trybin res b = b |> Integer.to_string =$= string (Int.to_string res) in
        if_then_else (
          trybin 1 Integer.(int 2 * (int 22 - int 20) / int 4)
          &&&
          trybin 0 Integer.(int 2 * (int 22 - int 20) / int 5)
          &&&
          trybin 8 Integer.(int 42 / int 5)
          &&&
          trybin 2 Integer.(int 42 mod int 5)
          &&&
          trybin 0 Integer.(int 3000 mod int 3)
        )
          (return 12)
          (return 13)
      );
    exits 17 Construct.(
        if_then_else (Integer.(int 2 = int 2)) (return 17) (return 13)
      );
    exits 13 Construct.(
        if_then_else (Integer.(int 2 > int 2)) (return 17) (return 13)
      );
    exits 13 Construct.(
        if_then_else (Integer.(int 2 < int 2)) (return 17) (return 13)
      );
    exits 23 Construct.(
        if_then_else (
          Integer.(int 2 <= int 2)
          &&& Integer.(int 2 >= int 2)
          &&& Integer.(int 3 > int 2)
          &&& Integer.(int 3 >= int 2)
          &&& Integer.(int 3 <> int 2)
          &&& not Integer.(int 3 = int 2)
          &&& Integer.(int (-1) < int 2)
        )
          (return 23) (return 13)
      );
    exits 25 Construct.(
        if_then_else (
          (getenv (string "HOME") =$= string (Sys.getenv "HOME"))
          &&&
          (getenv (string "PATH") =$= string (Sys.getenv "PATH"))
          &&&
          (getenv (string_concat [string "PA"; string "TH"])
           =$= string (Sys.getenv "PATH"))
        )
          (return 25) (return 13)
      );
    exits 29 Construct.(
        if_then_else (
          (getenv (string "HOMEEEEEEEE")) =$= string ""
        )
          (return 29) (return 27)
      );
    exits 27 Construct.(
        if_then_else (  (* Explicit test of a corner case: *)
          (getenv (string "HOME\nME")) =$= string (Sys.getenv "HOME")
        )
          (return 12) (return 27)
      );
    exits 27 Construct.(
        if_then_else (
          (getenv (string "HOME\000ME")) =$= string (Sys.getenv "HOME")
        )
          (return 12) (return 27)
      );
    exits 0 Construct.(
        let var = string "VVVVVVV" in
        let assert_or_return ret cond =
          if_then_else cond nop (seq [printf "Fail: %d" ret; fail]) in
        seq [
          assert_or_return 27 (getenv var =$= string "");
          setenv ~var (string "Bouh");
          assert_or_return 28 (getenv var =$= string "Bouh");
          (* We also “record the undefined behavior” *)
          setenv ~var (string "Bouhh\nbah");
          assert_or_return 29 (getenv var =$= string "Bouhh");
          setenv ~var (string "Bouhhh\nbah\n");
          assert_or_return 30 (getenv var =$= string "Bouhhh");
          setenv ~var (string "Bouhoo\000bah\n");
          assert_or_return 12 (getenv var =$= string "Bouhoobah");
          (* We check that the environment is affected in a brutal way:
             we mess up the $PATH:
             /bin/sh: 1: ls: not found
             We cannot even use `return` anymore after that: *)
          setenv ~var:(string "PATH") (string "/nope");
          assert_or_return 42 (
            exec ["/bin/sh"; "-c"; "ls"] |> succeeds |> not
          );
        ]
      );
    exits 32 Construct.(
        seq [
          with_throw
            ~catch:(seq [printf "Caught !"; exit 32])
            (fun throw ->
               seq [
                 printf "Throwing";
                 throw;
                 return 42;
               ]);
          return 28;
        ]
      );
    exits 28 Construct.(
        seq [
          comment "trowing once stuff";
          with_throw
            ~catch:(seq [printf "Caught !"; exit 32])
            (fun throw ->
               seq [
                 printf "Not Throwing";
               ]);
          return 28;
        ]
      );
    begin
      let open Genspio.EDSL in
      let tmp = tmp_file "agglomeration" in
      let make ~jump =
        seq [
          comment "Multi-trowing stuff: %b" jump;
          setenv (string "TMPDIR") (string "/var/tmp/");
          tmp#set (string "1");
          printf "adding 1 !\n";
          with_throw
            ~catch:(seq [
                printf "One Caught !\n";
                printf "adding 5 !\n";
                tmp#append (string ",5");
              ])
            (fun throw_one ->
               seq [
                 tmp#append (string ",2");
                 printf "adding 2 !\n";
                 with_throw
                   ~catch:(seq [
                       printf "Two Caught !\n";
                       printf "adding 4 !\n";
                       tmp#append (string ",4");
                       throw_one;
                     ])
                   (fun throw_two ->
                      seq [
                        printf "adding 3 !\n";
                        tmp#append (string ",3");
                        (if jump then throw_one else throw_two);
                      ]);
               ]);
          call [string "printf"; string "Agglo: %s\\n"; tmp#get;];
          if_then_else (tmp#get
                        =$=
                        string (if jump then "1,2,3,5" else "1,2,3,4,5"))
            (return 28)
            (return 29);
        ]
      in
      List.concat [
        exits ~name:"multijump" 28 (make ~jump:true);
        exits ~name:"multijump" 28 (make ~jump:false);
      ]
    end;
    begin
      let with_failwith_basic_test =
        Genspio.EDSL.(
          seq [
            comment "Test with failwith";
            with_failwith (fun die ->
                seq [
                  comment "Test with failwith: just before dying";
                  printf "Dying now\n";
                  die
                    ~message:(string "HElllooo I'm dying!!\n") ~return:(int 23)
                ]
              );
          ]
        ) in
      List.concat [
        exits ~name:"with_failwith" 23 with_failwith_basic_test;
        exits ~name:"with_failwith-and-more" 37 Genspio.EDSL.(
            let tmpextra = tmp_file "extratmp" in
            let tmpdir = Filename.temp_file "genspio" "test" in
            seq [
              comment "Test with failwith and check that files go away";
              exec ["rm"; "-f"; tmpdir];
              exec ["mkdir"; "-p"; tmpdir];
              setenv (string "TMPDIR") (string tmpdir);
              tmpextra#set (string "");
              assert_or_fail "tmpfile-in-tmpdir"
                begin
                  tmpextra#path >>
                  call [string "grep"; string tmpdir]
                  |> returns ~value:0
                end;
              write_output
                ~return_value:tmpextra#path
                begin
                  seq [
                    exec [
                      "sh"; "-c"; (* Soooo meta *)
                      Genspio.Language.to_one_liner with_failwith_basic_test;
                    ]
                  ]
                end;
              assert_or_fail "with_failwith:ret23"
                (tmpextra#get |> Integer.of_string |> Integer.eq (int 23));
              tmpextra#delete;
              call [
                string "echo";
                call [string "find"; string tmpdir]
                |> output_as_string
              ];
              assert_or_fail "with_failwith:no-files-in-tmpdir"
                begin
                  call [string "find"; string tmpdir]
                  |> output_as_string
                     =$= ksprintf string "%s\n" tmpdir
                end;
              return 37;
            ]
          );
      ];
    end;
    exits ~name:"tmp#delete" 23 Genspio.EDSL.(
        let tmp = tmp_file "test" in
        let s1 = string "hello\000you" in
        seq [
          tmp#set (string "");
          assert_or_fail "tmp#get 1" (tmp#get =$= string "");
          tmp#set s1;
          assert_or_fail "tmp#get 2" (tmp#get =$= s1);
          tmp#delete;
          assert_or_fail "tmp#get 3" (tmp#get =$= string "");
          return 23;
        ]
      );
    exits 2 ~no_trap:true ~name:"no-trap" Genspio.EDSL.(return 2);
    exits 2 ~no_trap:true ~name:"no-trap-but-failwith" Genspio.EDSL.(
        seq [
          with_failwith (fun die ->
              seq [
                printf "Dying now\n";
                die
                  ~message:(string "HElllooo I'm dying!!\n") ~return:(int 2)
              ]
            );
        ]
      );
  ]

let posix_sh_tests = [
  Test.command "ls" [`Exits_with 0];
]



let () =
  let test_filters =
    try Sys.getenv "filter_tests" |> String.split ~on:(`Character ',')
    with _ -> [] in
  let important_shells =
    try Sys.getenv "important_shells" |> String.split ~on:(`Character ',')
    with _ -> ["bash"; "dash"] in
  let all_tests = posix_sh_tests @ tests in
  let tests =
    if test_filters = [] then all_tests else
      all_tests |> List.filter ~f:(function
        | `Command (Some n,_,_,_) when
            List.exists test_filters ~f:(fun prefix ->
                String.is_prefix n ~prefix) -> true
        | `Command (Some n,_,_,_) ->
          eprintf "NAME: %S filtered out\n%!" n; false
        | _ -> false)
  in
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
