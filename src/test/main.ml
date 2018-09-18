open Nonstd
module String = Sosa.Native_string
open Tests.Test_lib
module Compile = Genspio.Language
module Construct = Genspio.EDSL_v0

let exits = Test.exits

let shexit n = Construct.exec ["exit"; Int.to_string n]

let return n = Construct.exec ["sh"; "-c"; sprintf "exit %d" n]

let tprintf fmt = ksprintf (fun s -> Construct.exec ["printf"; "%s"; s]) fmt

let comment fmt = ksprintf (fun s -> Construct.exec [":"; s]) fmt

let assert_or_fail name cond =
  let open Genspio.EDSL in
  if_then_else cond (tprintf "OK: %s\n" name)
    (seq [tprintf "Fail: %s\n" name; fail name])

let tests = ref []

let add_tests t = tests := t :: !tests

let () =
  add_tests
  @@ exits 23
       Construct.(
         seq
           [ if_then_else
               (file_exists (string "/etc/passwd"))
               (shexit 23) (shexit 1)
           ; shexit 2 ]) ;
  ()

let () =
  add_tests
  @@ exits 23
       Construct.(
         seq
           [ if_then_else
               (file_exists (string "/etc/passwd") |> not)
               (shexit 1) (shexit 23)
           ; shexit 2 ]) ;
  ()

let () =
  add_tests
  @@ exits 20
       Construct.(
         make_switch ~default:(return 18)
           [ (file_exists @@ string "/djlsjdseij", return 19)
           ; (file_exists @@ string "/etc/passwd", return 20)
           ; (file_exists @@ string "/djlsjdseij", return 21) ]) ;
  ()

let () =
  add_tests
  @@ exits 4 ~name:"Write-stdout"
       (let open Construct in
       let path = string "/tmp/bouh" in
       seq
         [ if_then (file_exists path) (call [string "rm"; string "-f"; path])
         ; write_stdout ~path (seq [tprintf "bouh"; exec ["ls"; "-la"]])
         ; if_then (file_exists path |> not) (return 11)
         ; return 4 ]) ;
  ()

let () =
  add_tests
  @@ exits 42 ~name:"Variations-on-write-output"
       (let open Construct in
       (* Many variations on `write_output` *)
       let stdout = string "/tmp/p1_out" in
       let stderr = string "/tmp/p1_err" in
       let return_value = string "/tmp/p1_ret" in
       seq
         [ write_output ~stdout ~stderr ~return_value
             (seq
                [ tprintf "%s" "hello"
                ; exec ["sh"; "-c"; "printf \"olleh\" 1>&2"]
                ; return 12 ])
         ; call [string "cat"; return_value]
         ; assert_or_fail "hello-1"
             C_string.(
               call [string "cat"; return_value]
               |> get_stdout |> Byte_array.to_c =$= string "12")
         ; write_output ~stderr ~return_value
             (seq
                [ tprintf "%s" "hello"
                ; exec ["sh"; "-c"; "printf \"olleh\" 1>&2"]
                ; return 12 ])
         ; write_output ~return_value
             (seq
                [ tprintf "%s" "hello"
                ; exec ["sh"; "-c"; "printf \"olleh\" 1>&2"]
                ; return 12 ])
         ; write_output ~stdout
             (seq
                [ tprintf "%s" "helloooo"
                ; exec ["sh"; "-c"; "printf \"olleh\" 1>&2"]
                ; return 12 ])
         ; call [string "cat"; stdout]
         ; assert_or_fail "hello-2"
             C_string.(
               call [string "cat"; stdout]
               |> get_stdout |> Byte_array.to_c =$= string "helloooo")
         ; write_output
             (seq
                [ tprintf "%s" "hello"
                ; exec ["sh"; "-c"; "printf \"olleh\" 1>&2"]
                ; return 12 ])
         ; return 42 ]) ;
  ()

let () =
  add_tests
  @@ exits 11 ~name:"write-output-as-string"
       (let open Construct in
       let stdout = string "/tmp/p1_out" in
       let stderr = string "/tmp/p1_err" in
       let return_value_path = string "/tmp/p1_ret" in
       let return_value_value = 31 in
       let will_be_escaped = "newline:\n tab: \t \x42\b" in
       let will_not_be_escaped =
         "spaces, a;c -- ' - '' \\  ''' # ''''  @ ${nope} & ` ~"
       in
       seq
         [ call [string "rm"; string "-f"; stdout; stderr; return_value_path]
         ; write_output ~stdout ~stderr ~return_value:return_value_path
             (seq
                [ tprintf "%s" will_be_escaped
                ; tprintf "%s" will_not_be_escaped
                ; exec ["sh"; "-c"; "printf \"err\\t\\n\" 1>&2"]
                ; return return_value_value ])
         ; if_then_else
             Byte_array.(
               get_stdout (call [string "cat"; stdout])
               =$= byte_array (will_be_escaped ^ will_not_be_escaped))
             (if_then_else
                Byte_array.(
                  get_stdout (call [string "cat"; stderr]) <$> byte_array "err")
                (if_then_else
                   Byte_array.(
                     get_stdout (call [string "cat"; return_value_path])
                     =$= ksprintf byte_array "%d" return_value_value)
                   (return 11) (return 22))
                (return 23))
             (return 24) ]) ;
  ()

let () =
  add_tests
  @@ exits ~name:"Basic strings" 12
       Construct.(
         (* This looks dumb but finding an encoding of strings that makes
       this work was pretty hard using CRAZIX shell *)
         if_then_else
           C_string.(string "some" =$= string "some\n")
           (return 11) (return 12)) ;
  ()

let () =
  add_tests
  @@ exits ~name:"more Basic strings" 11
       Construct.(
         if_then_else
           C_string.(
             string "some"
             =$= ( get_stdout
                     (if_then_else
                        (string "bouh\n" =$= string "bouh")
                        (tprintf "nnnooo") (tprintf "some"))
                 |> Byte_array.to_c ))
           (return 11) (return 12)) ;
  ()

let () =
  add_tests
  @@ exits 11 ~name:"output-as-empty-string"
       Construct.(
         if_then_else
           C_string.(
             string "" =$= (get_stdout (exec ["printf"; ""]) |> Byte_array.to_c))
           (return 11) (return 12)) ;
  ()

let () =
  add_tests
  @@ exits 11 ~name:"empty-string"
       Construct.(
         if_then_else
           C_string.(string "" =$= string "")
           (return 11) (return 12)) ;
  ()

let () =
  add_tests
  @@ exits 10 ~name:"byte array comparison"
       Construct.(
         if_then_else
           Byte_array.(
             byte_array "b\x00ouh\nbah\n"
             >> exec ["cat"]
             |> get_stdout
             =$= byte_array "b\x00ouh\nbah\n")
           (return 10) (return 11)) ;
  ()

let () =
  add_tests
  @@ exits 13
       (let open Construct in
       let tmp = "/tmp/test_loop_while" in
       let cat_potentially_empty =
         if_then_else (exec ["cat"; tmp] |> succeeds) nop (tprintf "")
       in
       seq
         [ exec ["rm"; "-f"; tmp]
         ; exec ["rm"; "-f"; tmp]
         ; loop_while
             Byte_array.(
               cat_potentially_empty |> get_stdout <$> byte_array "nnnn")
             ~body:(exec ["sh"; "-c"; sprintf "printf n >> %s" tmp])
         ; return 13 ]) ;
  ()

let () =
  add_tests
  @@
  let minus_f = "one \nwith \\ spaces and \ttabs -dashes -- " in
  let make ?(anon3 = "BBBBBBB") ret minus_g single count =
    let anon1 = "annonlkjde" in
    let anon2 = "annon 02e930 99e3\n d \t eij" in
    (* let anon3 = "annon deid \t dlsij" in *)
    (* let anon3 = "======== \t =====" in *)
    exits ret
      ~name:(sprintf "legacy-parse-cli-%d" count)
      ~args:["-f"; minus_f; single; anon1; "-g"; minus_g; anon2; anon3]
      (let open Genspio.EDSL_v0 in
      let open Command_line in
      let spec =
        let open Arg in
        string ~doc:"String one" ["-f"]
        & string ~doc:"String two" ["-g"]
        & flag ~doc:"Bool one" ["-v"]
        & usage "Usage string\nwith bunch of lines to\nexplain stuff"
      in
      parse spec (fun ~anon one two bone ->
          seq
            [ printf
                (string
                   "######## Begin action ########\\n=== one: '%s' two: \
                    '%s'\\n=== dollar-sharp '%s'\\n")
                [one; two; getenv (string "#")]
            ; switch
                [ case
                    C_string.(string single =$= string "-v")
                    [ assert_or_fail "bone-is-true"
                        ( bone
                        &&& C_string.(
                              concat_elist anon
                              =$= string
                                    (String.concat ~sep:"" [anon1; anon2; anon3]))
                        ) ]
                ; case
                    C_string.(string single =$= string "--")
                    (let concated_in_ocaml =
                       String.concat ~sep:""
                         [anon1; "-g"; minus_g; anon2; anon3]
                     in
                     [ printf
                         (ksprintf string
                            "######### In dash-dash case: #########\\n=== \
                             length-of-all: %d\\nbone: '%%s'\\n=== anon3: \
                             '%%s'\\n=== concat_elist anon: '%%s'\\n=== \
                             string.concat: '%%s'\\n"
                            (String.length concated_in_ocaml))
                         [ Bool.to_string bone
                         ; string anon3
                         ; C_string.concat_elist anon
                         ; string concated_in_ocaml ]
                     ; Elist.iter anon ~f:(fun v ->
                           printf (string "=== anonth: %s\\n") [v ()] )
                     ; assert_or_fail "dash-dash"
                         ( (not bone)
                         &&& C_string.(
                               concat_elist anon
                               =$= string
                                     (String.concat ~sep:""
                                        [anon1; "-g"; minus_g; anon2; anon3]))
                         ) ])
                ; default
                    [ assert_or_fail "single-is-anon"
                        ( (not bone)
                        &&& C_string.(
                              concat_elist anon
                              =$= string
                                    (String.concat ~sep:""
                                       [single; anon1; anon2; anon3])) ) ] ]
            ; if_then_else
                (C_string.(one =$= two) ||| bone)
                (return 11)
                (if_then_else
                   C_string.(one =$= string minus_f)
                   (* Should be always true *)
                   (return 12)
                   (* i.e. we're testing that weird characters have good escaping *)
                   (return 44)) ] ))
  in
  List.mapi
    ~f:(fun i f -> f i)
    [ make 11 minus_f ""
    ; make 12 "not-one" ""
    ; make 12 "not-one" "" ~anon3:(String.make 20 'S')
    ; make 11 "not-one" "-v"
    ; make 12 minus_f "--"
    ; (* the `--` should prevent the `-g one` from being parsed *)
      make 12 minus_f "--" ~anon3:(String.make 6 'S')
    ; make 12 minus_f "--" ~anon3:(String.make 7 'S')
    ; make 12 minus_f "--" ~anon3:(String.make 8 'S')
    ; make 12 minus_f "--" ~anon3:(String.make 9 'S')
    ; make 12 minus_f "--" ~anon3:(String.make 10 'S')
    ; make 12 minus_f "--" ~anon3:(String.make 11 'S')
    ; make 12 minus_f "--" ~anon3:(String.make 12 'S')
    ; make 12 minus_f "--" ~anon3:(String.make 20 'S')
    ; make 12 "not-one" "-x"
    ; (* option does not exist (untreated for now) *)
      make 12 "not-one" "--v"
    ; make 12 "not-one" "-v j"
    ; make 11 "not \\ di $bouh one" "-v"
    ; make 12 "not \\ di $bouh one" " -- -v"
    ; make 12 "one \nwith spaces and \ttabs -dashes -- " ""
    ; make 12 "one \nwith  spaces and \ttabs -dashes -- " ""
    ; make 12 "one with \\ spaces and \ttabs -dashes -- " ""
    ; make 0 "not-one" "--help"
    ; make 0 "not-one" "-help"
    ; make 0 "not-one" "-h" ]
  |> List.concat

let () =
  add_tests
  @@
  let open Genspio.EDSL in
  let minus_f = "one \nwith \\ spaces and \ttabs -dashes -- " in
  let check_anon anon anons_expected =
    Str.(concat_elist anon =$= string (String.concat ~sep:"" anons_expected))
    &&& Str.(
          (* let tmp = tmp_file "single-is-anon" in *)
          get_stdout
            (Elist.iter anon ~f:(fun item ->
                 printf (string "^^^%s@@@") [item ()] ))
          (* |> Byte_array.to_c *)
          =$= ( List.map ~f:(sprintf "^^^%s@@@") anons_expected
              |> String.concat ~sep:"" |> str ))
  in
  let spec =
    let open Command_line in
    let open Arg in
    string ~doc:"String one" ["-f"]
    & string ~doc:"String two" ["-g"]
    & flag ~doc:"Bool one" ["-v"]
    & usage "Usage string\nwith bunch of lines to\nexplain stuff"
  in
  let make ?(anon3 = "BBBBBBB") ret minus_g single count =
    let anon1 = "annonlkjde" in
    let anon2 = "annon 02e930 99e3\n d \t eij" in
    (* let anon3 = "annon deid \t dlsij" in *)
    (* let anon3 = "======== \t =====" in *)
    exits ret
      ~name:(sprintf "parse-cli-%d" count)
      ~args:["-f"; minus_f; single; anon1; "-g"; minus_g; anon2; anon3]
      (Command_line.parse spec (fun ~anon one two bone ->
           seq
             [ printf
                 (string
                    "######## Begin action ########\\n=== one: '%s' two: \
                     '%s'\\n=== dollar-sharp '%s'\\n")
                 [one; two; getenv (string "#")]
             ; switch
                 [ case
                     Str.(string single =$= string "-v")
                     [ assert_or_fail "bone-is-true"
                         (bone &&& check_anon anon [anon1; anon2; anon3]) ]
                 ; case
                     Str.(string single =$= string "--")
                     (let concated_in_ocaml =
                        String.concat ~sep:""
                          [anon1; "-g"; minus_g; anon2; anon3]
                      in
                      [ printf
                          (ksprintf string
                             "######### In dash-dash case: #########\\n=== \
                              length-of-all: %d\\nbone: '%%s'\\n=== anon3: \
                              '%%s'\\n=== concat_elist anon: '%%s'\\n=== \
                              string.concat: '%%s'\\n"
                             (String.length concated_in_ocaml))
                          [ Bool.to_string bone
                          ; string anon3
                          ; Str.concat_elist anon
                          ; string concated_in_ocaml ]
                      ; Elist.iter anon ~f:(fun v ->
                            printf (string "=== anonth: %s\\n") [v ()] )
                      ; assert_or_fail "dash-dash"
                          ( (not bone)
                          &&& check_anon anon
                                [anon1; "-g"; minus_g; anon2; anon3] ) ])
                 ; default
                     [ assert_or_fail "single-is-anon"
                         (let anons_expected = [single; anon1; anon2; anon3] in
                          (not bone) &&& check_anon anon anons_expected) ] ]
             ; if_then_else
                 (Str.(one =$= two) ||| bone)
                 (return 11)
                 (if_then_else
                    Str.(one =$= string minus_f)
                    (* Should be always true *)
                    (return 12)
                    (* i.e. we're testing that weird characters have good escaping *)
                    (return 44)) ] ))
  in
  let only_anon args name =
    exits
      ~name:(sprintf "parse-cli-only-anon-%s" name)
      ~args 0
      (Command_line.parse spec (fun ~anon one two bone ->
           assert_or_fail
             (sprintf "anon-is-anon-%s" name)
             (check_anon anon args) ))
  in
  [ only_anon [] "nothing"
  ; only_anon ["one"] "one"
  ; only_anon (List.init 4 ~f:(sprintf "a%d")) "fouras" ]
  @ List.mapi
      ~f:(fun i f -> f i)
      [ make 11 minus_f ""
      ; make 12 "not-one" ""
      ; make 12 "not-one" "" ~anon3:(String.make 20 'S')
      ; make 11 "not-one" "-v"
      ; make 12 minus_f "--"
      ; (* the `--` should prevent the `-g one` from being parsed *)
        make 12 minus_f "--" ~anon3:(String.make 6 'S')
      ; make 12 minus_f "--" ~anon3:(String.make 7 'S')
      ; make 12 minus_f "--" ~anon3:(String.make 8 'S')
      ; make 12 minus_f "--" ~anon3:(String.make 9 'S')
      ; make 12 minus_f "--" ~anon3:(String.make 10 'S')
      ; make 12 minus_f "--" ~anon3:(String.make 11 'S')
      ; make 12 minus_f "--" ~anon3:(String.make 12 'S')
      ; make 12 minus_f "--" ~anon3:(String.make 20 'S')
      ; make 12 "not-one" "-x"
      ; (* option does not exist (untreated for now) *)
        make 12 "not-one" "--v"
      ; make 12 "not-one" "-v j"
      ; make 11 "not \\ di $bouh one" "-v"
      ; make 12 "not \\ di $bouh one" " -- -v"
      ; make 12 "one \nwith spaces and \ttabs -dashes -- " ""
      ; make 12 "one \nwith  spaces and \ttabs -dashes -- " ""
      ; make 12 "one with \\ spaces and \ttabs -dashes -- " ""
      ; make 0 "not-one" "--help"
      ; make 0 "not-one" "-help"
      ; make 0 "not-one" "-h" ]
  |> List.concat

let () =
  add_tests
  @@ exits 77 ~name:"die in a sequence"
       Construct.(
         seq [tprintf "Going to die"; fail "die in sequence"; return 42]) ;
  ()

let () =
  add_tests
  @@ exits 77 ~name:"cannot capture death itself"
       Construct.(
         seq
           [ write_output ~return_value:(string "/tmp/dieretval")
               (seq
                  [ tprintf "Going to die\n"
                  ; fail "cannot capture death"
                  ; return 42 ])
           ; return 23 ]) ;
  ()

let () =
  add_tests
  @@ exits 77 ~name:"cannot poison death either"
       Construct.(
         seq
           [ byte_array "dj ijdedej j42 ijde - '' "
             >> seq
                  [ tprintf "Going to die\n"
                  ; fail "cannot poison death"
                  ; return 42 ]
           ; return 23 ]) ;
  ()

let () =
  add_tests
  @@
  let gives = 11 in
  let does_not_give = 12 in
  let t cmd yn value =
    let name =
      sprintf "%s %s %d" cmd
        (if yn = gives then "returns" else "does not return")
        value
    in
    exits ~name yn
      Construct.(
        if_then_else
          (exec ["sh"; "-c"; cmd] |> returns ~value)
          (return gives) (return does_not_give))
  in
  List.concat
    [ t "ls" gives 0
    ; t "ls /deijdsljidisjeidje" does_not_give 0
    ; t "ls /deijdsljidisjeidje" does_not_give 42
    ; t "exit 2" gives 2
    ; exits 21 ~name:"More complex return check"
        Construct.(
          if_then_else
            ( seq
                [ tprintf "I aaam so complex!\n"
                ; if_then_else
                    C_string.(
                      string "djsleidjs"
                      =$= (get_stdout (tprintf "diejliejjj") |> Byte_array.to_c))
                    (return 41) (return 42) ]
            |> returns ~value:42 )
            (return 21) (return 22)) ]

let () =
  add_tests
  @@ exits ~name:"error-in-get-stdout-1" 11
       (let open Construct in
       let tmp = "/tmp/test_error_in_get_stdout" in
       let cat_tmp = exec ["cat"; tmp] in
       seq
         [ exec ["rm"; "-f"; tmp]
         ; if_then_else
             (* cat <absent-file> |> to_string does not abort the script: *)
             C_string.(cat_tmp |> get_stdout |> Byte_array.to_c =$= string "")
             (return 11) (return 12) ]) ;
  ()

let () =
  add_tests
  @@ exits ~name:"error-in-get-stdout-2" 11
       (let open Construct in
       let tmp = "/tmp/test_error_in_get_stdout" in
       let cat_tmp = exec ["cat"; tmp] in
       seq
         [ exec ["rm"; "-f"; tmp]
         ; if_then_else
             C_string.(
               seq [tprintf "aaa"; cat_tmp]
               |> get_stdout |> Byte_array.to_c =$= string "aaa")
             (return 11) (return 12) ]) ;
  ()

let () =
  add_tests
  @@ exits ~name:"error-in-get-stdout-3" 77
       (let open Construct in
       let tmp = "/tmp/test_error_in_get_stdout" in
       let cat_tmp = exec ["cat"; tmp] in
       let succeed_or_die ut =
         if_then_else (succeeds ut) nop
           (seq [tprintf "Failure !"; fail "succeed_or_die"])
       in
       seq
         [ exec ["rm"; "-f"; tmp]
         ; tprintf "ps-output:\\n"
         ; exec ["ps"]
         ; if_then_else
             C_string.(
               seq [tprintf "aaa"; cat_tmp]
               |> succeed_or_die |> get_stdout |> Byte_array.to_c
               =$= string "aaa")
             (return 11) (return 12) ]) ;
  ()

let () =
  add_tests
  @@ (* Use of the `call` constructor: *)
     exits 28
       Construct.(
         if_then_else
           ( call
               [ string "cat"
               ; get_stdout (tprintf "/does not exist") |> Byte_array.to_c ]
           |> succeeds )
           (return 11) (return 28)) ;
  ()

let () =
  add_tests
  @@ List.concat
       [ exits 17 Construct.(if_then_else (bool true) (return 17) (return 16))
       ; exits 16
           Construct.(
             if_then_else (bool true &&& bool false) (return 17) (return 16))
       ; exits 16
           Construct.(
             if_then_else
               (bool true &&& not (bool false))
               (return 16) (return 17))
       ; exits 11
           Construct.(
             if_then_else
               C_string.(int 42 |> Integer.to_string =$= string "42")
               (return 11) (return 13)) ]

(* Bunch of Interger/arithmetic tests: *)
let () =
  add_tests
  @@ List.concat
       [ exits 12
           Construct.(
             if_then_else
               C_string.(
                 int 42 |> Integer.to_string |> Integer.of_string
                 |> Integer.to_string =$= string "42")
               (return 12) (return 13))
       ; exits 12
           Construct.(
             if_then_else
               C_string.(
                 int (-42) |> Integer.to_string |> Integer.of_string
                 |> Integer.to_string =$= string "-42")
               (return 12) (return 13))
       ; exits ~name:"failure-of-int-of-string" 77
           Construct.(
             (* It's not a string representing an integer: *)
             if_then_else
               C_string.(
                 string "87732b" |> Integer.of_string |> Integer.to_string
                 =$= string "8877732")
               (return 12) (return 13))
       ; exits 12
           Construct.(
             if_then_else
               C_string.(
                 Integer.(int 22 + int 20) |> Integer.to_string =$= string "42")
               (return 12) (return 13))
       ; exits 12
           Construct.(
             if_then_else
               C_string.(
                 Integer.(int 2 * (int 22 - int 20))
                 |> Integer.to_string =$= string "4")
               (return 12) (return 13))
       ; exits 12
           (let open Construct in
           let trybin res b =
             C_string.(b |> Integer.to_string =$= string (Int.to_string res))
           in
           if_then_else
             ( trybin 1 Integer.(int 2 * (int 22 - int 20) / int 4)
             &&& trybin 0 Integer.(int 2 * (int 22 - int 20) / int 5)
             &&& trybin 8 Integer.(int 42 / int 5)
             &&& trybin 2 Integer.(int 42 mod int 5)
             &&& trybin 0 Integer.(int 3000 mod int 3) )
             (return 12) (return 13))
       ; exits 17
           Construct.(
             if_then_else Integer.(int 2 = int 2) (return 17) (return 13))
       ; exits 13
           Construct.(
             if_then_else Integer.(int 2 > int 2) (return 17) (return 13))
       ; exits 13
           Construct.(
             if_then_else Integer.(int 2 < int 2) (return 17) (return 13))
       ; exits 23
           Construct.(
             if_then_else
               ( Integer.(int 2 <= int 2)
               &&& Integer.(int 2 >= int 2)
               &&& Integer.(int 3 > int 2)
               &&& Integer.(int 3 >= int 2)
               &&& Integer.(int 3 <> int 2)
               &&& not Integer.(int 3 = int 2)
               &&& Integer.(int (-1) < int 2) )
               (return 23) (return 13)) ]

let () =
  add_tests
  @@ exits ~name:"getenv" 25
       (let open Construct in
       let alternate_get_env v =
         (* We cannot use OCaml's Sys.getenv because the compilation output may
         be run on a different host/system (through SSH or alike). *)
         exec ["sh"; "-c"; sprintf "echo ${%s} | tr -d '\\n'" v]
         |> get_stdout |> Byte_array.to_c
       in
       if_then_else
         C_string.(
           getenv (string "HOME")
           =$= alternate_get_env "HOME"
           &&& (getenv (string "PATH") =$= alternate_get_env "PATH")
           &&& ( getenv (concat_list [string "PA"; string "TH"])
               =$= alternate_get_env "PATH" ))
         (return 25) (return 13)) ;
  ()

let () =
  add_tests
  @@ exits 29
       Construct.(
         if_then_else
           C_string.(getenv (string "HOMEEEEEEEE") =$= string "")
           (return 29) (return 27)) ;
  ()

let () =
  add_tests
  @@ exits 27 ~name:"getenv-corner-case-00"
       Construct.(
         if_then_else
           C_string.(
             (* Explicit test of a corner case: *)
             getenv (string "HOME\nME") =$= string (Sys.getenv "HOME"))
           (return 12) (return 27)) ;
  ()

(* This used to be a corner case test but with the string-schism,
   it becomes just a `Byte_array.to_c` normal failure. *)
let () =
  add_tests
  @@ exits 77 ~name:"Weird-env-variable"
       Construct.(
         if_then_else
           C_string.(
             getenv (string "HOME\000ME") =$= string (Sys.getenv "HOME"))
           (return 12) (return 27)) ;
  ()

let () =
  add_tests
  @@ exits 20 ~name:"setenv-getenv"
       (let open Genspio.EDSL_v0 in
       let var = string "VVVVVVV" in
       let assert_or_return ret cond =
         if_then_else cond nop
           (seq [tprintf "Fail: %d" ret; fail "assert_or_return"])
       in
       let set_and_test ?(varname = "XXXX") value =
         let var = string varname in
         seq
           [ setenv ~var (string value)
           ; if_seq
               C_string.(
                 (* The behavior when there is a final new-line is
                    considered undefined, the slow-stack compiler seems
                    to respect them but default one doesn't. We consider
                    both bahaviors OK for now. *)
                 getenv var =$= string value
                 ||| (getenv var =$= string (String.strip value)))
               ~t:[]
               ~e:
                 [ printf
                     (ksprintf string
                        "\\n====\\nvarname: %s\\nvalue: %S\\ngetenv: \
                         {%%s}\\nshell: {%%s}\\n"
                        varname value)
                     [ getenv var
                     ; exec ["sh"; "-c"; sprintf "echo \"ECHO: $%s\"" varname]
                       |> get_stdout |> Byte_array.to_c ]
                 ; ksprintf fail "fail: %s" value ] ]
       in
       seq
         [ assert_or_return 27 C_string.(getenv var =$= string "")
         ; set_and_test "bouh"
         ; set_and_test "bouh\nbah"
         ; set_and_test "bouh\nbah\n"
         ; set_and_test "bouh'bah"
         ; set_and_test "bouh\"bah"
         ; set_and_test "bouh\001bah"
         ; (* We check that the environment is affected properly: *)
           setenv ~var:(string "AAA") (string "aaa")
         ; assert_or_return 42
             (exec ["/bin/sh"; "-c"; "[ \"$AAA\" = \"aaa\" ]"] |> succeeds)
         ; return 20 ]) ;
  ()

let () =
  add_tests
  @@ List.concat
       [ exits ~name:"tmp#basic" 23
           (let open Genspio.EDSL_v0 in
           let tmp = tmp_file "test" in
           seq [tmp#set (byte_array ""); return 23])
       ; exits ~name:"tmp#delete" 23
           (let open Genspio.EDSL_v0 in
           let tmp = tmp_file "test" in
           let s1 = byte_array "hello\000you" in
           seq
             [ tmp#set (byte_array "")
             ; assert_or_fail "tmp#get 1" C_string.(tmp#get_c =$= string "")
             ; tmp#set s1
             ; assert_or_fail "tmp#get 2" Byte_array.(tmp#get =$= s1)
             ; tmp#delete
             ; assert_or_fail "tmp#get 3" C_string.(tmp#get_c =$= string "")
             ; return 23 ]) ]

let () =
  add_tests
  @@ List.concat
       [ exits 2 ~name:"no-trap" Genspio.EDSL.(return 2)
       (* Dying with error messages does not work without `trap` any more
       (string-schism): *)
       (* exits 2 ~no_trap:true ~name:"no-trap-but-failwith" Genspio.EDSL.( *)
       (*     seq [ *)
       (*       with_failwith (fun die -> *)
       (*           seq [ *)
       (*             (\* tprintf "Dying now\n"; *\) *)
       (*             die *)
       (*               ~message:(byte_array "HElllooo I'm dying!!\n") ~return:(int 2) *)
       (*           ] *)
       (*         ); *)
       (*     ] *)
       (*   ); *)
        ]

let () =
  add_tests
  @@ exits 21 ~name:"empty-seq" Genspio.EDSL.(seq [seq []; return 21]) ;
  ()

let () =
  add_tests
  @@ List.concat
       [ exits 2 ~name:"redirect-stuff"
           (let open Genspio.EDSL_v0 in
           let tmp1 = tmp_file "stdout" in
           let tmp2 = tmp_file "stderr" in
           let empty = string "" in
           let init = byte_array "This should be erraasseed" in
           let recognizable = "heelllloooooo" in
           seq
             [ call
                 [ string "printf"
                 ; string "1: %s, 2: %s\n"
                 ; tmp1#path
                 ; tmp2#path ]
             ; tmp1#set init
             ; tmp2#set init
             ; write_output ~stdout:tmp1#path ~stderr:tmp2#path
                 (with_redirections
                    (exec ["printf"; "%s"; recognizable])
                    [to_fd (int 1) (int 2)])
             ; assert_or_fail "stdout-empty" C_string.(tmp1#get_c =$= empty)
             ; assert_or_fail "stderr-hello"
                 ( (* We can only test with grep because stderr contains a bunch of
               other stuff, especially since we use the
               `-x` option of the shells *)
                   tmp2#get
                 >> exec ["grep"; recognizable]
                 |> succeeds )
             ; return 2 ])
       ; exits 3 ~name:"redirect-many"
           (let open Genspio.EDSL_v0 in
           let tmp1 = tmp_file "fd3" in
           let tmp2 = tmp_file "fd3-other" in
           let recognizable = "heelllloooooo" in
           seq
             [ tmp1#set (byte_array "")
             ; tmp2#set (byte_array "")
             ; with_redirections
                 (exec ["printf"; "%s"; recognizable])
                 [ to_file (int 3) tmp1#path
                 ; to_file (int 3) tmp2#path
                 ; (* we hijack tmp1's use of fd 3 *)
                   to_fd (int 2) (int 3)
                 ; to_fd (int 1) (int 2) ]
             ; call [string "cat"; tmp1#path]
             ; call [string "cat"; tmp2#path]
             ; assert_or_fail "fd3-empty" C_string.(tmp1#get_c =$= string "")
             ; assert_or_fail "fd3-other-recog"
                 ( (* Again going through fd `2` we've grabbed some junk: *)
                   tmp2#get
                 >> exec ["grep"; recognizable]
                 |> succeeds )
             ; return 3 ])
       ; exits 2 ~name:"redirect-fails"
           (let open Genspio.EDSL_v0 in
           let tmp1 = tmp_file "fd3" in
           let tmp2 = tmp_file "return" in
           let recognizable = "heelllloooooo" in
           let this_is_bash =
             exec ["ps"]
             |> get_stdout
             >> exec ["grep"; "bash"]
             |> returns ~value:0
           in
           seq
             [ tmp1#set (byte_array "")
             ; write_output ~return_value:tmp2#path
                 (with_redirections
                    (exec ["printf"; "%s"; recognizable])
                    [ to_fd (int 4) (int 3)
                    ; (* This fails because &3 is not open! *)
                      to_file (int 1) tmp1#path ])
             ; call [string "printf"; string "%s:\\n"; tmp1#path]
             ; call [string "cat"; tmp1#path]
             ; call [string "printf"; string "%s:\\n"; tmp2#path]
             ; call [string "cat"; tmp2#path]
             ; assert_or_fail "fd3"
                 ( C_string.(tmp1#get_c =$= string "")
                 ||| ( this_is_bash
                     &&& C_string.(tmp1#get_c =$= string recognizable) ) )
             ; assert_or_fail "return-value"
                 ( C_string.(tmp2#get_c =$= string "1")
                 ||| C_string.(tmp2#get_c =$= string "2")
                 ||| (this_is_bash &&& C_string.(tmp2#get_c =$= string "0")) )
             ; return 2 ]) ]

let () =
  add_tests
  @@ List.concat
       [ exits 2 ~name:"bool-string-conversions"
           Genspio.EDSL_v0.(
             seq
               [ assert_or_fail "test1"
                   C_string.(Bool.to_string (bool true) =$= string "true")
               ; assert_or_fail "test2"
                   C_string.(Bool.to_string (bool false) =$= string "false")
               ; assert_or_fail "test3"
                   (Bool.to_string (bool true) |> Bool.of_string)
               ; assert_or_fail "test4"
                   (Bool.to_string (bool false) |> Bool.of_string |> not)
               ; assert_or_fail "test5"
                   ( Bool.to_string
                       (exec ["ls"; "/deiuhdse"] |> succeeds_silently)
                   |> Bool.of_string |> not )
               ; assert_or_fail "test6"
                   ( Bool.to_string (exec ["ls"; "/"] |> succeeds_silently)
                   |> Bool.of_string )
               ; return 2 ])
       ; exits 77 ~name:"bool-string-wrong-conversions"
           Genspio.EDSL.(
             if_then_else
               (string "anything" |> Bool.of_string |> not)
               (return 11) (return 12)) ]

let () =
  add_tests
  @@ exits 5 ~name:"list-string-stuff"
       Genspio.EDSL_v0.(
         seq
           [ assert_or_fail "test1"
               C_string.(
                 C_string.concat_elist
                   (Elist.make [string "one"; string "two"; string "three"])
                 =$= string "onetwothree")
           ; assert_or_fail "test2"
               C_string.(
                 C_string.concat_elist
                   (Elist.make [string "one"; string "two"])
                 =$= string "onetwo")
           ; assert_or_fail "test3"
               C_string.(
                 C_string.concat_elist (Elist.make [string "one"])
                 =$= string "one")
           ; assert_or_fail "test4"
               C_string.(C_string.concat_elist (Elist.make []) =$= string "")
           ; assert_or_fail "test5"
               C_string.(
                 C_string.concat_elist (Elist.make [string ""]) =$= string "")
           ; assert_or_fail "test6"
               C_string.(
                 C_string.concat_elist
                   (Elist.make [string "one"; string ""; string "three"])
                 =$= string "onethree")
           ; assert_or_fail "test7"
               C_string.(
                 C_string.concat_elist
                   (Elist.make [string "one"; string ""; string ""])
                 =$= string "one")
           ; return 5 ]) ;
  ()

let () =
  add_tests
  @@ exits 5 ~name:"list-append"
       (let open Genspio.EDSL_v0 in
       let make_string_concat_test name la lb =
         let slist l = List.map l ~f:string |> Elist.make in
         assert_or_fail name
           C_string.(
             concat_elist (Elist.append (slist la) (slist lb))
             =$= string (la @ lb |> String.concat ~sep:""))
       in
       seq
         [ make_string_concat_test "test1" ["one"; "two"; "three"] []
         ; make_string_concat_test "test2" ["one"; "two"; "three"] ["four"]
         ; make_string_concat_test "test3" ["one"; "two"] ["thre"; "four"]
         ; make_string_concat_test "test4" [] ["thre"; "four"]
         ; make_string_concat_test "test5" [] []
         ; make_string_concat_test "test6" [""] []
         ; make_string_concat_test "test7" [""] [""]
         ; make_string_concat_test "test8" [] [""]
         ; make_string_concat_test "test9" [] ["deiajd\ndedaeijl"; ""]
         ; make_string_concat_test "test10" [] ["deiajd\ndeda\001eijl"; ":"]
         ; return 5 ]) ;
  ()

let () =
  add_tests
  @@
  (* We make a bunch of separate tests to avoid the command line
       argument size limit: *)
  let make_list_iter_strings_test i l =
    let name = sprintf "list-iter-strings-%d-%d-strings" i (List.length l) in
    exits 5 ~name
      (let open Genspio.EDSL_v0 in
      let slist = List.map l ~f:byte_array |> Elist.make in
      let tmp = ksprintf tmp_file "listitertest%d" i in
      let tmp2 = ksprintf tmp_file "listserializationtest%d" i in
      seq
        [ tmp#set (byte_array "")
        ; (* We serialize the list to `tmp2`: *)
          tmp2#set (Elist.serialize_byte_array_list slist)
        ; (* We get back the serialized list from `tmp2`: *)
          tmp2#get |> Elist.deserialize_to_byte_array_list
          |> Elist.iter ~f:(fun v ->
                 seq
                   [ eprintf
                       (string "Concatenating: '%s'\\n")
                       [v () |> Byte_array.to_c]
                   ; assert_or_fail
                       (* See issue #76 *)
                       "list-item-inside-condition"
                       Byte_array.(v () =$= v ())
                   ; tmp#set
                       ( C_string.concat_list
                           [tmp#get_c; string ":"; v () |> Byte_array.to_c]
                       |> C_string.to_bytes )
                   (* The `:` makes sure we count right [""] ≠ [""; ""] etc. *)
                    ] )
        ; assert_or_fail name
            C_string.(
              tmp#get_c
              =$= string
                    (String.concat (List.map l ~f:(sprintf ":%s")) ~sep:""))
        ; return 5 ])
  in
  List.concat_mapi ~f:make_list_iter_strings_test
    [ ["zero"]
    ; ["one"; "two"; "three"]
    ; ["four"]
    ; []
    ; [""]
    ; [""; ""]
    ; [""; "bouh"; ""; "bah"]
    ; ["deiajd\ndedaeijl"; ""]
    ; ["deiajd\ndeda\001eijl"; ":"]
    (* Used `\001` because we convert to C-strings in the *)
     ]

let () =
  add_tests
  @@
  let make_int_test i l =
    let name =
      sprintf "list-iter-ints-%d-%s" i
        (List.map l ~f:Int.to_string |> String.concat ~sep:"-")
    in
    exits 5 ~name
      (let open Genspio.EDSL_v0 in
      let ilist = List.map l ~f:int |> Elist.make in
      let tmp = tmp_file ("tmp-" ^ name) in
      let tmp2 = tmp_file ("tmp2" ^ name) in
      (* Checking that implementing `fold` with `iter` does the `fold`: *)
      seq
        [ (* We serialize the list to `tmp2`: *)
          tmp2#set @@ Elist.serialize_int_list ilist
        ; tmp#set (int 0 |> Integer.to_byte_array)
        ; (* We get back the serialized list from `tmp2`: *)
          tmp2#get |> Elist.deserialize_to_int_list
          |> Elist.iter ~f:(fun v ->
                 seq
                   [ eprintf (string "Adding: '%s'\\n")
                       [v () |> Integer.to_string]
                   ; assert_or_fail
                       (* See issue #76 *)
                       "list-item-inside-condition"
                       Integer.(v () = v ())
                   ; tmp#set
                       Integer.(
                         (tmp#get |> of_byte_array) + v () |> to_byte_array) ]
             )
        ; printf (string "TMP: %s, TMP2: %s\\n") [tmp#path; tmp2#path]
        ; call [string "cat"; tmp#path]
        ; printf (string "::\\n") []
        ; assert_or_fail name
            C_string.(
              tmp#get_c
              =$= Integer.to_string (List.fold ~init:0 l ~f:( + ) |> int))
        ; return 5 ])
  in
  List.concat_mapi ~f:make_int_test
    [[]; [1]; [3]; [1; 2; 3]; [1; 2; 3; 0]; List.init 42 (fun i -> i)]

let () =
  add_tests
  @@ List.concat
       [ exits 13 ~name:"pipe-basic"
           (let open Genspio.EDSL_v0 in
           let bag =
             pipe
               [ exec ["printf"; "hello-world\\n"]
               ; exec ["tr"; "-d"; "-"]
               ; exec ["sed"; "s/wo/Wo/"]
               ; exec ["sed"; "s/h/H/"]
               ; exec ["tr"; "-d"; "\\n"] ]
             |> get_stdout |> Byte_array.to_c
           in
           let fed =
             "let fed"
             %%% ( bag |> C_string.to_bytes
                 >> pipe [exec ["tr"; "H"; "B"]]
                 |> get_stdout |> Byte_array.to_c )
           in
           "pipe-basic test"
           %%% seq
                 [ eprintf (string "Bag: %s") [bag]
                 ; "pipe-basic1 assertion"
                   %%% assert_or_fail "pipe-basic1"
                         C_string.(
                           "Bag in pipe-basic 1" %%% bag
                           =$= string "HelloWorld")
                 ; "pipe-basic2 assertion"
                   %%% assert_or_fail "pipe-basic2"
                         C_string.(fed =$= string "BelloWorld")
                 ; return 13 ])
       ; exits 42 ~name:"pipe-xargs"
           Genspio.EDSL_v0.(
             seq
               [ assert_or_fail "pipe-xargs-1"
                   C_string.(
                     get_stdout
                       ( exec ["printf"; "1\\n2\\n3\\n"]
                       ||> exec ["xargs"; "printf"; "%02d:%02d:%02d"] )
                     |> Byte_array.to_c =$= string "01:02:03")
               ; return 42 ]) ]

let () =
  let tests =
    [ exits 12 ~name:"edsl-ng-on-stdin-lines"
        Genspio.EDSL.(
          let tmp1 = tmp_file "edsl-ng-on-stdin-lines-tmp1" in
          let tmp2 = tmp_file "edsl-ng-on-stdin-lines-tmp2" in
          let cmd = exec ["ls"; "/"] in
          seq
            [ write_stdout cmd ~path:tmp1#path
            ; tmp2#set (str "")
            ; cmd
              ||> on_stdin_lines (fun line ->
                      seq [tmp2#append line; tmp2#append (str "\n")] )
            ; say "<<%s>> Vs <<%s>>" [tmp1#get; tmp2#get]
            ; assert_or_fail "tmp1 = tmp2" Str.(tmp1#get =$= tmp2#get)
            ; return 12 ])
    ; exits 12 ~name:"edsl-ng-cmd-avail"
        Genspio.EDSL.(
          seq
            [ assert_or_fail "ls is there" (command_available (str "ls"))
            ; assert_or_fail "lslslsls is not there"
                (not (command_available (str "lslslsls")))
            ; return 12 ])
    ; exits 12 ~name:"edsl-ng-getout-1line"
        Genspio.EDSL.(
          let make_test f input output =
            assert_or_fail (sprintf "%S" output)
              Str.(f (printf (str input) []) =$= str output)
          in
          seq
            [ make_test get_stdout_one_line "one two\\nthree\\nfo ur"
                "one twothreefo ur"
            ; make_test get_stdout_one_line "one two\\n  three\\nfo ur"
                "one two  threefo ur"
            ; make_test
                (get_stdout_one_line ~remove_spaces:true)
                "one two\\n  three\\nfo ur" "onetwothreefour"
            ; make_test
                (get_stdout_one_line ~first_line:true ~remove_spaces:true)
                "one two\\n  three\\nfo ur" "onetwo"
            ; make_test
                (get_stdout_one_line ~first_line:true ~remove_spaces:false)
                "one two  \\n  three\\nfo ur" "one two  "
            ; make_test
                (get_stdout_one_line ~first_line:true ~remove_spaces:false)
                "\\n  three\\nfo ur" ""
            ; make_test
                (get_stdout_one_line ~first_line:true ~remove_spaces:false)
                "" ""
            ; make_test
                (get_stdout_one_line ~first_line:false ~remove_spaces:false)
                "  " "  "
            ; make_test
                (get_stdout_one_line ~first_line:true ~remove_spaces:true)
                "  \\nAA" ""
            ; make_test
                (get_stdout_one_line ~first_line:true ~remove_spaces:true)
                "     " ""
            ; return 12 ])
    ; exits 12 ~name:"edsl-ng-is-stuff"
        Genspio.EDSL.(
          let tmp = tmp_file "bouh" in
          seq
            [ tmp#set (str "")
            ; assert_or_fail "is-reg" (is_regular_file tmp#path)
            ; assert_or_fail "is-not-reg" (is_regular_file (str "/tmp") |> not)
            ; assert_or_fail "is-not-dir" (is_directory tmp#path |> not)
            ; assert_or_fail "is-dir" (is_directory (str "/tmp"))
            ; assert_or_fail "is-not-exec" (is_executable tmp#path |> not)
            ; verbose_call [str "chmod"; str "+x"; tmp#path]
            ; assert_or_fail "is-exec" (is_executable tmp#path)
            ; assert_or_fail "is-read" (is_readable tmp#path)
            ; assert_or_fail "is-not-read"
                (is_readable (str "/etc/shadow") |> not)
            ; return 12 ])
    ; exits 12 ~name:"edsl-ng-greps-to"
        Genspio.EDSL.(
          let tst extended_re re pre does =
            assert_or_fail
              (sprintf "%s does %smatch %s (E: %b)" re
                 (if does then "" else "not")
                 pre extended_re)
              ( greps_to ~extended_re (str re) (printf (str pre) [])
              |> if does then fun e -> e else not )
          in
          let tn = tst false in
          let te = tst true in
          seq
            [ tn "hello" "hello" true
            ; te "hello" "hello" true
            ; te "hello" "\\n ldje hello  dleijsd\\n" true
            ; tn "helllo" "\\n ldje hello  dleijsd\\n" false
            ; tn "hel\\*o" "\\n ldje hello  dleijsd\\n" false
            ; te "hel\\*o" "\\n ldje hello  dleijsd\\n" false
            ; tn "hel*o" "\\n ldje hello  dleijsd\\n" true
            ; te "hel*o" "\\n ldje hello  dleijsd\\n" true
            ; tn " hell?o " "\\n ldje hello  dleijsd\\n" false
            ; te " hell?o " "\\n ldje hello  dleijsd\\n" true
            ; return 12 ]) ]
  in
  add_tests (List.concat tests)

let compilation_error_tests () =
  let results = ref [] in
  let test ?options e expect =
    let res = Genspio.Compile.To_posix.string ?options e in
    results := (e, expect res, res) :: !results
  in
  let open Genspio in
  test
    EDSL.(
      "comment 0" %%% seq ["comment 1" %%% exec ["echo"; "diej\000dejldsjie"]])
    (function
      | Error
          { Genspio.Compile.To_posix.error= `Not_a_c_string "diej\000dejldsjie"
          ; code= Some _
          ; comment_backtrace= ["comment 1"; "comment 0"] } ->
          true
      | _ -> false) ;
  let too_big = String.make 200_000 'B' in
  test
    EDSL.("comment 0" %%% seq [exec ["echo"; too_big]])
    (function
      | Error
          { Genspio.Compile.To_posix.error= `Max_argument_length that_one
          ; code= Some _
          ; comment_backtrace= ["comment 0"] }
        when that_one = Filename.quote too_big ->
          true
      | _ -> false) ;
  let options =
    {Genspio.Compile.To_posix.multi_line with fail_with= `Nothing}
  in
  test ~options
    EDSL.(
      "comment 0" %%% seq [exec ["echo"; "fail"]; "cmt2" %%% fail "failure"])
    (function
      | Error
          { Genspio.Compile.To_posix.error= `No_fail_configured (User "failure")
          ; code= None
          ; comment_backtrace= ["cmt2"; "comment 0"] } ->
          true
      | _ -> false) ;
  printf "Compilation-error tests:\n%!" ;
  List.iter (List.rev !results) ~f:(fun (expr, succ, res) ->
      let expr_str = Genspio.Compile.to_string_hum expr in
      let res_str =
        match res with
        | Ok s -> s
        | Error e -> Genspio.Compile.To_posix.error_to_string e
      in
      printf "* %s: %s\n%s\n%!%!"
        (if succ then "SUCCESS" else "FAILURE")
        (String.sub expr_str 0 60 |> Option.value ~default:expr_str)
        res_str ) ;
  List.exists !results ~f:(fun (_, res, _) -> res = false)

let () =
  let anon = ref [] in
  let usage = "$0 [-help] <path>" in
  let important_shells = ref ["bash"; "dash"; "busybox"] in
  let failf fmt =
    ksprintf
      (fun s ->
        eprintf "Error: %s\nUsage: %s\n%!" s usage ;
        exit 1 )
      fmt
  in
  let anon_fun p = anon := p :: !anon in
  let no_compilation_tests = ref false in
  let extra_slow_flow_tests = ref false in
  let extra_transform_cp_tests = ref false in
  let filter_tests = ref None in
  let args =
    Arg.align
      [ ( "--important-shells"
        , Arg.String
            (fun s -> important_shells := String.split ~on:(`Character ',') s)
        , sprintf
            "<comma-sep-list> Set the shells that are considered \
             “important”\n\
             \t(default: '%s')."
            (String.concat ~sep:"," !important_shells) )
      ; ( "--filter-tests"
        , Arg.String (fun s -> filter_tests := Some s)
        , "<match> Just keep the tests matching <match>" )
      ; ( "--no-compilation-tests"
        , Arg.Set no_compilation_tests
        , " Do not do the compilation tests." )
      ; ( "--run-slow-stack-tests"
        , Arg.Set extra_slow_flow_tests
        , " Run the To_slow_flow.test tests." )
      ; ( "--run-constant-propagation-tests"
        , Arg.Set extra_transform_cp_tests
        , " Run the Transform.Constant_propagation tests." )
      ; ( "--"
        , Rest anon_fun
        , "<args> Arguments following are not interpreted as CL options." ) ]
  in
  Arg.parse args anon_fun usage ;
  let path_opt =
    match !anon with
    | [one] -> Some one
    | [] -> None
    | moar -> failf "Too many arguments: %s" (String.concat ~sep:", " moar)
  in
  Option.iter path_opt ~f:(fun path ->
      let open Test in
      let testlist =
        List.concat !tests
        |>
        match !filter_tests with
        | None -> fun e -> e
        | Some s ->
            let matches name = String.index_of_string name ~sub:s <> None in
            fun tests ->
              List.filter tests ~f:(function
                | Tests.Test_lib.Test.Exits
                    {no_trap; name; args; returns; script}
                  when matches name ->
                    true
                | other -> false )
      in
      printf "Testlist: %d\n%!" (List.length testlist) ;
      let testdir =
        let tests =
          List.concat_map
            Shell.(known_shells ())
            ~f:(fun shell ->
              let make compilation optimization_passes =
                Shell_directory.
                  {shell; verbose= true; compilation; optimization_passes}
              in
              [ make `Std_one_liner []
              ; make `Std_multi_line []
              ; make `Std_multi_line [`Cst_prop]
              ; make `Slow_stack []
              ; make `Slow_stack [`Cst_prop] ] )
        in
        let open Test_directory in
        {shell_tests= tests; important_shells= !important_shells; verbose= true}
      in
      let todo = Test_directory.contents testdir ~path testlist in
      List.iter todo ~f:(function
        | `File (p, v) ->
            let mo = open_out p in
            fprintf mo "%s\n" v ; close_out mo
        | `Directory v -> ksprintf Sys.command "mkdir -p '%s'" v |> ignore ) ) ;
  let errors =
    if !no_compilation_tests then false else compilation_error_tests ()
  in
  if !extra_slow_flow_tests then Genspio.To_slow_flow.test () ;
  if !extra_transform_cp_tests then
    Genspio.Transform.Constant_propagation.test () ;
  exit (if errors then 23 else 0)
