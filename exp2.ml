
open Nonstd
module String = Sosa.Native_string
(*

   ocamlbuild -use-ocamlfind -package sosa,nonstd,pvem_lwt_unix exp2.byte && ./exp2.byte

*)

let with_buffer ?(size = 42) f =
  let b = Buffer.create 42 in
  let str = Buffer.add_string b in
  f str;
  Buffer.contents b

module Test = struct
  open Pvem_lwt_unix.Deferred_result

  let check_command s ~verifies =
    Pvem_lwt_unix.System.Shell.execute
      (List.map ~f:Filename.quote ["dash"; "-x"; "-c"; s]
       |> String.concat ~sep:" ")
    >>= fun (out, err, exit_status) ->
    List.fold verifies ~init:(return []) ~f:(fun prev_m v ->
        prev_m >>= fun prev ->
        match v with
        | `Exits_with i ->
          let l =
            if exit_status = `Exited i
            then (true, "exited well") :: prev
            else (
              false,
              sprintf "%s:\nout:\n%s\nerr:\n%s"
                (Pvem_lwt_unix.System.Shell.status_to_string exit_status)
                out
                err
            ) :: prev
          in
          return l)
    >>= fun results ->
    List.filter ~f:(fun (t, _) -> t = false) results |> return

  let command s ~verifies = `Command (s, verifies)

  let run l =
    Pvem_lwt_unix.Deferred_list.while_sequential l ~f:(function
      | `Command (s, verifies) ->
        check_command s ~verifies
        >>= begin function
        | [] ->
          return None
        | failures ->
          return (Some (
              (sprintf "Command:\n    %s\nFailures:\n%s\n" s
                 (List.map failures ~f:(fun (_, msg) -> sprintf "* %s" msg)
                  |> String.concat ~sep:"\n"))))
        end)
    >>= fun l ->
    let failures = List.filter_opt l in
    List.iter failures ~f:(printf "%s");
    printf "\n%!";
    printf "End of tests: %d / %d failures\n%!"
      (List.length failures)
      (List.length l);
    return ()
end

module Script = struct

  module Literal = struct
    type _ t =
      | Int: int -> int t
      | String: string -> string t
      | Bool: bool -> bool t
    let to_shell: type a. a t -> string =
      function
      | Int i -> sprintf "%d" i
      | String s ->
        with_buffer begin fun str ->
            str "'";
            String.iter s ~f:(fun c ->
                Char.code c |> sprintf "%03o" |> str
              );
            str "'"
        end
      | Bool true -> "0"
      | Bool false -> "1"
  end
  type _ t =
    | Exec: string list -> unit t
    | Bool_operator: bool t * [ `And | `Or ] * bool t -> bool t
    | String_operator: string t * [ `Eq | `Neq ] * string t -> bool t
    | Not: bool t -> bool t
    | Succeed: { expr: 'a t; exit_with: int} -> bool t
    | No_op: unit t
    | If: bool t * unit t * unit t -> unit t
    | Seq: unit t list -> unit t
    | Literal: 'a Literal.t -> 'a t
    | Output_as_string: unit t -> string t
    | Write_output: {
        expr: unit t;
        stdout: string option;
        stderr: string option;
        return_value: string option;
      } -> unit t
    | Feed: string t * unit t -> unit t
    | While: {condition: bool t; body: unit t} -> unit t

  module Construct = struct
    let exec l = Exec l
    let (&&&) a b = Bool_operator (a, `And, b)
    let (|||) a b = Bool_operator (a, `Or, b)
    let (=$=) a b = String_operator (a, `Eq, b)
    let (<$>) a b = String_operator (a, `Neq, b)
    let succeed ?(exit_with = 2) expr = Succeed {expr; exit_with}
    let (~$) x = succeed x
    let nop = No_op
    let if_then_else a b c = If (a, b, c)
    let if_then a b = if_then_else a b nop
    let seq l = Seq l

    let not t = Not t

    let printf fmt =
      ksprintf (fun s -> exec ["printf"; "%s"; s]) fmt

    let file_exists p =
      exec ["test"; "-f"; p] |> succeed

    let switch: type a. (bool t * unit t) list -> default: unit t -> unit t =
      fun conds ~default ->
        List.fold_right conds ~init:default ~f:(fun (x, body) prev ->
            if_then_else x body prev)

    let write_output ?stdout ?stderr ?return_value expr =
      Write_output {expr; stdout; stderr; return_value}

    let write_stdout ~path expr = write_output expr ~stdout:path

    let literal l = Literal l
    let string s = Literal.String s |> literal
    let int s = Literal.Int s |> literal
    let bool = Literal.Bool true |> literal

    let output_as_string e = Output_as_string e

    let feed ~string e = Feed (string, e)
    let (>>) string e = feed ~string e

    let loop_while condition ~body = While {condition; body}
  end

  type output_parameters = {
    statement_separator: string;
  }
  let rec to_shell: type a. _ -> a t -> string =
    fun params e ->
      let continue e = to_shell params e in
      let seq l = String.concat  ~sep:params.statement_separator l in
      let expand_octal s =
        sprintf
          {sh| printf "$(printf '%%s' %s | sed -e 's/\(.\{3\}\)/\\\1/g')" |sh}
          s in
      (* let expand_output_to_string = *)
      (*   sprintf "\"$(%s)\"" in *)
      match e with
      | Exec l ->
        let easy_to_escape =
          function
          | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '-' | '_' | '*' | '&' | '^'
          | '=' | '+' | '%' | '$' | '"' | '\'' | '/' | '#' | '@' | '!' | ' '
          | '~' | '`' | '\\' | '|' | '?' | '>' | '<' | '.' | ',' | ':' | ';'
          | '{' | '}' 
          | '(' | ')'
          | '[' | ']'
            -> true
          | other -> false in
        let impossible_to_escape = String.exists ~f:((=) '\x00') in
        let variables = ref [] in
        let args =
          List.mapi l ~f:(fun index -> function
            | arg when String.for_all arg ~f:easy_to_escape ->
              Filename.quote arg
            | arg when impossible_to_escape arg ->
              ksprintf failwith "to_shell: sorry %S is impossible to escape as \
                                 `exec` argument" arg
            | arg ->
              let var =
                with_buffer begin fun str ->
                  ksprintf str "argument_%d=$(printf '" index;
                  String.iter arg ~f:(fun c ->
                      Char.code c |> sprintf "\\%03o" |> str
                    );
                  str "'; printf 'x') ; "
                end in
              variables := var :: !variables;
              sprintf "\"${argument_%d%%?}\"" index
              (* Literal.(to_shell (String arg)) |> expand_octal *)
              (* |> expand_output_to_string *)
            )
        in
        (List.rev !variables) @ args |> String.concat ~sep:" "
      | Succeed {expr; exit_with} ->
        seq [
          (continue expr);
          sprintf "( if [ $? -ne 0 ] ; then exit %d ; else exit 0 ; fi )"
            exit_with;
        ]
      | Bool_operator (a, op, b) ->
        sprintf "{ %s %s %s ; }"
          (continue a)
          (match op with `And -> "&&" | `Or -> "||")
          (continue b)
      | String_operator (a, op, b) ->
        sprintf "[ %s %s %s ]"
          (continue a)
          (match op with `Eq -> "=" | `Neq -> "!=")
          (continue b)
      | No_op -> "printf ''"
      | If (c, t, e) ->
        seq [
          sprintf "if { %s ; }" (continue c);
          sprintf "then %s" (continue t);
          sprintf "else %s" (continue e);
          "fi";
        ]
      | While {condition; body} ->
        seq [
          sprintf "while { %s ; }" (continue condition);
          sprintf "do %s" (continue body);
          "done"
        ]
      | Seq l -> seq (List.map l ~f:continue)
      | Not t ->
        sprintf "! { %s ; }" (continue t)
      | Write_output { expr; stdout; stderr; return_value } ->
        sprintf " ( %s %s ) %s %s"
          (continue expr)
          (Option.value_map return_value ~default:"" ~f:(fun path ->
               sprintf "; echo \"$?\" > %s" path))
          (Option.value_map stdout ~default:"" ~f:(fun path ->
               sprintf " > %s" path))
          (Option.value_map stderr ~default:"" ~f:(fun path ->
               sprintf "2> %s" path))
      | Literal lit ->
        Literal.to_shell lit
      | Output_as_string e ->
        sprintf "\"$( %s  | od -t o1 -w10000000 -An -v | tr -d \" \" )\"" (continue e)
      | Feed (string, e) ->
        sprintf {sh|  %s | %s  |sh}
          (continue string |> expand_octal) (continue e)

  let rec to_one_liner: type a. a t -> string = fun e ->
    to_shell {statement_separator = " ; "} e
  let rec to_many_lines: type a. a t -> string = fun e ->
    to_shell {statement_separator = "\n"} e

  let exits n c = [
      Test.command (to_one_liner c) [`Exits_with n];
      Test.command (to_many_lines c) [`Exits_with n];
    ]

  let tests =
    let exit n = Construct.exec ["exit"; Int.to_string n] in
    let return n =
      Construct.exec ["bash"; "-c"; sprintf "exit %d" n] in
    List.concat [
      exits 0 (Exec ["ls"]);
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
    ]
end


let posix_sh_tests = [
  Test.command "ls" [`Exits_with 0];
]



let () =
  let tests =
    posix_sh_tests
    @ Script.tests
  in
  begin match Lwt_main.run (Test.run tests) with
  | `Ok () -> printf "Done.\n%!"
  | `Error (`Shell (s, `Exn e)) ->
    eprintf "SHELL-ERROR:\n  %s\n  %s\n%!" s (Printexc.to_string e);
    exit 2
  end
