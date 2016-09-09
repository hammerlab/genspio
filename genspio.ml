
open Nonstd
module String = Sosa.Native_string
(*

   ocamlbuild -use-ocamlfind -package sosa,nonstd,pvem_lwt_unix genspio.byte && ./genspio.byte

*)

module Posix_sh = struct
  type t =
    | Command of string
    | And of t * t
    | Or of t * t
    | Define_function of string * t
    | If of t * t * t

  module Construct = struct
    let cmd = fun s -> Command s
    let cf fmt = ksprintf cmd fmt
    let (&&) a b = And (a, b)
    let (||) a b = Or (a, b)

    let escape = Filename.quote

    let if_then_else a b c = If (a, b, c)

    module Function = struct
      let define name body = Define_function (name, body)
      let call name args = cf "%s %s" name (List.map args ~f:escape |> String.concat ~sep:" ")
    end
  end

  let rec to_one_liner =
    function
    | Command s -> sprintf "( %s )" s
    | And (a, b) -> sprintf "( %s && %s )" (to_one_liner a) (to_one_liner b)
    | Or (a, b) -> sprintf "( %s || %s )" (to_one_liner a) (to_one_liner b)
    | Define_function (name, body) ->
      sprintf "%s () { %s ; }" name (to_one_liner body)
    | If (c, t, e) ->
      sprintf "if %s ; then %s ; else %s ; fi"
        (to_one_liner c) (to_one_liner t) (to_one_liner e)

end

module EDSL = struct

  module Condition = struct
    type t = [
      | `File_exists of string
    ]
    let file_exists path : t = `File_exists path
    let to_unix_test_option =
      function
      | `File_exists s -> sprintf "-f %s" (Filename.quote s)
  end

  module type Semantics = sig
    type 'a t  (* representation type *)
    val lambda : ('a t -> 'b t) -> ('a -> 'b) t
    val apply : ('a -> 'b) t -> 'a t -> 'b t

    type 'a observation
    val observe : 'a t -> 'a observation

    val echo : string -> unit t

    val fail: ?exit : int -> string -> 'a t

    val test : Condition.t -> bool t

    val (&&&) : bool t -> bool t -> bool t

    val (|||) : bool t -> bool t -> bool t

    val switch: (bool t * 'a t) list -> default: 'a t -> 'a t

    val unsafe: string -> 'a t
    val unsafe_exec: string list -> 'a t
    (*
    val list: ('a t) list -> 'a list t
    val list_map: ('a list t) -> f:('a -> 'b) t -> ('b list t)

    val pair: 'a t -> 'b t -> ('a * 'b) t
    val pair_first: ('a * 'b) t -> 'a t
    val pair_second: ('a * 'b) t -> 'b t


       let f1 =
         lambda (fun arg1 ->
            lambda (fun arg2 ->
                  let id = int_of_file arg2 in
                  write_file (int_to_string (id + int 42))
            )
       ) in
       f1 <| (temp_file ".txt") <| (get_pid () |> int_to_string |> write_file (path "/tmp/pid"))

      *)
  end

  module To_posix_sh : Semantics with type 'a observation = Posix_sh.t = struct
    type 'a t =
      | Expr: Posix_sh.t -> 'a t
      | Lambda : ('a t -> 'b t) -> ('a -> 'b) t

    type 'a observation = Posix_sh.t

    let observe: type a. a t -> Posix_sh.t =
      function
      | Expr e -> e
      | Lambda f ->
        failwith "cannot observe lambdas for now"

    let lambda f = Lambda f

    let apply f x =
      match f with
      | Lambda f -> f x
      | Expr e ->
        assert false

    let echo s = Expr Posix_sh.Construct.(cf "echo %s" (escape s))

    let fail ?(exit = 2) s =
      Expr Posix_sh.Construct.(
          cf "echo %s >&2" (escape s)
          && cf "exit %d" exit
        )

    let test condition =
      Expr Posix_sh.Construct.(
          cf "test %s" (Condition.to_unix_test_option condition)
        )
      (* type 'a observation *)
      (* val observe : (unit -> 'a t) -> 'a observation *)

    let get_expression : type a . a t -> Posix_sh.t =
      function
      | Expr s -> s
      | Lambda f -> failwith "BUG get_expression: this is a `Lambda`"

    let (&&&) a b =
      Expr Posix_sh.Construct.(get_expression a && get_expression b)
    let (|||) a b =
      Expr Posix_sh.Construct.(get_expression a || get_expression b)

    let switch cases ~default =
      Expr Posix_sh.Construct.(
          let rec chain =
            function
            | [] -> get_expression default
            | (case_one, body) :: more ->
              if_then_else
                (get_expression case_one)
                (get_expression body)
                (chain more)
          in
          chain cases
        )

    let unsafe s = Expr Posix_sh.Construct.(cmd s)
    let unsafe_exec s =
      Expr Posix_sh.Construct.(
          cmd (List.map s ~f:escape |> String.concat ~sep:" ")
        )

    (*
    val list: ('a t) list -> 'a list t
    val list_map: ('a list t) -> f:('a -> 'b) t -> ('b list t)

    val pair: 'a t -> 'b t -> ('a * 'b) t
    val pair_first: ('a * 'b) t -> 'a t
    val pair_second: ('a * 'b) t -> 'b t
*)
  end

end

module Test = struct
  open Pvem_lwt_unix.Deferred_result

  let check_command s ~verifies =
    Pvem_lwt_unix.System.Shell.execute s
    >>= fun (out, err, exit_status) ->
    List.fold verifies ~init:(return []) ~f:(fun prev_m v ->
        prev_m >>= fun prev ->
        match v with
        | `Exits_with i ->
          let l =
            if exit_status = `Exited i
            then (true, "exited well") :: prev
            else (false,
                  Pvem_lwt_unix.System.Shell.status_to_string exit_status) :: prev
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
        | [] -> return (sprintf "Test OK: %s\n" s)
        | failures ->
          return (sprintf "Command:\n    %s\nFailures:\n%S\n" s
                    (List.map failures ~f:(fun (_, msg) -> sprintf "* %s" msg)
                     |> String.concat ~sep:"\n"))
        end)
    >>= fun l ->
    List.iter l ~f:(printf "%s");
    printf "\n%!";
    return ()
end

let () =
  let tests = [
    Test.command
      Posix_sh.(
        Construct.(cf "ls" && (cf "exit 1" || cf "pwd"))
        |> to_one_liner)
      [`Exits_with 0];
    Test.command
      Posix_sh.(
        Construct.(cf "ls" && (cf "exit 1" || cf "exit 2"))
        |> to_one_liner)
      [`Exits_with 2];
    Test.command
      Posix_sh.(
        Construct.(cf "exit 4"
                   || (Function.define "one"  (cf "exit $1")
                       && Function.call "one" ["3"]))
        |> to_one_liner)
      [`Exits_with 3];
    Test.command
      Posix_sh.(
        Construct.(cf "exit 4"
                   || (Function.define "one"  (cf "exit 2")
                       && Function.call "one" [])
                   || (Function.define "one"  (cf "exit 3")
                       && Function.call "one" []))
        |> to_one_liner)
      [`Exits_with 3];
    Test.command
      begin
        let module Script(Shell : EDSL.Semantics) = struct
          open Shell
          let e =
            observe begin
              test EDSL.Condition.(file_exists "/etc/passwd")
            end
        end in
        let module Compiled = Script(EDSL.To_posix_sh) in
        Compiled.e |> Posix_sh.to_one_liner
      end
      [`Exits_with 0];
    Test.command
      begin
        let module Script(Shell : EDSL.Semantics) = struct
          open Shell
          let t = test EDSL.Condition.(file_exists "/etc/passwd")
          let f = test EDSL.Condition.(file_exists "/etc/passwdijljdiedjsdi")
          let e =
            observe begin
              switch ~default:(fail ~exit:1 "default should not happen") [
                f, fail ~exit:2 "should not happen";
                f &&& t, fail ~exit:3 "should not happen either";
                t &&& f, fail ~exit:4 "should not happen either";
                f ||| t, fail ~exit:5 "should be displayed";
              ]
            end
        end in
        let module Compiled = Script(EDSL.To_posix_sh) in
        Compiled.e |> Posix_sh.to_one_liner
      end
      [`Exits_with 5];
  ] in
  begin match Lwt_main.run (Test.run tests) with
  | `Ok () -> printf "Done.\n%!"
  | `Error (`Shell (s, `Exn e)) ->
    eprintf "SHELL-ERROR:\n  %s\n  %s\n%!" s (Printexc.to_string e);
    exit 2
  end
