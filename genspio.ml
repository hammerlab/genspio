
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
    | If of t * t * t option
    | Seq of t list

  module Construct = struct
    let cmd = fun s -> Command s
    let cf fmt = ksprintf cmd fmt
    let (&&) a b = And (a, b)
    let (||) a b = Or (a, b)
    let seq l = Seq l

    let escape = Filename.quote

    let if_then_else a b c = If (a, b, Some c)
    let if_then a b = If (a, b, None)

    module Function = struct
      let define name body = Define_function (name, body)
      let call name args = cf "%s %s" name (List.map args ~f:escape |> String.concat ~sep:" ")
    end
  end

  let rec to_one_liner =
    function
    | Command s -> sprintf " %s " s
    | And (a, b) -> sprintf "( %s && %s )" (to_one_liner a) (to_one_liner b)
    | Or (a, b) -> sprintf "( %s || %s )" (to_one_liner a) (to_one_liner b)
    | Define_function (name, body) ->
      sprintf "%s () { %s ; }" name (to_one_liner body)
    | If (c, t, Some e) ->
      sprintf "if %s ; then %s ; else %s ; fi"
        (to_one_liner c) (to_one_liner t) (to_one_liner e)
    | If (c, t, None) ->
      sprintf "if %s ; then %s ; fi"
        (to_one_liner c) (to_one_liner t)
    | Seq l ->
      String.concat (List.map l ~f:to_one_liner) ~sep:" ; "

end

module Unique_name = struct

  let x = ref 0
  let create prefix =
    incr x;
    let now = Unix.gettimeofday () in
    sprintf "%s_%s_%d"
      prefix
      (truncate (1000. *. now) |> Int.to_string)
      !x

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

    (* val unsafe: string -> 'a t *)
    (* val unsafe_exec: string list -> 'a t *)

    (* type 'a io *)
    val exec_to_string: string list -> (int * string) t
    (* val exec_to_string_io: string list -> (int * string) io t *)

    val int: int -> int t
    val string: string -> string t

    val compare_int: [`Lt | `Le | `Eq ] -> int t -> int t -> bool t

    val pair: 'a t -> 'b t -> ('a * 'b) t
    val pair_first: ('a * 'b) t -> 'a t
    val pair_second: ('a * 'b) t -> 'b t
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

  module Defaults = struct
    let tmp_dir = "/tmp"
  end
  module To_posix_sh (Config : sig
      val tmp_dir: string
      end) (* : Semantics with type 'a observation = Posix_sh.t *) = struct

    type sh = Posix_sh.t
    module Expr = struct
      (* type 'a t = *)
      (*   | Unit: unit t *)
      (*   | Bool: sh -> bool t *)
      (*   | Int: sh -> int t *)
      (*   | String: sh -> string t *)
      type t =
        | Unit
        | Return_value of string
        | Output of string
        (* | Pair of t * t *)
        (* | String of sh *)
          (* | Lambda : string -> ('a -> 'b) t *)
    end
    open Expr
    module Script = struct
      type t = {
        (* TODO:
           need identifiers for chunks of code
           track purely functional and I/O separately?
        *)
        script: sh list;
        expr: Expr.t;
      }
    end
    open Script
    type 'a t =
      | Script: Script.t -> 'a t
      | Lambda: ('a t -> 'b t) -> ('a -> 'b) t
          (* TODO
      | Pair: 'a t * 'b t -> ('a * 'b) t
             *)

    let of_expr expr = Script {script = []; expr}

    module Sh = Posix_sh.Construct
    (* | Expr: Posix_sh.t -> 'a t *)
    (* | Int_file: Posix_sh.t * string -> int t *)
    (* | Int_literal: int -> int t *)
    (* | String_file: Posix_sh.t * string -> string t *)
    (* | String_literal: string -> string t *)
    (* | Lambda : ('a t -> 'b t) -> ('a -> 'b) t *)

    let get_script : type a . a t -> sh list =
      function
      | Lambda f -> failwith "BUG get_expression: this is a `Lambda`"
      | Script {script; _} -> script

    let get_bool : type a . a t -> string =
      function
      | Lambda f -> failwith "BUG get_bool: this is a `Lambda`"
      | Script {expr; script} ->
        let open Expr in
        begin match expr with
        | Return_value s ->  s
        | Output s  -> sprintf " [ `%s` = true ] " s
        (* | Pair _ ->failwith "BUG get_bool: this is a `Pair`" *)
        | Unit  -> failwith "BUG get_bool: this is a `Unit`"
        end

    let output_expression: type a. path: string -> a t -> sh = fun ~path ->
      function
      | Lambda f -> failwith "BUG output_expression: this is a `Lambda`"
      | Script {expr; _} ->
        let open Expr in
        let rec build =
          function
          | Return_value s ->
            Sh.if_then_else (Sh.cmd s) 
              (Sh.cf "echo true > %s" path)
              (Sh.cf "echo false > %s" path)
          | Output o  ->
            Sh.cf "%s > %s" o path
          (* | Pair (ta, tb) -> *)
          (*   Sh.seq [build ta; build tb] *)
          | Unit  ->
            Sh.cf "touch %s" path
        in
        build expr

    let is_unit =
      function
      | Script {expr = Unit; _} -> true
      | _ -> false

    (*
    let get_int_expression : type a . a t -> _ =
      let open Posix_sh.Construct in
      function
      | Int_file (ex, s) -> Some ex, cf "`cat %s`" s
      | Int_literal i -> None, cf "%d" i
      | _ -> failwith "BUG get_expression: this is not a `Expr`"
       *)

    type 'a observation = Posix_sh.t

    let (//) = Filename.concat
    let tmp_file suffix =
      Config.tmp_dir // Unique_name.create suffix


    let observe: type a. a t -> Posix_sh.t = fun t ->
      let script = get_script t in
      let tmp = tmp_file "observe_result" in
      let e = output_expression t ~path:tmp in
      Sh.(seq (script @ [e]))

    let lambda f =
      Lambda f
        (*
      let fname = Unique_name.create "lambda" in
      let vname = Unique_name.create "lambda" in
      let definition =
        Posix_sh.Construct.(
          let body = f (of_expr (String (cf "${%s}" vname))) in
          Function.define fname
            (seq [cf "local %s=$1" vname; get_expression body])
        ) in
      {script =[definition]; expr = Lambda fname}
           *)

    let apply f x =
      match f with
      | Lambda f ->
        f x
      (* {script = f.script @ x.script; *)
      (*  expr =  ppnotf x *)
      | Script e ->
        assert false

    let int i = Output (sprintf "echo %d" i) |> of_expr
    let string s = Output (sprintf "echo %s" (Filename.quote s)) |> of_expr

    let unit_command sh =
      Script {script = [sh]; expr = Unit}

    let echo s =
      unit_command Sh.(cf "echo %s" (escape s))

    let fail ?(exit = 2) s =
      unit_command Sh.(
          seq [
            cf "echo %s >&2" (escape s);
            cf "exit %d" exit;
          ]
        )

    let test condition =
      Return_value (
        sprintf "test %s" (Condition.to_unix_test_option condition)
      )
      |> of_expr
    (* type 'a observation *)
    (* val observe : (unit -> 'a t) -> 'a observation *)


    let binary op a b =
      let aexpr = get_bool a in
      let bexpr = get_bool b in
      Script {
        script = get_script a @ get_script b;
        expr = Return_value (sprintf "( %s %s %s )" aexpr op bexpr);
      }
    let (&&&) a b = binary "&&" a b
    let (|||) a b = binary "||" a b
      (* Expr Posix_sh.Construct.(get_expression a || get_expression b) *)

    (* val switch: (bool t * 'a t) list -> default: 'a t -> 'a t *)
    let switch = fun cases ~default ->
      (* let make_expr sh = *)
      (*   match default with *)
      (*   | Lambda _ -> failwith "switch: default cannot be a Lambda" *)
      (*   | Script {script; expr} -> *)
      (*     begin match expr with *)
      (*     | Unit -> Unit *)
      (*     | Return_value _ -> Return_value ( *)
      (*     | Bool _ -> Bool sh *)
      (*     | Int _ -> Int sh *)
      (*     | String _ -> String sh *)
      (*     end *)
      (* in *)
      (* let pre_script = *)
      (*   List.map cases ~f:(fun (c, b) -> get_script c) |> List.concat in *)
      let tmp_result = tmp_file "switch_result" in
      let switch_script =
        let rec chain =
          function
          | [] -> get_script default |> Sh.seq
          | (case_one, body) :: more ->
            let cond_script = get_script case_one in
            Sh.seq (
              cond_script @ [
                Sh.if_then_else
                  (get_bool case_one |> Sh.cmd)
                  Sh.(
                    let scr = get_script body in
                    let set_result =
                      if is_unit body
                      then []
                      else [
                        output_expression  body ~path:tmp_result;
                      ] in
                    seq (scr @ set_result)
                  )
                  (chain more)
              ])
        in
        chain cases
      in
      let script = [switch_script] in
      Script {
        script;
        expr =
          if is_unit default then Unit else Output (sprintf "cat %s" tmp_result);
      }
(*
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
*)
    (* let unsafe s = Expr Posix_sh.Construct.(cmd s) *)
    (* let unsafe_exec s = *)
    (*   Expr Posix_sh.Construct.( *)
    (*       cmd (List.map s ~f:escape |> String.concat ~sep:" ") *)
    (*     ) *)

    let pair a b =
      assert false
    let pair_first a =
      assert false
    let pair_second a =
      assert false

    let exec_to_string s =
      assert false
    (*
      let return_variable = tmp_file "return" in
      let value_variable = tmp_file "output" in
      let already_run = tmp_file "run_witness" in
      let run =
        Posix_sh.Construct.(
          if_then
            (cf "[ \"`cat %s`\" = \"true\" ]" already_run)
            (seq [
                cf "%s > %s"
                  (List.map s ~f:escape |> String.concat ~sep:" ")
                  value_variable;
                cf "echo \"$?\" > %s" return_variable;
                cf "echo 'true' > %s " already_run;
              ])
            )
      in
      let return = Int_file (run, return_variable) in
      let output = String_file (run, value_variable) in
      Script {
        script = Sh.[
            cf "%s > %s"
              (List.map s ~f:escape |> String.concat ~sep:" ")
              value_variable;
            cf "echo \"$?\" > %s" return_variable;
      (return, output)
*)
    let compare_int op at bt =
      (* let before_a, expr_a = get_int_expression at in *)
      (* let before_b, expr_b = get_int_expression bt in *)
      assert false
      (* : [`Lt | `Le | `Eq ] -> int t -> int t -> bool t *)

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
            else (
              false,
              sprintf "%s: %S %S"
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

let posix_sh_tests = [
  Test.command
    Posix_sh.(
      (* `( exit <n> )`, with the parens seems to behave like a command that
         returns `<n>`, as oppposed to `exit <n>` that is a “more powerfull” shell
         built-in. *)
      Construct.(cf "ls" && (cf "( exit 1 )" || cf "pwd"))
      |> to_one_liner)
    [`Exits_with 0];
  Test.command
    Posix_sh.(
      Construct.(cf "ls" && (cf "( exit 1 )" || cf "( exit 2 )"))
      |> to_one_liner)
    [`Exits_with 2];
  Test.command
    Posix_sh.(
      Construct.(cf "( exit 4 )"
                 || (Function.define "one"  (cf "( exit $1 )")
                     && Function.call "one" ["3"]))
      |> to_one_liner)
    [`Exits_with 3];
  Test.command
    Posix_sh.(
      Construct.(cf "( exit 4 )"
                 || (Function.define "one"  (cf "( exit 2 )")
                     && Function.call "one" [])
                 || (Function.define "one"  (cf "( exit 3 )")
                     && Function.call "one" []))
      |> to_one_liner)
    [`Exits_with 3];
]

let basic_edsl_tests = [
  Test.command
    begin
      let module Script(Shell : EDSL.Semantics) = struct
        open Shell
        let e =
          observe begin
            test EDSL.Condition.(file_exists "/etc/passwd")
          end
      end in
      let module Compiled = Script(EDSL.To_posix_sh(EDSL.Defaults)) in
      Compiled.e |> Posix_sh.to_one_liner
    end
    [`Exits_with 0];
  Test.command
    begin
      let module Script(Shell : EDSL.Semantics) = struct
        open Shell
        let t = test EDSL.Condition.(file_exists "/etc/passwd")
        let f = test EDSL.Condition.(file_exists "/etc/passwd-NOPE")
        let e =
          observe begin
            switch ~default:(fail ~exit:1 "default should not happen") [
              f, fail ~exit:2 "should not happen";
              f &&& t, fail ~exit:3 "should not happen either";
              t &&& f, fail ~exit:4 "should not happen either";
              f ||| t, fail ~exit:5 "should be displayed";
              f ||| t, fail ~exit:6 "should not be displayed";
            ]
          end
      end in
      let module Compiled = Script(EDSL.To_posix_sh(EDSL.Defaults)) in
      Compiled.e |> Posix_sh.to_one_liner
    end
    [`Exits_with 5];
]

let more_edsl_tests =
  let module Script(Shell : EDSL.Semantics) = struct
    open Shell
    let t = test EDSL.Condition.(file_exists "/etc/passwd")
    let f = test EDSL.Condition.(file_exists "/etc/passwdijljdiedjsdi")
    let goes_to_default =
      observe begin
        switch ~default:(fail ~exit:1 "default should happen") [
          f, fail ~exit:2 "should not happen";
          f &&& t, fail ~exit:3 "should not happen either";
          f ||| f, fail ~exit:4 "should not happen either";
        ]
      end
        (*
    let uses_exec =
      observe begin
        unsafe_exec ["bash"; "-c";
                     "export B=$(whoami) ; \
                      (echo \"$B\" | sed 's/\\(.*\\)$/You \"are\" \\1/' \
                      | grep -v are) || exit 9 \
                     "]
      end
           *)
  end in
  let module Compiled = Script(EDSL.To_posix_sh(EDSL.Defaults)) in
  let make_test compiled expectations =
    Test.command (compiled |> Posix_sh.to_one_liner) expectations in
  [
    make_test Compiled.goes_to_default [`Exits_with 1];
    (* make_test Compiled.uses_exec [`Exits_with 9]; *)
  ]


let () =
  let tests =
    posix_sh_tests
    @ basic_edsl_tests
    @ more_edsl_tests
  in
  begin match Lwt_main.run (Test.run tests) with
  | `Ok () -> printf "Done.\n%!"
  | `Error (`Shell (s, `Exn e)) ->
    eprintf "SHELL-ERROR:\n  %s\n  %s\n%!" s (Printexc.to_string e);
    exit 2
  end
