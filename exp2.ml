
open Nonstd
module String = Sosa.Native_string
(*

   ocamlbuild -use-ocamlfind -package sosa,nonstd,pvem_lwt_unix exp2.byte && ./exp2.byte

*)

module Script = struct

  type _ t =
    | Exec: string list -> unit t
    | Bool_operator: bool t * [ `And | `Or ] * bool t -> bool t
    | Succeed: 'a t -> bool t
    | Noop: 'a t
    | If: bool t * 'a t * 'a t -> 'a t
    | Seq: unit t list -> unit t
  module Construct = struct
    let exec l = Exec l
    let (&&&) a b = Bool_operator (a, `And, b)
    let (|||) a b = Bool_operator (a, `Or, b)
    let (~$) x = Succeed x
    let nop = Noop
    let if_then_else a b c = If (a, b, c)
    let if_then a b = if_then_else a b nop
    let seq l = Seq l
  end

  let ex1 =
    Exec ["ls"]
  let ex2 = Construct.(
      ~$ ex1 &&& ~$ (seq [exec ["ls"]; exec ["bash"; "-c"; "exit 2"]]) 
    )
  let rec to_one_liner: type a. a t -> string =
    fun e ->
      match e with
      | Exec l -> List.map l ~f:Filename.quote |> String.concat ~sep:" "
      | Succeed c ->
        sprintf "%s ; if [ $? -ne 0 ] ; then echo Fail; exit 2 ; fi"
          (to_one_liner c)
      | Bool_operator (a, op, b) ->
        sprintf "( %s %s %s )"
          (to_one_liner a)
          (match op with `And -> "&&" | `Or -> "||")
          (to_one_liner b)
      | Noop -> "printf ''"
      | If (c, t, e) ->
        sprintf "if %s ; then %s ; else %s ; fi"
          (to_one_liner c) (to_one_liner t) (to_one_liner e)
      | Seq l ->
        String.concat (List.map l ~f:to_one_liner) ~sep:" ; "
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
  Test.command "ls" [`Exits_with 0];
]



let () =
  let tests =
    posix_sh_tests
  in
  begin match Lwt_main.run (Test.run tests) with
  | `Ok () -> printf "Done.\n%!"
  | `Error (`Shell (s, `Exn e)) ->
    eprintf "SHELL-ERROR:\n  %s\n  %s\n%!" s (Printexc.to_string e);
    exit 2
  end
