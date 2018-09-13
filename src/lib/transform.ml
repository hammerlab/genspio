(*md

This module implements basic AST (type-preserving) transformations.
  
- The class `Visitor.nothing_doer` is an “open-recursion” AST visitor
  (cf.
  [chapter 12](https://v1.realworldocaml.org/v1/en/html/classes.html#open-recursion)
  of RWO).
- The module `Constant_propagation` implements many constant
  propagation transformations using the visitor.

*)
open Common

module Visitor = struct
  open Language

  class nothing_doer ?(trace : Format.formatter option) () =
    object (self)
      method exec : c_string t list -> unit t =
        fun e -> Exec (List.map ~f:self#expression e)

      method raw_cmd : type a. raw_command_annotation option * string -> a t =
        fun e -> Raw_cmd e

      method bool_operator : bool t * [`And | `Or] * bool t -> bool t =
        fun (a, op, b) ->
          Bool_operator (self#expression a, op, self#expression b)

      method string_operator
          : byte_array t * [`Eq | `Neq] * byte_array t -> bool t =
        fun (a, op, b) ->
          String_operator (self#expression a, op, self#expression b)

      method not : bool t -> bool t = fun e -> Not (self#expression e)

      method returns : type a. expr:a t -> value:int -> bool t =
        fun ~expr ~value -> Returns {expr= self#expression expr; value}

      method no_op : unit t = No_op

      method if_ : bool t * unit t * unit t -> unit t =
        fun (a, t, e) ->
          If (self#expression a, self#expression t, self#expression e)

      method seq : unit t list -> unit t =
        fun e -> Seq (List.map ~f:self#expression e)

      method literal : type a. a Literal.t -> a t = fun e -> Literal e

      method output_as_string : unit t -> byte_array t =
        fun e -> Output_as_string (self#expression e)

      method redirect_output : unit t * fd_redirection list -> unit t =
        fun (u, l) -> Redirect_output (self#expression u, l)

      method write_output
          :    expr:unit t
            -> stdout:c_string t option
            -> stderr:c_string t option
            -> return_value:c_string t option
            -> unit t =
        fun ~expr ~stdout ~stderr ~return_value ->
          let opt f o = Option.map ~f o in
          Write_output
            { expr= self#expression expr
            ; stdout= opt self#expression stdout
            ; stderr= opt self#expression stderr
            ; return_value= opt self#expression return_value }

      method feed : byte_array t * unit t -> unit t =
        fun (s, u) -> Feed (self#expression s, self#expression u)

      method pipe : unit t list -> unit t =
        fun e -> Pipe (List.map ~f:self#expression e)

      method while_ : condition:bool t -> body:unit t -> unit t =
        fun ~condition ~body ->
          While
            {condition= self#expression condition; body= self#expression body}

      method fail : string -> unit t = fun e -> Fail e

      method int_to_string : int t -> c_string t =
        fun e -> Int_to_string (self#expression e)

      method string_to_int : c_string t -> int t =
        fun e -> String_to_int (self#expression e)

      method bool_to_string : bool t -> c_string t =
        fun e -> Bool_to_string (self#expression e)

      method string_to_bool : c_string t -> bool t =
        fun e -> String_to_bool (self#expression e)

      method list_to_string : type a.
          a list t * (a t -> byte_array t) -> byte_array t =
        fun (l, f) -> List_to_string (self#expression l, f)

      method string_to_list : type a.
          byte_array t * (byte_array t -> a t) -> a list t =
        fun (l, f) -> String_to_list (self#expression l, f)

      method list : type a. a t list -> a list t =
        fun e -> List (List.map ~f:self#expression e)

      method c_string_concat : c_string list t -> c_string t =
        fun e -> C_string_concat (self#expression e)

      method byte_array_concat : byte_array list t -> byte_array t =
        fun e -> Byte_array_concat (self#expression e)

      method list_append : type a. a list t * a list t -> a list t =
        fun (a, b) -> List_append (self#expression a, self#expression b)

      method list_iter : type a. a list t * ((unit -> a t) -> unit t) -> unit t
          =
        fun (l, f) ->
          let newf (* : type a. (unit -> a t) -> unit t *) item =
            self#expression (f item)
          in
          List_iter (self#expression l, newf)

      method byte_array_to_c_string : byte_array t -> c_string t =
        fun e -> Byte_array_to_c_string (self#expression e)

      method c_string_to_byte_array : c_string t -> byte_array t =
        fun e -> C_string_to_byte_array (self#expression e)

      method int_bin_op
          : int t * [`Plus | `Minus | `Mult | `Div | `Mod] * int t -> int t =
        fun (a, op, b) -> Int_bin_op (self#expression a, op, self#expression b)

      method int_bin_comparison
          : int t * [`Eq | `Ne | `Gt | `Ge | `Lt | `Le] * int t -> bool t =
        fun (a, op, b) ->
          Int_bin_comparison (self#expression a, op, self#expression b)

      method getenv : c_string t -> c_string t =
        fun e -> Getenv (self#expression e)

      method setenv : c_string t * c_string t -> unit t =
        fun (k, v) -> Setenv (self#expression k, self#expression v)

      method comment : type a. string * a t -> a t =
        fun (c, e) -> Comment (c, self#expression e)

      method expression : type a. a Language.t -> a Language.t =
        fun e ->
          Option.iter trace ~f:(fun formatter ->
              Format.fprintf formatter "-> %a\n" pp e ) ;
          match e with
          | Exec l -> self#exec (List.map l ~f:self#expression)
          | Raw_cmd (x, y) -> self#raw_cmd (x, y)
          | Bool_operator (x, y, z) -> self#bool_operator (x, y, z)
          | String_operator (x, y, z) -> self#string_operator (x, y, z)
          | Not x -> self#not x
          | Returns {expr; value} -> self#returns ~expr ~value
          | No_op -> self#no_op
          | If (x, y, z) -> self#if_ (x, y, z)
          | Seq x -> self#seq x
          | Literal x -> self#literal x
          | Output_as_string x -> self#output_as_string x
          | Redirect_output (x, y) -> self#redirect_output (x, y)
          | Write_output {expr; stdout; stderr; return_value} ->
              self#write_output ~expr ~stdout ~stderr ~return_value
          | Feed (x, y) -> self#feed (x, y)
          | Pipe x -> self#pipe x
          | While {condition; body} -> self#while_ ~condition ~body
          | Fail x -> self#fail x
          | Int_to_string x -> self#int_to_string x
          | String_to_int x -> self#string_to_int x
          | Bool_to_string x -> self#bool_to_string x
          | String_to_bool x -> self#string_to_bool x
          | List_to_string (x, y) -> self#list_to_string (x, y)
          | String_to_list (x, y) -> self#string_to_list (x, y)
          | List x -> self#list x
          | C_string_concat x -> self#c_string_concat x
          | Byte_array_concat x -> self#byte_array_concat x
          | List_append x -> self#list_append x
          | List_iter (x, y) -> self#list_iter (x, y)
          | Byte_array_to_c_string x -> self#byte_array_to_c_string x
          | C_string_to_byte_array x -> self#c_string_to_byte_array x
          | Int_bin_op (x, y, z) -> self#int_bin_op (x, y, z)
          | Int_bin_comparison (x, y, z) -> self#int_bin_comparison (x, y, z)
          | Getenv x -> self#getenv x
          | Setenv (x, y) -> self#setenv (x, y)
          | Comment (x, y) -> self#comment (x, y)
    end
end

module Constant_propagation = struct
  open Language

  (*md

The `propagator` class inherits from `Visitor.nothing_doer` and overwrites only the constructs that matter.

 *)
  class propagator ?trace () =
    object (self)
      inherit Visitor.nothing_doer ?trace () as super

      (*md
Boolean operators are not commutative, the left side has to be
evaluated first and may break the execution flow (e.g. with `fail`).
*)
      method bool_operator (a, op, b) =
        let ga = self#expression a in
        let gb = self#expression b in
        match (ga, op, gb) with
        | Literal (Literal.Bool true), `And, b -> b
        | Literal (Literal.Bool false), `Or, b -> b
        | _ -> Bool_operator (ga, op, gb)

      (*md
We can only know how to simplify expressions when they are about
literals (all non-literals can be non-deterministic and
side-effectful).

 *)
      method string_operator (a, op, b) =
        let ga = self#expression a in
        let gb = self#expression b in
        match (ga, op, gb) with
        | Literal (Literal.String sa), op, Literal (Literal.String sb) ->
            Literal
              ( match op with
              | `Neq -> Literal.Bool (sa <> sb)
              | `Eq -> Literal.Bool (sa = sb) )
        | _ -> String_operator (ga, op, gb)

      method returns : type a. expr:a t -> _ =
        fun ~expr ~value ->
          let e = self#expression expr in
          match (e, value) with
          | No_op, 0 -> Construct.bool true
          | No_op, _ -> Construct.bool false
          | _ -> Returns {expr; value}

      method if_ (c, t, e) =
        let gc = self#expression c in
        let gt = self#expression t in
        let ge = self#expression e in
        match gc with
        | Literal (Literal.Bool true) -> gt
        | Literal (Literal.Bool false) -> ge
        | _ -> If (gc, gt, ge)

      method while_ ~condition ~body =
        match self#expression condition with
        | Literal (Literal.Bool false) -> No_op
        | cond -> While {condition= cond; body= self#expression body}

      method not b =
        let gb = self#expression b in
        match gb with
        | Literal (Literal.Bool b) -> Literal (Literal.Bool (not b))
        | other -> Not other

      method seq l =
        let transformed =
          List.map ~f:self#expression l |> List.filter ~f:(( <> ) No_op)
        in
        match transformed with [] -> No_op | [one] -> one | l -> Seq l

      method pipe l =
        let tr = List.map ~f:self#expression l in
        match tr with Pipe l :: more -> Pipe (l @ more) | other -> Pipe other

      method c_string_concat l =
        let gl = self#expression l in
        match gl with
        | List [] -> Construct.c_string ""
        | List more -> (
            let build =
              List.fold more ~init:[] ~f:(fun prev item ->
                  match (prev, item) with
                  | [], _ -> [item]
                  | _, Byte_array_to_c_string (Literal (Literal.String "")) ->
                      prev
                  | ( Byte_array_to_c_string (Literal (Literal.String pstring))
                      :: more
                    , Byte_array_to_c_string (Literal (Literal.String sitem)) )
                    ->
                      Byte_array_to_c_string
                        (Literal (Literal.String (pstring ^ sitem)))
                      :: more
                  | _, _ -> item :: prev )
              |> List.rev
            in
            match build with
            | [one] -> one
            | more -> C_string_concat (List more) )
        | default -> C_string_concat default

      method byte_array_concat l =
        let gl = self#expression l in
        match gl with
        | List [] -> Construct.byte_array ""
        | List more -> (
            let build =
              List.fold more ~init:[] ~f:(fun prev item ->
                  match (prev, item) with
                  | [], _ -> [item]
                  | _, Literal (Literal.String "") -> prev
                  | ( Literal (Literal.String pstring) :: more
                    , Literal (Literal.String sitem) ) ->
                      Literal (Literal.String (pstring ^ sitem)) :: more
                  | _, _ -> item :: prev )
              |> List.rev
            in
            match build with
            | [one] -> one
            | more -> Byte_array_concat (List more) )
        | default -> Byte_array_concat default

      method list_append (a, b) =
        let la = self#expression a in
        let lb = self#expression b in
        match (la, lb) with
        | List [], _ -> lb
        | _, List [] -> la
        | List lla, List llb -> List (lla @ llb)
        | _, _ -> List_append (la, lb)

      method list_iter (l, f) =
        let gl = self#expression l in
        match gl with List [] -> No_op | _ -> List_iter (gl, f)

      method int_bin_op (a, op, b) =
        let ga = self#expression a in
        let gb = self#expression b in
        let default = Int_bin_op (ga, op, gb) in
        let lit n = Literal (Literal.Int n) in
        match (ga, op, gb) with
        (* Any non-literal may have side effects that we cannot eliminate.
           Most operations cannot be ocamlized because of unknown semantics.*)
        | Literal (Literal.Int na), op, Literal (Literal.Int nb) -> (
          match (na, op, nb) with
          | 0, `Plus, _ -> lit nb
          | 0, (`Mult | `Div | `Mod), _ -> lit 0
          | _, `Plus, 0 -> lit na
          | _, `Mult, 0 -> lit 0
          | 1, `Mult, _ -> lit nb
          | _, `Mult, 1 -> lit na
          | _ -> default )
        | _ -> default

      method int_bin_comparison (a, op, b) =
        let ga = self#expression a in
        let gb = self#expression b in
        let default = Int_bin_comparison (ga, op, gb) in
        match (ga, op, gb) with
        | Literal (Literal.Int na), op, Literal (Literal.Int nb) ->
            Literal
              (Literal.Bool
                 (( match op with
                  | `Ge -> ( >= )
                  | `Lt -> ( < )
                  | `Eq -> ( = )
                  | `Le -> ( <= )
                  | `Gt -> ( > )
                  | `Ne -> ( <> ) )
                    na nb))
        | _ -> default
    end

  let process ?trace e =
    let p = new propagator ?trace () in
    p#expression e

  type forget = Forget : 'a t -> forget

  let test () =
    let failures = ref [] in
    let count = ref 0 in
    let check ?trace name e res =
      let p = process ?trace e in
      incr count ;
      match p = res with
      | true -> ()
      | false ->
          failures :=
            (!count, name, Forget e, Forget res, Forget p) :: !failures
    in
    check "no-op" No_op No_op ;
    check "some bool"
      Construct.(bool true &&& bool false)
      Construct.(bool false) ;
    check "some bool"
      Construct.(bool false ||| bool true)
      Construct.(bool true) ;
    check "some bool and string"
      Construct.(
        if_then_else
          (not
             ( bool false
             ||| Byte_array.(byte_array "bouh" =$= byte_array "bah") ))
          (fail "then") (fail "else"))
      Construct.(fail "then") ;
    check "seq []" Construct.(seq []) Construct.(nop) ;
    check "seq [nops]" Construct.(seq [seq []; seq [seq []]]) Construct.(nop) ;
    check "seq [one-thing]"
      Construct.(seq [seq [nop]; seq [seq [fail "bouh"]]])
      Construct.(fail "bouh") ;
    let e n = Construct.exec [Int.to_string n] in
    check "pipes"
      Construct.(e 1 ||> e 2 ||> e 3 ||> e 4)
      Construct.(pipe [e 1; e 2; e 3; e 4]) ;
    check "concat one-two"
      Construct.(C_string.concat_list [string "one"; string "-"; string "two"])
      Construct.(string "one-two") ;
    let s n =
      Construct.(get_stdout (exec [Int.to_string n]) |> Byte_array.to_c)
    in
    check "concat one-two"
      Construct.(
        C_string.concat_list
          [ string "before"
          ; s 0
          ; string "one"
          ; string "-"
          ; string "two"
          ; s 1
          ; string "" ])
      Construct.(
        C_string.concat_list [string "before"; s 0; string "one-two"; s 1]) ;
    check "list-append"
      Construct.(
        Elist.append
          (Elist.append (Elist.make []) (Elist.make [s 0; s 1]))
          (Elist.make [s 2]))
      Construct.(Elist.make [s 0; s 1; s 2]) ;
    let make_deep expr =
      Construct.(
        seq
          [ e 42
          ; loop_seq_while
              ("Comment on the success" %%% succeeds (s 0))
              [e 1; "Comment on the `setenv`" %%% setenv (string "bouh") expr]
          ])
    in
    check "deep1"
      Construct.(make_deep Integer.(to_string (int 1 + int 0)))
      Construct.(make_deep Integer.(to_string (int 1))) ;
    check "deep int comparison"
      Construct.(make_deep Integer.(int 42 > int 44 |> Bool.to_string))
      Construct.(make_deep (bool false |> Bool.to_string)) ;
    match !failures with
    | [] -> ()
    | more ->
        List.iter (List.rev more)
          ~f:(fun (nth, name, Forget e, Forget res, Forget p) ->
            let open Format in
            eprintf
              "## Test %d failure %s:\n\
               Input:\n\
               %a\n\
               Expected:\n\
               %a\n\
               Result:\n\
               %a\n\
               %!"
              nth name pp e pp res pp p ) ;
        let nb = List.length more in
        ksprintf failwith "There %s %d test failure%s"
          (if nb > 1 then "were" else "was")
          nb
          (if nb > 1 then "s" else "")
end
