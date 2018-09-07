open Common

type c_string = C_string

type byte_array = Byte_Array

module Literal = struct
  type _ t =
    | Int : int -> int t
    | String : string -> byte_array t
    | Bool : bool -> bool t

  let pp : type a. _ -> a t -> unit =
    let open Format in
    fun fmt -> function
      | Int i -> fprintf fmt "@[(int@ %d)@]" i
      | String s -> fprintf fmt "@[(string@ %S)@]" s
      | Bool b -> fprintf fmt "@[(bool@ %b)@]" b

  module Str = struct
    let easy_to_escape s =
      String.for_all s ~f:(function
        | 'a' .. 'z'
         |'A' .. 'Z'
         |'0' .. '9'
         |'-' | '_' | '*' | '&' | '^' | '=' | '+' | '%' | '$' | '"' | '\''
         |'/' | '#' | '@' | '!' | ' ' | '~' | '`' | '\\' | '|' | '?' | '>'
         |'<' | '.' | ',' | ':' | ';' | '{' | '}' | '(' | ')' | '[' | ']' ->
            true
        | other -> false )

    let impossible_to_escape_for_variable = String.exists ~f:(( = ) '\x00')
  end
end

type raw_command_annotation = ..

type raw_command_annotation += Magic_unit

type fd_redirection =
  { take: int t
  ; redirect_to:
      [`Path of c_string t | `Fd of int t (* | `Input_of of unit t *)] }

and _ t =
  | Exec : c_string t list -> unit t
  | Raw_cmd : (raw_command_annotation option * string) -> 'a t
  | Bool_operator : bool t * [`And | `Or] * bool t -> bool t
  | String_operator : byte_array t * [`Eq | `Neq] * byte_array t -> bool t
  | Not : bool t -> bool t
  | Returns : {expr: 'a t; value: int} -> bool t
  | No_op : unit t
  | If : bool t * unit t * unit t -> unit t
  | Seq : unit t list -> unit t
  | Literal : 'a Literal.t -> 'a t
  | Output_as_string : unit t -> byte_array t
  | Redirect_output : unit t * fd_redirection list -> unit t
  | Write_output :
      { expr: unit t
      ; stdout: c_string t option
      ; stderr: c_string t option
      ; return_value: c_string t option }
      -> unit t
  | Feed : byte_array t * unit t -> unit t
  | Pipe : unit t list -> unit t
  | While : {condition: bool t; body: unit t} -> unit t
  | Fail : string -> unit t
  | Int_to_string : int t -> c_string t
  | String_to_int : c_string t -> int t
  | Bool_to_string : bool t -> c_string t
  | String_to_bool : c_string t -> bool t
  | List_to_string : 'a list t * ('a t -> byte_array t) -> byte_array t
  | String_to_list : byte_array t * (byte_array t -> 'a t) -> 'a list t
  | List : 'a t list -> 'a list t
  | C_string_concat : c_string list t -> c_string t
  | Byte_array_concat : byte_array list t -> byte_array t
  | List_append : ('a list t * 'a list t) -> 'a list t
  | List_iter : 'a list t * ((unit -> 'a t) -> unit t) -> unit t
  | Byte_array_to_c_string : byte_array t -> c_string t
  | C_string_to_byte_array : c_string t -> byte_array t
  | Int_bin_op :
      int t * [`Plus | `Minus | `Mult | `Div | `Mod] * int t
      -> int t
  | Int_bin_comparison :
      int t * [`Eq | `Ne | `Gt | `Ge | `Lt | `Le] * int t
      -> bool t
  | Getenv : c_string t -> c_string t
  (* See [man execve]. *)
  | Setenv : c_string t * c_string t -> unit t
  | Comment : string * 'a t -> 'a t

let pp_in_expr fmt pp =
  let open Format in
  pp_open_box fmt 2 ; fprintf fmt "(%a)" pp () ; pp_close_box fmt () ; ()

let pp_fun_call fmt name pp_arg args =
  let open Format in
  pp_open_box fmt 2 ;
  fprintf fmt "(%s@ %a)" name
    (pp_print_list ~pp_sep:(fun fmt () -> pp_print_space fmt ()) pp_arg)
    args ;
  pp_close_box fmt () ;
  ()

let rec pp : type a. Format.formatter -> a t -> unit =
  let open Format in
  fun fmt -> function
    | Exec l -> pp_fun_call fmt "exec" pp l
    | Raw_cmd (_, s) ->
        pp_fun_call fmt "raw-command" (fun fmt -> fprintf fmt "%S") [s]
    | Bool_operator (a, op, b) ->
        pp_fun_call fmt (match op with `And -> "and" | `Or -> "or") pp [a; b]
    | String_operator (a, op, b) ->
        pp_fun_call fmt
          (match op with `Eq -> "string-eq" | `Neq -> "string-neq")
          pp [a; b]
    | Int_bin_comparison (a, op, b) ->
        let sop =
          match op with
          | `Eq -> "int-eq"
          | `Ne -> "int-neq"
          | `Gt -> "gt"
          | `Ge -> "ge"
          | `Lt -> "lt"
          | `Le -> "le"
        in
        pp_fun_call fmt sop pp [a; b]
    | Int_bin_op (a, op, b) ->
        let sop =
          match op with
          | `Plus -> "+"
          | `Minus -> "-"
          | `Mult -> "×"
          | `Div -> "÷"
          | `Mod -> "%"
        in
        pp_fun_call fmt sop pp [a; b]
    | Not b -> pp_fun_call fmt "not" pp [b]
    | Returns {expr; value : int} ->
        pp_fun_call fmt (sprintf "returns-{%d}" value) pp [expr]
    | No_op -> fprintf fmt "(noop)"
    | If (c, t, e) ->
        pp_open_box fmt 1 ;
        fprintf fmt "(if@ %a@ then: %a@ else: %a)" pp c pp t pp e ;
        pp_close_box fmt ()
    | Seq l -> pp_fun_call fmt "seq" pp l
    | Literal l -> Literal.pp fmt l
    | Output_as_string u -> pp_fun_call fmt "as-string" pp [u]
    | Redirect_output (u, l) ->
        let redirs fmt {take; redirect_to} =
          fprintf fmt "@[(%a@ >@ %a)@]" pp take
            (fun fmt -> function `Fd f -> fprintf fmt "%a" pp f
              | `Path f -> fprintf fmt "%a" pp f )
            redirect_to
        in
        pp_in_expr fmt (fun fmt () ->
            fprintf fmt "redirect@ %a@ %a" pp u
              (pp_print_list ~pp_sep:pp_print_space redirs)
              l )
    | Write_output {expr; stdout; stderr; return_value} ->
        let o name fmt opt =
          match opt with
          | None -> ()
          | Some c -> fprintf fmt "@ @[<hov 2>(%s → %a)@]" name pp c
        in
        pp_in_expr fmt (fun fmt () ->
            fprintf fmt "write-output@ %a%a%a%a" pp expr (o "stdout") stdout
              (o "stderr") stderr (o "return-value") return_value )
    | Feed (s, u) ->
        pp_in_expr fmt (fun fmt () -> fprintf fmt "%a@ >>@ %a" pp s pp u)
    | Pipe l ->
        pp_in_expr fmt (fun fmt () ->
            fprintf fmt "pipe:@ %a"
              (pp_print_list ~pp_sep:(fun fmt () -> fprintf fmt "@ |@ ") pp)
              l )
    | While {condition; body} ->
        pp_in_expr fmt (fun fmt () ->
            fprintf fmt "while@ %a@ do:@ %a" pp condition pp body )
    | Fail s -> pp_in_expr fmt (fun fmt () -> fprintf fmt "FAIL@ %S" s)
    | Int_to_string i -> pp_fun_call fmt "int-to-string" pp [i]
    | String_to_int i -> pp_fun_call fmt "string-to-int" pp [i]
    | Bool_to_string b -> pp_fun_call fmt "bool-to-string" pp [b]
    | String_to_bool b -> pp_fun_call fmt "string-to-bool" pp [b]
    | List_to_string (l, f) -> pp_fun_call fmt "list-to-string" pp [l]
    (* : 'a list t * ('a t -> byte_array t) -> byte_array t *)
    | String_to_list (s, f) -> pp_fun_call fmt "string-to-list" pp [s]
    | List l -> pp_fun_call fmt "list" pp l
    | C_string_concat t -> pp_fun_call fmt "c-string-concat" pp [t]
    | Byte_array_concat t -> pp_fun_call fmt "byte-array-concat" pp [t]
    | List_append (la, lb) -> pp_fun_call fmt "list-append" pp [la; lb]
    | List_iter (l, f) ->
        let body = f (fun () -> Raw_cmd (None, "VARIABLE")) in
        pp_open_box fmt 1 ;
        fprintf fmt
          "(list-iter@ list: %a@ f: @[<hov 4>(fun VARIABLE ->@ %a)@])" pp l pp
          body ;
        pp_close_box fmt ()
    (* : 'a list t * ((unit -> 'a t) -> unit t) -> unit t *)
    | Byte_array_to_c_string ba ->
        pp_fun_call fmt "byte-array-to-c-string" pp [ba]
    | C_string_to_byte_array c ->
        pp_fun_call fmt "c-string-to-byte-array" pp [c]
    | Getenv s -> pp_fun_call fmt "getenv" pp [s]
    | Setenv (s, v) -> pp_fun_call fmt "setenv" pp [s; v]
    | Comment (cmt, expr) ->
        fprintf fmt "@[<hov 1>(comment@ %S@ %a)@]" cmt pp expr

module Construct = struct
  let to_c_string ba = Byte_array_to_c_string ba

  let to_byte_array c = C_string_to_byte_array c

  module C_string = struct
    let equals a b = String_operator (to_byte_array a, `Eq, to_byte_array b)

    let ( =$= ) a b = String_operator (to_byte_array a, `Eq, to_byte_array b)

    let ( <$> ) a b = String_operator (to_byte_array a, `Neq, to_byte_array b)

    let to_byte_array c = C_string_to_byte_array c

    let to_bytes c = C_string_to_byte_array c

    let concat_elist l = C_string_concat l

    let concat_list sl = concat_elist (List sl)
  end

  module Byte_array = struct
    let ( =$= ) a b = String_operator (a, `Eq, b)

    let ( <$> ) a b = String_operator (a, `Neq, b)

    let to_c_string ba = Byte_array_to_c_string ba

    let to_c ba = Byte_array_to_c_string ba
  end

  module Base = struct
    let literal l = Literal l

    let byte_array s = Literal.String s |> literal

    let int s = Literal.Int s |> literal

    let bool t = Literal.Bool t |> literal

    let c_string s = byte_array s |> to_c_string

    let string = c_string

    let exec l = Exec (List.map l ~f:(fun s -> string s))

    let call l = Exec l

    let ( &&& ) a b = Bool_operator (a, `And, b)

    let ( ||| ) a b = Bool_operator (a, `Or, b)

    let returns expr ~value = Returns {expr; value}

    let succeeds expr = returns expr ~value:0

    let nop = No_op

    let if_then_else a b c = If (a, b, c)

    let if_then a b = if_then_else a b nop

    let seq l = Seq l

    let not t = Not t

    let fail s = Fail s

    let comment s u = Comment (s, u)

    let ( %%% ) s u = comment s u

    let make_switch : type a.
        (bool t * unit t) list -> default:unit t -> unit t =
     fun conds ~default ->
      List.fold_right conds ~init:default ~f:(fun (x, body) prev ->
          if_then_else x body prev )

    let write_output ?stdout ?stderr ?return_value expr =
      Write_output {expr; stdout; stderr; return_value}

    let write_stdout ~path expr = write_output expr ~stdout:path

    let to_fd take fd = {take; redirect_to= `Fd fd}

    let to_file take file = {take; redirect_to= `Path file}

    let with_redirections cmd l = Redirect_output (cmd, l)

    let file_exists p = call [c_string "test"; c_string "-f"; p] |> succeeds

    let getenv v = Getenv v

    let setenv ~var v = Setenv (var, v)

    let get_stdout e = Output_as_string e

    let feed ~string e = Feed (string, e)

    let ( >> ) string e = feed ~string e

    let pipe l = Pipe l

    let ( ||> ) a b = Pipe [a; b]

    let loop_while condition ~body = While {condition; body}

    let loop_seq_while condition body = While {condition; body= Seq body}

    let byte_array_concat_list l = Byte_array_concat l
  end

  include Base

  module Bool = struct
    let of_string s = String_to_bool s

    let to_string b = Bool_to_string b
  end

  module Integer = struct
    let to_string i = Int_to_string i

    let to_byte_array i = C_string_to_byte_array (Int_to_string i)

    let of_string s = String_to_int s

    let of_byte_array s = String_to_int (Byte_array_to_c_string s)

    let bin_op a o b = Int_bin_op (a, o, b)

    let add a b = bin_op a `Plus b

    let ( + ) = add

    let sub a b = bin_op a `Minus b

    let ( - ) = sub

    let mul a b = bin_op a `Mult b

    let ( * ) = mul

    let div a b = bin_op a `Div b

    let ( / ) = div

    let modulo a b = bin_op a `Mod b

    let ( mod ) = modulo

    let cmp op a b = Int_bin_comparison (a, op, b)

    let eq = cmp `Eq

    let ne = cmp `Ne

    let lt = cmp `Lt

    let le = cmp `Le

    let ge = cmp `Ge

    let gt = cmp `Gt

    let ( = ) = eq

    let ( <> ) = ne

    let ( < ) = lt

    let ( <= ) = le

    let ( >= ) = ge

    let ( > ) = gt
  end

  module Magic = struct
    let unit s : unit t = Raw_cmd (Some Magic_unit, s)
  end

  module Elist = struct
    let make l = List l

    let append la lb = List_append (la, lb)

    let iter l ~f = List_iter (l, f)

    let to_string f l = List_to_string (l, f)

    let of_string f l = String_to_list (l, f)

    let serialize_byte_array_list : byte_array list t -> byte_array t =
      to_string (fun e -> e)

    let deserialize_to_byte_array_list : byte_array t -> byte_array list t =
      of_string (fun e -> e)

    let serialize_c_string_list : c_string list t -> byte_array t =
      to_string (fun e -> to_byte_array e)

    let deserialize_to_c_string_list : byte_array t -> c_string list t =
      of_string (fun e -> to_c_string e)

    let serialize_int_list : int list t -> byte_array t =
      to_string Integer.to_byte_array

    let deserialize_to_int_list : byte_array t -> int list t =
      of_string Integer.of_byte_array

    let to_string _ = `Do_not_use

    let of_string _ = `Do_not_use
  end
end
