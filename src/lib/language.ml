
open Common

type c_string = C_string
type byte_array = Byte_Array

module Literal = struct
  type _ t =
    | Int: int -> int t
    | String: string -> byte_array t
    | Bool: bool -> bool t

  (** Compile a literal to the internal representation. *)
  let to_shell: type a. a t -> string =
    function
    | Int i -> sprintf "%d" i
    | String s ->
      with_buffer begin fun str ->
        String.iter s ~f:(fun c ->
            Char.code c |> sprintf "%03o" |> str
          );
      end |> fst
    | Bool true -> "true"
    | Bool false -> "false"

  module String = struct

    let easy_to_escape s =
      String.for_all s
        ~f:(function
          | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '-' | '_' | '*' | '&' | '^'
          | '=' | '+' | '%' | '$' | '"' | '\'' | '/' | '#' | '@' | '!' | ' '
          | '~' | '`' | '\\' | '|' | '?' | '>' | '<' | '.' | ',' | ':' | ';'
          | '{' | '}' | '(' | ')' | '[' | ']' -> true
          | other -> false)

    let impossible_to_escape_for_variable = String.exists ~f:((=) '\x00')

  end

end

type fd_redirection = {
  take: int t;
  redirect_to: [ `Path of c_string t | `Fd of int t (* | `Input_of of unit t *) ];
}
and _ t =
  | Exec: c_string t list -> unit t
  | Raw_cmd: string -> 'a t
  | Bool_operator: bool t * [ `And | `Or ] * bool t -> bool t
  | String_operator: byte_array t * [ `Eq | `Neq ] * byte_array t -> bool t
  | Not: bool t -> bool t
  | Returns: {expr: 'a t; value: int} -> bool t
  | No_op: unit t
  | If: bool t * unit t * unit t -> unit t
  | Seq: unit t list -> unit t
  | Literal: 'a Literal.t -> 'a t
  | Output_as_string: unit t -> byte_array t
  | Redirect_output: unit t * fd_redirection list -> unit t
  | Write_output: {
      expr: unit t;
      stdout: c_string t option;
      stderr: c_string t option;
      return_value: c_string t option;
    } -> unit t
  | Feed: byte_array t * unit t -> unit t
  | Pipe: unit t list -> unit t
  | While: {condition: bool t; body: unit t} -> unit t
  | Fail: unit t
  | Int_to_string: int t -> c_string t
  | String_to_int: c_string t -> int t
  | Bool_to_string: bool t -> c_string t
  | String_to_bool: c_string t -> bool t
  | List_to_string: 'a list t * ('a t -> byte_array t) -> byte_array t
  | String_to_list: byte_array t * (byte_array t -> 'a t) -> 'a list t
  | List: 'a t list -> 'a list t
  | C_string_concat: c_string list t -> c_string t
  | Byte_array_concat: byte_array list t -> byte_array t
  | List_append: ('a list t * 'a list t) -> 'a list t
  | List_iter: 'a list t * ((unit -> 'a t) -> unit t) -> unit t
  | Byte_array_to_c_string: byte_array t -> c_string t
  | C_string_to_byte_array: c_string t -> byte_array t
  | Int_bin_op:
      int t * [ `Plus | `Minus | `Mult | `Div | `Mod ] * int t -> int t
  | Int_bin_comparison:
      int t * [ `Eq | `Ne | `Gt | `Ge | `Lt | `Le ] * int t -> bool t
  | Getenv: c_string t -> c_string t (* See [man execve]. *)
  | Setenv: c_string t * c_string t -> unit t
  | With_signal: {
      signal_name: string;
      catch: unit t;
      run: unit t -> unit t;
    } -> unit t

module Construct = struct
  let to_c_string ba = Byte_array_to_c_string ba
  let to_byte_array c = C_string_to_byte_array c

  let literal l = Literal l
  let byte_array s = Literal.String s |> literal
  let int s = Literal.Int s |> literal
  let bool t = Literal.Bool t |> literal
  let c_string s = byte_array s |> to_c_string
  let string = c_string

  let exec l = Exec (List.map l ~f:(fun s -> string s))
  let call l = Exec l
  let (&&&) a b = Bool_operator (a, `And, b)
  let (|||) a b = Bool_operator (a, `Or, b)
  let (=$=) a b = String_operator (to_byte_array a, `Eq, to_byte_array b)
  let (<$>) a b = String_operator (to_byte_array a, `Neq, to_byte_array b)
  module Byte_array = struct
    let (=$=) a b = String_operator (a, `Eq, b)
    let (<$>) a b = String_operator (a, `Neq, b)
    let of_c s = to_byte_array s
  end

  let returns expr ~value = Returns {expr; value}

  let succeeds expr = returns expr ~value:0

  let nop = No_op
  let if_then_else a b c = If (a, b, c)
  let if_then a b = if_then_else a b nop
  let seq l = Seq l

  let not t = Not t

  let with_signal ?(signal_name = "USR2") ~catch run =
    With_signal {signal_name; catch; run}

  let fail = Fail

  let make_switch: type a. (bool t * unit t) list -> default: unit t -> unit t =
    fun conds ~default ->
      List.fold_right conds ~init:default ~f:(fun (x, body) prev ->
          if_then_else x body prev)

  let write_output ?stdout ?stderr ?return_value expr =
    Write_output {expr; stdout; stderr; return_value}

  let write_stdout ~path expr = write_output expr ~stdout:path

  let to_fd take fd = { take; redirect_to = `Fd fd }
  let to_file take file = { take; redirect_to = `Path file }
  let with_redirections cmd l =
    Redirect_output (cmd, l)

  let file_exists p =
    call [c_string "test"; c_string "-f"; p] |> succeeds

  let getenv v = Getenv v
  let setenv ~var v = Setenv (var, v)

  let output_as_string e = Output_as_string e

  let feed ~string e = Feed (string, e)
  let (>>) string e = feed ~string e

  let pipe l = Pipe l
  let (||>) a b = Pipe [a; b]

  let loop_while condition ~body = While {condition; body}

  let list l = List l

  let string_concat_list l = C_string_concat l
  let byte_array_concat_list l = Byte_array_concat l

  let list_append la lb = List_append (la, lb)

  let list_iter l ~f = List_iter (l, f)

  let list_to_string l ~f = List_to_string (l, f)
  let list_of_string l ~f = String_to_list (l, f)

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
    let (+) = add
    let sub a b = bin_op a `Minus b
    let (-) = sub
    let mul a b = bin_op a `Mult b
    let ( * ) = mul
    let div a b = bin_op a `Div b
    let (/) = div
    let modulo a b = bin_op a `Mod b
    let (mod) = modulo
    let cmp op a b = Int_bin_comparison (a, op, b)
    let eq = cmp `Eq
    let ne = cmp `Ne
    let lt = cmp `Lt
    let le = cmp `Le
    let ge = cmp `Ge
    let gt = cmp `Gt
    let (=) = eq
    let (<>) = ne
    let (<) = lt
    let (<=) = le
    let (>=) = ge
    let (>) = gt
  end

  module Magic = struct
    let unit s : unit t = Raw_cmd s
  end

end

type output_parameters = {
  statement_separator: string;
  die_command: (string -> string) option;
}
let rec to_shell: type a. _ -> a t -> string =
  fun params e ->
    let continue e = to_shell params e in
    let seq =
      function
      | [] -> ":"
      | l -> String.concat  ~sep:params.statement_separator l in
    let die s =
      match params.die_command with
      | Some f -> f s
      | None ->
        ksprintf failwith
          "Die command not set: you cannot use the `fail` construct \
           together with the `~no_trap:true` option (error message was: %S)" s
    in
    let expand_octal s =
      sprintf
        {sh| printf -- "$(printf -- '%%s' %s | sed -e 's/\(.\{3\}\)/\\\1/g')" |sh}
        s in
    let to_argument varprefix =
      let argument ?declaration ?variable_name argument =
        object
          method declaration = declaration
          method export = Option.map ~f:(sprintf "export %s ; ") declaration
          method variable_name = variable_name
          method argument = argument
        end in
      function
      | `C_string (c_str : c_string t) ->
        begin match c_str with
        | Byte_array_to_c_string (Literal (Literal.String s))
          when Literal.String.easy_to_escape s ->
          argument (Filename.quote s)
        | Byte_array_to_c_string (Literal (Literal.String s))
          when Literal.String.impossible_to_escape_for_variable s ->
          ksprintf failwith "to_shell: sorry literal %S is impossible to \
                             escape as `exec` argument" s
        | other ->
          let variable_name = Unique_name.variable varprefix in
          let declaration =
            sprintf "%s=$(%s; printf 'x')"
              variable_name (continue other |> expand_octal)
          in
          argument ~variable_name ~declaration
            (sprintf "\"${%s%%?}\"" variable_name)
        end
      | `Int (Literal (Literal.Int s)) -> argument (Int.to_string s)
      | `Int other ->
        let variable_name = Unique_name.variable varprefix in
        let declaration = sprintf "%s=%s" variable_name (continue other) in
        argument ~variable_name ~declaration
          (sprintf "\"${%s%%?}\"" variable_name)
    in
    match e with
    | Exec l ->
      let variables = ref [] in
      let args =
        List.mapi l ~f:(fun index v ->
            let varname = sprintf "argument_%d" index in
            let arg = to_argument varname (`C_string v) in
            match arg#declaration with
            | None -> arg#argument
            | Some vardef ->
              variables := sprintf "%s ; " vardef :: !variables;
              arg#argument) in
      (List.rev !variables) @ args
      |> String.concat ~sep:" "
      |> sprintf " { %s ; } "
    | Raw_cmd s -> s
    | Byte_array_to_c_string ba ->
      let bac = continue ba in
      let var =  Unique_name.variable "byte_array_to_c_string" in
      let value = sprintf "\"$%s\"" var in
      (* We store the internal octal representation in a variable, then
         we use `sed` to check that there are no `'\000'` characters.
         If OK we re-export with printf, if not we fail hard. *)
      sprintf "$(%s)" @@ seq [
        sprintf "export %s=%s" var bac;
        sprintf
          {sh|if [ "$(printf -- %s | sed -e 's/\(000\)\|\(.\{3\}\)/\1/g')" = "" ]|sh}
          value;
        sprintf "then printf -- %s" value;
        sprintf "else %s"
          (die (sprintf "Byte_array_to_c_string: error, $%s is not a C string"
                  var));
        "fi"
      ]
    | C_string_to_byte_array c -> continue c
    | Returns {expr; value} ->
      sprintf " { %s ; [ $? -eq %d ] ; }" (continue expr) value
    | Bool_operator (a, op, b) ->
      sprintf "{ %s %s %s ; }"
        (continue a)
        (match op with `And -> "&&" | `Or -> "||")
        (continue b)
    | String_operator (a, op, b) ->
      sprintf "[ \"%s\" %s \"%s\" ]"
        (continue a)
        (match op with `Eq -> "=" | `Neq -> "!=")
        (continue b)
    | No_op -> ":"
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
    | Redirect_output (unit_t, redirections) ->
      (*
         We're here compiling the redirections into `exec` statements which
         set up global redirections; we limit their scope with `( .. )`.
         E.g.
         (  exec 3>/tmp/output-of-ls ; exec 2>&3 ; exec 1>&2 ; ls ; ) ;
      *)
      let make_redirection { take; redirect_to } =
        let takearg = to_argument "redirection_take" (`Int take) in
        let retoarg =
          to_argument "redirection_to"
            (match redirect_to with `Fd i -> `Int i | `Path p -> `C_string p) in
        let variables =
          takearg#export :: retoarg#export :: [] |> List.filter_opt in
        let exec =
          sprintf "\"exec %%s>%s%%s\" %s %s"
            (match redirect_to with `Fd _ -> "&" | `Path _ -> "")
            takearg#argument
            retoarg#argument
        in
        sprintf "%s eval \"$(printf -- %s)\" || { echo 'Exec %s failed' >&2 ; } "
          (String.concat variables ~sep:"")
          exec
          exec
      in
      begin match redirections with
      | [] -> continue unit_t
      | one :: more ->
        continue (Seq (
            Raw_cmd (sprintf "( %s" (make_redirection one))
            ::
            List.map more ~f:(fun r -> Raw_cmd (make_redirection r))
            @ [unit_t]
            @ [ Raw_cmd ")" ]
          ))
      end
    | Write_output { expr; stdout; stderr; return_value } ->
      let ret_arg =
        Option.map return_value ~f:(fun v -> to_argument "retval" (`C_string v))
      in
      let var =
        Option.((ret_arg >>= fun ra -> ra#export) |> value ~default:"") in
      let with_potential_return =
        sprintf "%s { %s %s ; }" var (continue expr)
          (Option.value_map ret_arg ~default:"" ~f:(fun r ->
               sprintf "; printf -- \"$?\" > %s" r#argument))
      in
      let redirections =
        let make fd =
          Option.map
            ~f:(fun p -> {take = Construct.int fd; redirect_to = `Path p}) in
        [make 1 stdout; make 2 stderr] |> List.filter_opt in
      continue (Redirect_output (Raw_cmd with_potential_return, redirections))
    | Literal lit ->
      Literal.to_shell lit
    | Output_as_string e ->
      sprintf "\"$( { %s ; } | od -t o1 -An -v | tr -d ' \\n' )\"" (continue e)
    | Int_to_string i ->
      continue (Output_as_string (Raw_cmd (sprintf "printf -- '%%d' %s" (continue i))))
    | String_to_int s ->
      let var =  Unique_name.variable "string_to_int" in
      let value = sprintf "\"$%s\"" var in
      (* We put the result of the string expression in a variable to
         evaluate it once; then we test that the result is an integer
         (i.e. ["test ... -eq ...."] parses it as an integer). *)
      sprintf " $( %s=$( %s ) ; if [ %s -eq %s ] ; then printf -- %s ; else %s ; fi ; ) "
        var
        (continue s |> expand_octal)
        value value value
        (die (sprintf "String_to_int: error, $%s is not an integer" var))
    | Bool_to_string b ->
      continue (Output_as_string (Raw_cmd (sprintf "printf -- '%s'"
                                             (continue b))))
    | String_to_bool s ->
      continue (
        If (
          (String_operator (C_string_to_byte_array s,
                            `Eq, Literal (Literal.String "true"))),
          (Raw_cmd "true"),
          If (
            (String_operator (C_string_to_byte_array s,
                              `Eq, Literal (Literal.String "false"))),
            (Raw_cmd "false"),
            (Fail))
        )
      )
    | List l ->
      (* Lists are newline-separated internal represetations,
         prefixed by `G`. *)
      let output o = sprintf "printf -- 'G%%s' \"%s\"" (continue o) in
      let outputs = List.map l ~f:output in
      let rec build =
        function
        | [] -> []
        | [one] -> [one]
        | one :: two :: t ->
          one :: "printf -- '\\n'" :: build (two :: t)
      in
      (seq (build outputs))
    | List_to_string (l, f) ->
      continue (Output_as_string (Raw_cmd (continue l)))
    | String_to_list (s, f) ->
      continue s |> expand_octal |> sprintf "printf -- '%%s' \"$(%s)\""
    | C_string_concat sl ->
      let outputing_list = continue sl in
      sprintf "$( { %s ; } | tr -d 'G\\n' )" outputing_list
    | Byte_array_concat sl ->
      let outputing_list = continue sl in
      sprintf "$( { %s ; } | tr -d 'G\\n' )" outputing_list
    | List_append (la, lb) ->
      seq (continue la :: "printf -- '\\n'" :: continue lb :: [])
    | List_iter (l, f) ->
      let variter = Unique_name.variable "list_iter_var" in
      let varlist = Unique_name.variable "list_iter_list" in
      let outputing_list = continue l in
      seq [
        sprintf "export %s=\"$(%s)\" " varlist outputing_list;
        sprintf "for %s in ${%s} " variter varlist;
        "do : "; (* we cannot put a `;` after do so the first command is no-op *)
        continue (f (fun () ->
            (* Here we remove the `G` from the internal represetation: *)
            Raw_cmd (sprintf "${%s#G}" variter)));
        "done";
      ]
    | Int_bin_op (ia, op, ib) ->
      sprintf "$(( %s %s %s ))"
        (continue ia)
        begin match op with
        | `Div -> "/"
        | `Minus -> "-"
        | `Mult -> "*"
        | `Plus -> "+"
        | `Mod -> "%"
        end
        (continue ib)
    | Int_bin_comparison (ia, op, ib) ->
      sprintf "[ %s %s %s ]"
        (continue ia)
        begin match op with
        | `Eq -> "-eq"
        | `Ge -> "-ge"
        | `Gt -> "-gt"
        | `Le -> "-le"
        | `Lt -> "-lt"
        | `Ne -> "-ne"
        end
        (continue ib)
    | Feed (string, e) ->
      sprintf {sh|  %s | %s  |sh}
        (continue string |> expand_octal) (continue e)
    | Pipe [] -> ":"
    | Pipe l ->
      sprintf " %s "
        (List.map l ~f:continue |> String.concat ~sep:" | ")
    | Getenv s ->
      let var = Unique_name.variable "getenv" in
      let value = sprintf "\"$%s\"" var in
      let cmd_outputs_value =
        (* We need to get the output of the `string t` and then do a `$<thing>`
           on it:
           f () { printf "HOME" ;}
           aa=$(printf "\${%s}" $(f)) ; eval "printf \"$aa\""
           And the `` | tr -d '\\n' `` part is because `\n` in the variable name
           just “cuts” it, it wouldn't fail and `${HOME\nBOUH}` would be
           equal to `${HOME}`
        *)
        sprintf "{ %s=$(printf \\\"\\${%%s}\\\" $(%s | tr -d '\\n')) ; eval \"printf -- '%%s' %s\" ; } "
          var (continue s |> expand_octal) value in
      continue (Output_as_string (Raw_cmd cmd_outputs_value))
    | Setenv (variable, value) ->
      sprintf "export $(%s)=\"$(%s)\""
        (continue variable |> expand_octal)
        (continue value |> expand_octal)
    | With_signal {signal_name; catch; run} ->
      let var = Unique_name.variable "with_signal" in
      let value = sprintf "\"$%s\"" var in
      continue Construct.(seq [
          Raw_cmd (sprintf "export %s=$$" var);
          exec ["trap"; continue catch; signal_name];
          exec [
            (* We need the `sh -c ...` in order to properly create a subprocess,
               if not we break when `With_signal` are enclosed, the kill
               command does not wake up the right process. *)
            "sh"; "-c";
            run (Raw_cmd (sprintf " kill -s %s %s ; kill $$ " signal_name value))
            |> continue
          ];
        ])
    | Fail -> die "EDSL.fail called"

(*
     POSIX does not have ["set -o pipefail"].
     We implement it by killing the toplevel process with SIGUSR1, then we use
     ["trap"] to choose the exit status.
  *)
let with_trap ~statement_separator ~exit_with script =
  let variable_name = Unique_name.variable "genspio_trap" in
  let die s =
    sprintf " { printf -- '%%s\\n' \"%s\" >&2 ; kill -s USR1 ${%s} ; } " s variable_name in
  String.concat ~sep:statement_separator [
    sprintf "export %s=$$" variable_name;
    sprintf "trap 'exit %d' USR1" exit_with;
    script ~die;
  ]

let compile ~statement_separator ?(no_trap = false) e =
  match no_trap with
  | false ->
    with_trap ~statement_separator ~exit_with:77
      (fun ~die -> to_shell {statement_separator; die_command = Some die} e)
  | true ->
    to_shell {statement_separator; die_command = None} e

let to_one_liner ?no_trap e =
  let statement_separator = " ; " in
  compile ~statement_separator ?no_trap e

let to_many_lines ?no_trap e =
  let statement_separator = " \n " in
  compile ~statement_separator ?no_trap e
