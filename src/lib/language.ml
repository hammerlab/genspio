
open Common
    
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
      end |> fst
    | Bool true -> "0"
    | Bool false -> "1"
end

type cli_option = {
  switch: char;
  doc: string;
}
type _ option_spec =
  | Opt_flag: cli_option -> bool t option_spec
  | Opt_string: cli_option -> string t option_spec
and (_, _) cli_options =
  | Opt_end: string -> ('a, 'a) cli_options
  | Opt_cons: 'c option_spec * ('a, 'b) cli_options -> ('c -> 'a, 'b) cli_options
and _ t =
  | Exec: string list -> unit t
  | Raw_cmd: string -> unit t
  | Bool_operator: bool t * [ `And | `Or ] * bool t -> bool t
  | String_operator: string t * [ `Eq | `Neq ] * string t -> bool t
  | Not: bool t -> bool t
  | Returns: {expr: 'a t; value: int} -> bool t
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
  | Parse_command_line: {
      options: ('a, unit t) cli_options;
      action: 'a;
    } -> unit t
  | Fail: unit t

module Construct = struct
  let exec l = Exec l
  let (&&&) a b = Bool_operator (a, `And, b)
  let (|||) a b = Bool_operator (a, `Or, b)
  let (=$=) a b = String_operator (a, `Eq, b)
  let (<$>) a b = String_operator (a, `Neq, b)

  let returns expr ~value = Returns {expr; value}

  let succeeds expr = returns expr ~value:0

  let nop = No_op
  let if_then_else a b c = If (a, b, c)
  let if_then a b = if_then_else a b nop
  let seq l = Seq l

  let not t = Not t

  let printf fmt =
    ksprintf (fun s -> exec ["printf"; "%s"; s]) fmt

  let file_exists p =
    exec ["test"; "-f"; p] |> succeeds

  let fail = Fail

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

  module Option_list = struct
    let string ~doc switch  = Opt_string {switch; doc}
    let flag ~doc switch = Opt_flag {switch; doc}

    let (&) x y = Opt_cons (x, y)
    let usage s = Opt_end s

  end

  let parse_command_line options action =
    Parse_command_line {options; action}


end

type output_parameters = {
  statement_separator: string;
  die_command: string
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
        | '{' | '}' | '(' | ')' | '[' | ']' -> true
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
            let var, () =
              with_buffer begin fun str ->
                ksprintf str "argument_%d=$(printf '" index;
                String.iter arg ~f:(fun c ->
                    Char.code c |> sprintf "\\%03o" |> str
                  );
                str "'; printf 'x') ; "
              end in
            variables := var :: !variables;
            sprintf "\"${argument_%d%%?}\"" index
          )
      in
      (List.rev !variables) @ args |> String.concat ~sep:" "
    | Raw_cmd s -> s 
    | Returns {expr; value} ->
      sprintf " { %s ; [ $? -eq %d ] ; }" (continue expr) value
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
      sprintf "\"$( { %s || %s ; }  | od -t o1 -An -v | tr -d ' \\n' )\""
        (continue e) params.die_command
    | Feed (string, e) ->
      sprintf {sh|  %s | %s  |sh}
        (continue string |> expand_octal) (continue e)
    | Fail ->
      params.die_command
    | Parse_command_line { options; action } ->
      let prefix = Unique_name.create "getopts" in
      let variable {switch; doc;} =
        sprintf "%s_%c" prefix switch in
      let inits = ref [] in
      let to_init s = inits := s :: !inits in
      let cases = ref [] in
      let to_case s = cases := s :: !cases in
      let help_intro = ref "" in
      let help = ref [] in
      let to_help s = help := s :: !help in
      let string_of_var var =
        Output_as_string (Raw_cmd (sprintf "printf \"${%s}\"" var)) in
      let bool_of_var var =
        Construct.succeeds (Raw_cmd (sprintf "[ \"${%s}\" -eq 0 ]" var)) in
      let unit_t =
        let rec loop
          : type a b.
            a -> (a, b) cli_options -> b =
          fun f -> function
          | Opt_end doc ->
            help_intro := doc;
            f
          | Opt_cons (Opt_string x, more) ->
            let var = variable x in
            to_init (sprintf "export %s= " var);
            to_case (sprintf "-%c) %s ;;"
                       x.switch
                       (seq [
                           "if [ -n \"$2\" ]";
                           sprintf "then export %s=\"$2\" " var;
                           sprintf "else printf \"ERROR -%c requires an argument\\n\" \
                                    >&2" x.switch;
                           params.die_command;
                           "fi";
                           "shift";
                           "shift";
                         ]));
            ksprintf to_help "* `-%c <string>`: %s" x.switch x.doc;
            loop (f (string_of_var var)) more
          | Opt_cons (Opt_flag x, more) ->
            let var = variable x in
            to_init (sprintf "export %s=1 " var);
            to_case (
              sprintf "-%c) %s ;;"
                x.switch (seq [
                    sprintf "export %s=0" var;
                    "shift";
                  ])
            );
            ksprintf to_help "* `-%c`: %s" x.switch x.doc;
            loop (f (bool_of_var var)) more
        in
        loop action options
      in
      let help_msg =
        sprintf "%s\n\nOptions:\n\n%s\n"
          !help_intro (String.concat ~sep:"\n" (List.rev !help))
      in
      let while_loop =
        let sep = if params.statement_separator = " \n " then "\n" else " " in
        String.concat ~sep (
          [
            "while :;"; " do case $1 in";
            "-h|-help|--help) ";
            sprintf "export %s_help=0 ; " prefix;
            sprintf "%s ;"
              (continue Construct.(string help_msg
                                   >>  exec ["cat"]));
            " break ;;"
          ]
          @ List.rev !cases
          @ [
            "--) shift ; break ;;";
            "-?*)";
            "printf 'ERROR: Unknown option: %s\\n' \"$1\" >&2 ;";
            params.die_command;
            ";;";
            "*) if [ $# -eq 0 ] ; ";
            "then echo \" $1 $# \" ; break ;";
            sprintf
              " else export %s_args=\"${%s_args} %s\" ; shift ; "
              prefix prefix
              (continue (Output_as_string (Raw_cmd "printf \"$1\""))) ;
            "fi ;; ";
            "esac;";
            "done"]
        )
      in
      seq (
        sprintf "export %s_args=" prefix
        :: sprintf "export %s_help=1" prefix
        :: List.rev !inits @ [
          while_loop;
          continue Construct.(
              if_then_else (bool_of_var (sprintf "%s_help" prefix))
                (nop)
                unit_t);
        ])

(* 
     POSIX does not have ["set -o pipefail"].
     We implement it by killing the toplevel process with SIGUSR1, then we use
     ["trap"] to choose the exit status.
  *)
let with_trap ~statement_separator ~exit_with script =
  let variable_name = "very_long_name_that_we_should_not_reuse" in
  let die = sprintf "kill -s USR1 ${%s}" variable_name in
  String.concat ~sep:statement_separator [
    sprintf "export %s=$$" variable_name;
    sprintf "trap 'echo Script-failed-using-signal ; exit %d' USR1" exit_with;
    script ~die;
  ]


let rec to_one_liner: type a. a t -> string = fun e ->
  let statement_separator = " ; " in
  with_trap ~statement_separator ~exit_with:77
    (fun ~die -> to_shell {statement_separator; die_command = die} e)

let rec to_many_lines: type a. a t -> string = fun e ->
  let statement_separator = " \n " in
  with_trap ~statement_separator ~exit_with:77
    (fun ~die -> to_shell {statement_separator; die_command = die} e)
