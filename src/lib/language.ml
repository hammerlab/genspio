
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

type 'a cli_option = {
  switch: char;
  doc: string;
  default: 'a;
}
type _ option_spec =
  | Opt_flag:   bool t cli_option -> bool t option_spec
  | Opt_string: string t cli_option -> string t option_spec
and (_, _) cli_options =
  | Opt_end: string -> ('a, 'a) cli_options
  | Opt_cons: 'c option_spec * ('a, 'b) cli_options -> ('c -> 'a, 'b) cli_options
and _ t =
  | Exec: string t list -> unit t
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
      stdout: string t option;
      stderr: string t option;
      return_value: string t option;
    } -> unit t
  | Feed: string t * unit t -> unit t
  | While: {condition: bool t; body: unit t} -> unit t
  | Parse_command_line: {
      options: ('a, unit t) cli_options;
      action: 'a;
    } -> unit t
  | Fail: unit t
  | Int_to_string: int t -> string t
  | String_to_int: string t -> int t
  | Int_bin_op:
      int t * [ `Plus | `Minus | `Mult | `Div | `Mod ] * int t -> int t
  | Int_bin_comparison:
      int t * [ `Eq | `Ne | `Gt | `Ge | `Lt | `Le ] * int t -> bool t
  | Getenv: string t -> string t
  | Setenv: string t * string t -> unit t 
  | With_signal: {
      signal_name: string;
      catch: unit t;
      run: unit t -> unit t;
    } -> unit t

module Construct = struct
  let exec l = Exec (List.map l ~f:(fun s -> Literal (Literal.String s)))
  let call l = Exec l
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

  let with_signal ?(signal_name = "USR1") ~catch run =
    With_signal {signal_name; catch; run}

  let fail = Fail

  let make_switch: type a. (bool t * unit t) list -> default: unit t -> unit t =
    fun conds ~default ->
      List.fold_right conds ~init:default ~f:(fun (x, body) prev ->
          if_then_else x body prev)

  let write_output ?stdout ?stderr ?return_value expr =
    Write_output {expr; stdout; stderr; return_value}

  let write_stdout ~path expr = write_output expr ~stdout:path

  let literal l = Literal l
  let string s = Literal.String s |> literal
  let int s = Literal.Int s |> literal
  let bool t = Literal.Bool t |> literal

  let file_exists p =
    call [string "test"; string "-f"; p] |> succeeds

  let getenv v = Getenv v
  let setenv ~var v = Setenv (var, v)

  let output_as_string e = Output_as_string e

  let feed ~string e = Feed (string, e)
  let (>>) string e = feed ~string e

  let loop_while condition ~body = While {condition; body}

  module Integer = struct
    let to_string i = Int_to_string i
    let of_string s = String_to_int s
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

  module Option_list = struct
    let string ?(default = string "") ~doc switch =
      Opt_string {switch; doc; default}
    let flag ?(default = bool false) ~doc switch =
      Opt_flag {switch; doc; default}

    let (&) x y = Opt_cons (x, y)
    let usage s = Opt_end s

  end

  let parse_command_line options action =
    Parse_command_line {options; action}

  module Magic = struct
    let unit s = Raw_cmd s
  end

end

type output_parameters = {
  statement_separator: string;
  die_command: (string -> string) option;
}
let rec to_shell: type a. _ -> a t -> string =
  fun params e ->
    let continue e = to_shell params e in
    let seq l = String.concat  ~sep:params.statement_separator l in
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
        {sh| printf "$(printf '%%s' %s | sed -e 's/\(.\{3\}\)/\\\1/g')" |sh}
        s in
    let to_argument varname =
      function
      | Literal (Literal.String s) when Literal.String.easy_to_escape s ->
        None, Filename.quote s
      | Literal (Literal.String s) when
          Literal.String.impossible_to_escape_for_variable s ->
        ksprintf failwith "to_shell: sorry literal %S is impossible to \
                           escape as `exec` argument" s
      | v ->
        let var =
          sprintf "%s=$(%s; printf 'x')"
            varname (continue v |> expand_octal)
        in
        Some var, sprintf "\"${%s%%?}\"" varname
    in
    match e with
    | Exec l ->
      let variables = ref [] in
      let args =
        List.mapi l ~f:(fun index v ->
            let varname = sprintf "argument_%d" index in
            match to_argument varname v with
            | None, v -> v
            | Some vardef, v ->
              variables := sprintf "%s ; " vardef :: !variables;
              v) in
      (List.rev !variables) @ args
      |> String.concat ~sep:" "
      |> sprintf " { %s ; } "
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
      let make_argument name option =
        Option.value_map ~default:(None, None) option ~f:(fun v ->
            let v, e = to_argument name v in
            v, Some e) in
      let varstdout, exprstdout = make_argument "stdoutfile" stdout in
      let varstderr, exprstderr = make_argument "stderrfile" stderr in
      let varret, exprret = make_argument "retfile" return_value in
      let vars =
        List.filter_map [varstdout; varstderr; varret]
          ~f:(Option.map ~f:(sprintf "export %s ; "))
        |> String.concat ~sep:" " in
      sprintf "%s { %s %s ; } %s %s"
        vars
        (continue expr)
        (Option.value_map exprret ~default:"" ~f:(fun path ->
             sprintf "; echo \"$?\" > %s" path))
        (Option.value_map exprstdout ~default:"" ~f:(fun path ->
             sprintf " > %s" path))
        (Option.value_map exprstderr ~default:"" ~f:(fun path ->
             sprintf "2> %s" path))
    | Literal lit ->
      Literal.to_shell lit
    | Output_as_string e ->
      sprintf "\"$( { %s ; } | od -t o1 -An -v | tr -d ' \\n' )\"" (continue e)
    | Int_to_string i ->
      continue (Output_as_string (Raw_cmd (sprintf "printf '%%d' %s" (continue i))))
    | String_to_int s ->
      let var = "tmpxxxxijljlijdeifh" in
      let value = sprintf "\"$%s\"" var in
      (* We put the result of the string expression in a variable to
         evaluate it once; then we test that the result is an integer
         (i.e. ["test ... -eq ...."] parses it as an integer). *)
      sprintf " $( %s=$( %s ) ; if [ %s -eq %s ] ; then printf -- %s ; else %s ; fi ; ) "
        var
        (continue s |> expand_octal)
        value value value
        (die (sprintf "String_to_int: error, $%s is not an integer" var))
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
    | Getenv s ->
      let var = "tmpxxjljlijdeifhideijdedeiii" in
      let value = sprintf "\"$%s\"" var in
      let cmd_outputs_value =
        (* We need to get the output of the `string t` and then do a `$<thing>`
           on it:
           f () { printf "HOME" ;}
           aa=$(printf "\${%s}" $(f)) ; sh -c "printf \"$aa\""
           And the `` | tr -d '\\n' `` part is because `\n` in the variable name
           just “cuts” it, it wouldn't fail and `${HOME\nBOUH}` would be
           equal to `${HOME}`
        *)
        sprintf "{ %s=$(printf \\\"\\${%%s}\\\" $(%s | tr -d '\\n')) ; sh -c \"printf %s\" ; } "
          var (continue s |> expand_octal) value in
      continue (Output_as_string (Raw_cmd cmd_outputs_value))
    | Setenv (variable, value) ->
      sprintf "export $(%s)=$(%s)"
        (continue variable |> expand_octal)
        (continue value |> expand_octal)
    | With_signal {signal_name; catch; run} ->
      let var =
        sprintf "tmpxxjljeadjeidjelidjeideijdedeiii_%d" (Random.int 4309843) in
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
        Construct.succeeds (Raw_cmd (sprintf "{ ${%s} ; } " var)) in
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
            to_init (sprintf "export %s=$(%s)"
                       var (continue x.default |> expand_octal));
            to_case (sprintf "-%c) %s ;;"
                       x.switch
                       (seq [
                           "if [ -n \"$2\" ]";
                           sprintf "then export %s=\"$2\" " var;
                           sprintf "else printf \"ERROR -%c requires an argument\\n\" \
                                    >&2" x.switch;
                           die "Command line parsing error: Aborting";
                           "fi";
                           "shift";
                           "shift";
                         ]));
            ksprintf to_help "* `-%c <string>`: %s" x.switch x.doc;
            loop (f (string_of_var var)) more
          | Opt_cons (Opt_flag x, more) ->
            let var = variable x in
            to_init (sprintf
                       "export %s=$(if %s ; then printf 'true' ; else printf 'false' ; fi)" var
                       (continue x.default));
            to_case (
              sprintf "-%c) %s ;;"
                x.switch (seq [
                    sprintf "export %s=true" var;
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
            sprintf "export %s_help=true ; " prefix;
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
            die "Command line parsing error: Aborting";
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
        :: sprintf "export %s_help=false" prefix
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
  let die s =
    sprintf " { printf '%%s\\n' \"%s\" >&2 ; kill -s USR1 ${%s} ; } " s variable_name in
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
