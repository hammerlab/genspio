open Common

type internal_error_details = {variable: string; content: string; code: string}

let pp_internal_error_details ~big_string fmt {variable; content; code} =
  let open Format in
  fprintf fmt "@[<2>{variable:@ %a;@ content:@ %a;@ code:@ %a}@]" big_string
    variable big_string content big_string code

type death_message =
  | User of string
  | C_string_failure of internal_error_details
  | String_to_int_failure of internal_error_details

let pp_death_message ?(style = `Lispy) ~big_string fmt dm =
  let open Format in
  match style with
  | `Lispy -> (
    match dm with
    | User s -> fprintf fmt "@[<hov 2>(user@ %a)@]" big_string s
    | C_string_failure ied ->
        fprintf fmt "@[<hov 2>(c-string-failure@ %a)@]"
          (pp_internal_error_details ~big_string)
          ied
    | String_to_int_failure ied ->
        fprintf fmt "@[<hov 2>(string-to-int-failure@ %a)@]"
          (pp_internal_error_details ~big_string)
          ied )
  | `User -> (
    match dm with
    | User s -> fprintf fmt "@[<hov 2>%s@]" s
    | C_string_failure ied ->
        fprintf fmt
          "@[Byte-array cannot be converted to a C-string:@ @[<2>%a@]@]"
          (pp_internal_error_details ~big_string)
          ied
    | String_to_int_failure ied ->
        fprintf fmt "@[String cannot be converted to an Integer@ @[<2>%a@]@]"
          (pp_internal_error_details ~big_string)
          ied )

type death_function = comment_stack:string list -> death_message -> string

type output_parameters =
  { statement_separator: string
  ; die_command: death_function option
  ; max_argument_length: int option }

type internal_representation =
  | Unit of string
  | Octostring of string
  | Int of string
  | Bool of string
  | List of string
  | Death of string

let ir_unit s = Unit s

let ir_octostring s = Octostring s

let ir_int s = Int s

let ir_bool s = Bool s

let ir_death s = Death s

let ir_list s = List s

let ir_to_shell = function
  | Unit s -> s
  | Octostring s -> s
  | Int s -> s
  | Bool s -> s
  | List s -> s
  | Death s -> s

type compilation_error =
  { error:
      [ `No_fail_configured of death_message (* Argument of fail *)
      | `Max_argument_length of string (* Incriminated argument *)
      | `Not_a_c_string of string (* The actual string *) ]
  ; code: string option
  ; comment_backtrace: string list }

exception Compilation of compilation_error

let error ?code ~comment_backtrace error =
  raise (Compilation {code; comment_backtrace; error})

let pp_error fmt {code; comment_backtrace; error} =
  let open Format in
  let summary s =
    match String.sub s 0 70 with Some s -> s ^ " …" | None -> s
  in
  let big_string fmt s = fprintf fmt "@[%s@]" (summary s) in
  fprintf fmt "@[<hov 2>" ;
  fprintf fmt "Error:@ @[%a@];@ "
    (fun fmt -> function
      | `Max_argument_length s ->
          fprintf fmt "Comand-line argument too long:@ %d bytes,@ %S."
            (String.length s) (summary s)
      | `Not_a_c_string s ->
          fprintf fmt "String literal is not a valid/escapable C-string:@ %S."
            (summary s)
      | `No_fail_configured msg ->
          fprintf fmt
            "Call to `fail %a`@ while no “die” command is configured."
            (pp_death_message ~style:`Lispy ~big_string)
            msg )
    error ;
  fprintf fmt "Code:@ @[%s@];@ "
    (match code with None -> "NONE" | Some c -> summary c) ;
  fprintf fmt "Comment-backtrace:@ @[[%a]@]@ "
    (pp_print_list
       ~pp_sep:(fun fmt () -> fprintf fmt ";@ ")
       (fun fmt -> fprintf fmt "%S"))
    comment_backtrace ;
  fprintf fmt "@]" ;
  ()

let rec to_ir : type a. _ -> _ -> a Language.t -> internal_representation =
 fun comments params e ->
  let open Language in
  let continue_match ?add_comment e =
    let cmts =
      match add_comment with Some c -> c :: comments | None -> comments
    in
    to_ir cmts params e
  in
  let continue e = continue_match e |> ir_to_shell in
  let seq = function
    | [] -> ":"
    | l -> String.concat ~sep:params.statement_separator l
  in
  let die s =
    match params.die_command with
    | Some f -> f ~comment_stack:comments s
    | None -> error ~comment_backtrace:comments (`No_fail_configured s)
  in
  let expand_octal s =
    sprintf
      {sh| printf -- "$(printf -- '%%s\n' %s | sed -e 's/\(.\{3\}\)/\\\1/g')" |sh}
      s
  in
  let to_argument ~error_loc varprefix =
    let argument ?declaration ?variable_name argument =
      object
        method declaration = declaration

        method export = Option.map ~f:(sprintf "export %s ; ") declaration

        method variable_name = variable_name

        method argument = argument
      end
    in
    let check_length s =
      match params.max_argument_length with
      | None -> s
      | Some m when String.length s > m ->
          error ~comment_backtrace:comments (`Max_argument_length s)
            ~code:(Format.asprintf "%a" pp error_loc)
      | Some _ -> s
    in
    function
    | `C_string (c_str : c_string t) -> (
      match c_str with
      | Byte_array_to_c_string (Literal (Literal.String s))
        when Literal.Str.easy_to_escape s ->
          argument (Filename.quote s |> check_length)
      | Byte_array_to_c_string (Literal (Literal.String s))
        when Literal.Str.impossible_to_escape_for_variable s ->
          error ~comment_backtrace:comments (`Not_a_c_string s)
            ~code:(Format.asprintf "%a" pp error_loc)
      | other ->
          let variable_name = Unique_name.variable varprefix in
          let declaration =
            sprintf "%s=$(%s; printf 'x')" variable_name
              (continue other |> expand_octal |> check_length)
          in
          argument ~variable_name ~declaration
            (sprintf "\"${%s%%?}\"" variable_name) )
    | `Int (Literal (Literal.Int s)) -> argument (Int.to_string s)
    | `Int other ->
        let variable_name = Unique_name.variable varprefix in
        let declaration =
          sprintf "%s=%s" variable_name (continue other |> check_length)
        in
        argument ~variable_name ~declaration
          (sprintf "\"${%s%%?}\"" variable_name)
  in
  match e with
  | Exec l ->
      let variables = ref [] in
      let args =
        List.mapi l ~f:(fun index v ->
            let varname = sprintf "argument_%d" index in
            let arg = to_argument ~error_loc:e varname (`C_string v) in
            match arg#declaration with
            | None -> arg#argument
            | Some vardef ->
                variables := sprintf "%s ; " vardef :: !variables ;
                arg#argument )
      in
      List.rev !variables @ args
      |> String.concat ~sep:" " |> sprintf " { %s ; } " |> ir_unit
  | Raw_cmd (_, s) -> s |> ir_unit
  | Byte_array_to_c_string ba ->
      let bac = continue ba in
      let var = Unique_name.variable "byte_array_to_c_string" in
      let value = sprintf "\"$%s\"" var in
      let value_n = sprintf "\"$%s\\n\"" var in
      (* We store the internal octal representation in a variable, then
         we use `sed` to check that there are no `'\000'` characters.
         If OK we re-export with printf, if not we fail hard.
         The initial attempt was 's/\(000\)\|\(.\{3\}\)/\1/g' but
         BSD-ish `sed`s do not support “or”s in regular expressions.
         Cf. http://pubs.opengroup.org/onlinepubs/9699919799/utilities/sed.html
      *)
      sprintf "\"$(%s ; )\""
      @@ seq
           [ sprintf " %s=%s" var bac
           ; sprintf
               {sh|if [ "$(printf -- %s | sed -e 's/\(.\{3\}\)/@\1/g' | grep @000)" = "" ] |sh}
               value_n
           ; sprintf "then printf -- %s" value
           ; sprintf "else %s"
               (die
                  (C_string_failure
                     { variable= var
                     ; content= bac
                     ; code= Format.asprintf "%a" pp ba }))
           ; (* (sprintf "Byte_array_to_c_string: error, $%s is not a C string" *)
             (*      var)); *)
             "fi" ]
      |> ir_octostring
  | C_string_to_byte_array c -> continue c |> ir_octostring
  | Returns {expr; value} ->
      sprintf " { %s ; [ $? -eq %d ] ; }" (continue expr) value |> ir_bool
  | Bool_operator (a, op, b) ->
      sprintf "{ %s %s %s ; }" (continue a)
        (match op with `And -> "&&" | `Or -> "||")
        (continue b)
      |> ir_bool
  | String_operator (a, op, b) ->
      sprintf "[ \"%s\" %s \"%s\" ]" (continue a)
        (match op with `Eq -> "=" | `Neq -> "!=")
        (continue b)
      |> ir_bool
  | No_op -> ":" |> ir_unit
  | If (c, t, e) ->
      seq
        [ sprintf "if { %s ; }" (continue c)
        ; sprintf "then %s" (continue t)
        ; sprintf "else %s" (continue e)
        ; "fi" ]
      |> ir_unit
  | While {condition; body} ->
      seq
        [ sprintf "while { %s ; }" (continue condition)
        ; sprintf "do %s" (continue body)
        ; "done" ]
      |> ir_unit
  | Seq l -> seq (List.map l ~f:continue) |> ir_unit
  | Not t -> sprintf "! { %s ; }" (continue t) |> ir_bool
  | Redirect_output (unit_t, redirections) ->
      (*
         We're here compiling the redirections into `exec` statements which
         set up global redirections; we limit their scope with `( .. )`.
         E.g.
         (  exec 3>/tmp/output-of-ls ; exec 2>&3 ; exec 1>&2 ; ls ; ) ;
      *)
      let make_redirection {take; redirect_to} =
        let takearg =
          to_argument ~error_loc:e "redirection_take" (`Int take)
        in
        let retoarg =
          to_argument ~error_loc:e "redirection_to"
            (match redirect_to with `Fd i -> `Int i | `Path p -> `C_string p)
        in
        let variables = [takearg#export; retoarg#export] |> List.filter_opt in
        let exec =
          sprintf "\"exec %%s>%s%%s\" %s %s"
            (match redirect_to with `Fd _ -> "&" | `Path _ -> "")
            takearg#argument retoarg#argument
        in
        sprintf
          "%s eval \"$(printf -- %s)\" || { echo 'Exec %s failed' >&2 ; } "
          (String.concat variables ~sep:"")
          exec exec
      in
      ( match redirections with
      | [] -> continue unit_t
      | one :: more ->
          continue
            (Seq
               ( Raw_cmd (None, sprintf "( %s" (make_redirection one))
                 :: List.map more ~f:(fun r ->
                        Raw_cmd (None, make_redirection r) )
               @ [unit_t]
               @ [Raw_cmd (None, ")")] )) )
      |> ir_unit
  | Write_output {expr; stdout; stderr; return_value} ->
      let ret_arg =
        Option.map return_value ~f:(fun v ->
            to_argument ~error_loc:e "retval" (`C_string v) )
      in
      let var =
        Option.(ret_arg >>= (fun ra -> ra#export) |> value ~default:"")
      in
      let with_potential_return =
        sprintf "%s { %s %s ; }" var (continue expr)
          (Option.value_map ret_arg ~default:"" ~f:(fun r ->
               sprintf "; printf -- \"$?\" > %s" r#argument ))
      in
      let redirections =
        let make fd =
          Option.map ~f:(fun p -> {take= Construct.int fd; redirect_to= `Path p}
          )
        in
        [make 1 stdout; make 2 stderr] |> List.filter_opt
      in
      continue
        (Redirect_output (Raw_cmd (None, with_potential_return), redirections))
      |> ir_unit
  | Literal lit -> (
      let open Literal in
      match lit with
      | Int i -> sprintf "%d" i |> ir_int
      | String s ->
          with_buffer (fun str ->
              String.iter s ~f:(fun c -> Char.code c |> sprintf "%03o" |> str)
          )
          |> fst |> ir_octostring
      | Bool true -> ir_bool "true"
      | Bool false -> ir_bool "false" )
  | Output_as_string e ->
      sprintf "\"$( { %s ; } | od -t o1 -An -v | tr -d ' \\n' )\"" (continue e)
      |> ir_octostring
  | Int_to_string i ->
      continue
        (Output_as_string
           (Raw_cmd (None, sprintf "printf -- '%%d' %s" (continue i))))
      |> ir_octostring
  | String_to_int s ->
      let var = Unique_name.variable "string_to_int" in
      let value = sprintf "\"$%s\"" var in
      let content = continue s |> expand_octal in
      (* We put the result of the string expression in a variable to
         evaluate it once; then we test that the result is an integer
         (i.e. ["test ... -eq ...."] parses it as an integer). *)
      sprintf
        " $( %s=$( %s ) ; if [ %s -eq %s ] ; then printf -- %s ; else %s ; fi \
         ; ) "
        var content value value value
        (die
           (String_to_int_failure
              {variable= var; content; code= Format.asprintf "%a" pp s}))
      |> ir_int
  | Bool_to_string b ->
      continue
        (Output_as_string
           (Raw_cmd
              ( None
              , sprintf
                  "{ if %s ; then printf true ; else printf false ; fi ; }"
                  (continue b) )))
      |> ir_octostring
  | String_to_bool s ->
      continue
        (If
           ( String_operator
               (C_string_to_byte_array s, `Eq, Literal (Literal.String "true"))
           , Raw_cmd (None, "true")
           , If
               ( String_operator
                   ( C_string_to_byte_array s
                   , `Eq
                   , Literal (Literal.String "false") )
               , Raw_cmd (None, "false")
               , Fail (sprintf "String_to_bool") ) ))
      |> ir_bool
  | List l ->
      (* Lists are space-separated internal represetations,
         prefixed by `G`. *)
      let output o = sprintf "printf -- 'G%%s' \"%s\"" (continue o) in
      let outputs = List.map l ~f:output in
      let rec build = function
        | [] -> []
        | [one] -> [one]
        | one :: two :: t -> one :: "printf -- ' '" :: build (two :: t)
      in
      seq (build outputs) |> ir_list
  | List_to_string (l, f) ->
      continue (Output_as_string (Raw_cmd (None, continue l))) |> ir_octostring
  | String_to_list (s, f) ->
      continue s |> expand_octal
      |> sprintf "printf -- '%%s' \"$(%s)\""
      |> ir_list
  | C_string_concat sl ->
      let outputing_list = continue sl in
      sprintf "$( { %s ; } | tr -d 'G ' )" outputing_list |> ir_octostring
  | Byte_array_concat sl ->
      let outputing_list = continue sl in
      sprintf "$( { %s ; } | tr -d 'G ' )" outputing_list |> ir_octostring
  | List_append (la, lb) ->
      seq [continue la; "printf -- ' '"; continue lb] |> ir_list
  | List_iter (l, f) ->
      let variter = Unique_name.variable "list_iter_var" in
      let outputing_list = continue l in
      seq
        [ sprintf "for %s in $(%s) " variter outputing_list
        ; "do : "
        ; (* we cannot put a `;` after do so the first command is no-op *)
          continue
            (f (fun () ->
                 (* Here we remove the `G` from the internal represetation: *)
                 Raw_cmd (None, sprintf "${%s#G}" variter) ))
        ; "done" ]
      |> ir_unit
  | Int_bin_op (ia, op, ib) ->
      sprintf "$(( %s %s %s ))" (continue ia)
        ( match op with
        | `Div -> "/"
        | `Minus -> "-"
        | `Mult -> "*"
        | `Plus -> "+"
        | `Mod -> "%" )
        (continue ib)
      |> ir_int
  | Int_bin_comparison (ia, op, ib) ->
      sprintf "[ %s %s %s ]" (continue ia)
        ( match op with
        | `Eq -> "-eq"
        | `Ge -> "-ge"
        | `Gt -> "-gt"
        | `Le -> "-le"
        | `Lt -> "-lt"
        | `Ne -> "-ne" )
        (continue ib)
      |> ir_int
  | Feed (string, e) ->
      sprintf {sh|  %s | %s  |sh}
        (continue string |> expand_octal)
        (continue e)
      |> ir_unit
  | Pipe [] -> ":" |> ir_unit
  | Pipe l ->
      sprintf " %s " (List.map l ~f:continue |> String.concat ~sep:" | ")
      |> ir_unit
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
        sprintf
          "{ %s=$(printf \\\"\\${%%s}\\\" $(%s | tr -d '\\n')) ; eval \
           \"printf -- '%%s' %s\" ; } "
          var
          (continue s |> expand_octal)
          value
      in
      continue (Output_as_string (Raw_cmd (None, cmd_outputs_value)))
      |> ir_octostring
  | Setenv (variable, value) ->
      sprintf "export $(%s)=\"$(%s)\""
        (continue variable |> expand_octal)
        (continue value |> expand_octal)
      |> ir_unit
  | Fail s -> die (User s) |> ir_death
  | Comment (cmt, expr) -> (
    match continue_match ~add_comment:cmt expr with
    | Unit u ->
        sprintf " { %s ; %s ; }" Construct.(exec [":"; cmt] |> continue) u
        |> ir_unit
    | (Octostring _ | Int _ | Bool _ | List _ | Death _) as d -> d )

let to_shell options expr = to_ir [] options expr |> ir_to_shell

(*
     POSIX does not have ["set -o pipefail"].
     We implement it by killing the toplevel process with SIGUSR1, then we use
     ["trap"] to choose the exit status.
  *)
let with_die_function ~print_failure ~statement_separator ~signal_name
    ?(trap = `Exit_with 77) script =
  let variable_name = Unique_name.variable "genspio_trap" in
  let die ~comment_stack s =
    let pr = print_failure ~comment_stack s in
    sprintf " { %s ; kill -s %s ${%s} ; } " pr signal_name variable_name
  in
  String.concat ~sep:statement_separator
    [ sprintf "export %s=$$" variable_name
    ; ( match trap with
      | `Exit_with ex -> sprintf "trap 'exit %d' %s" ex signal_name
      | `None -> ": 'No Trap'" )
    ; script ~die ]
