(*md

The `To_slow_flow` Compiler is meant to be more portable than the
“standard” one by using less complex constructs.

In particular, the default `bash` installed on MacOSX is old and contains (at least) one bug
which breaks many standard-compiled Genspio scripts,
cf. the issue [`hammerlab/genspio#68`](https://github.com/hammerlab/genspio/issues/68),

The `To_slow_flow` compiler is often tested
on the “old darwin” VM (cf.
[`src/examples/vm_tester.ml`](https://github.com/hammerlab/genspio/blob/4ad23712e7af5a2cd1471f5ca00165cd18a93011/src/examples/vm_tester.ml#L401)), and all the Genspio tests succeed, see also
[comment](https://github.com/hammerlab/genspio/pull/75#issuecomment-411843975)
on the initial PR
[`hammerlab/genspio#75`](https://github.com/hammerlab/genspio/pull/75).

*)
open Common
open Language

let string_to_octal ?(prefix = "") s =
  with_buffer (fun str ->
      String.iter s ~f:(fun c ->
          str prefix ;
          Char.code c |> sprintf "%03o" |> str ) )
  |> fst

let expand_octal_command ~remove_l s =
  sprintf
    {sh| printf -- "$(printf -- '%%s\n' %s %s | sed -e 's/\(.\{3\}\)/\\\1/g')" |sh}
    s
    (if remove_l then "| tr -d L" else "")

let m = ref 0

let var_name ?expression ?script tag =
  incr m ;
  let stag = String.map tag ~f:(function '-' -> '_' | a -> a) in
  sprintf "genspio_%s_%d_%d_%s" stag (Random.int 100_000_000) !m
    ( Marshal.to_string (expression, script) [Marshal.Closures]
    |> Digest.string |> Digest.to_hex )

module Tmp_db = struct
  type t =
    { default_tmpdir: string
    ; deletion_grouping: int
    ; mutable tmp_file_db: (string * string) list }

  let make ?(deletion_grouping = 20) how =
    let default_tmpdir =
      match how with
      | `Fresh -> Filename.concat "/tmp" (var_name "tmpdir")
      | `Use p -> p
    in
    {default_tmpdir; tmp_file_db= []; deletion_grouping}

  let register_file t ~variable ~directory =
    t.tmp_file_db <- (variable, directory) :: t.tmp_file_db

  let default_tmpdir t = t.default_tmpdir

  let delete_function t delete_fname =
    let rec unroll = function
      | [] -> []
      | l ->
          let tk, nxt = List.split_n l t.deletion_grouping in
          sprintf "rm -f %s"
            ( List.map tk ~f:(fun (v, _) -> sprintf "\"${%s}\"" v)
            |> String.concat ~sep:" " )
          :: unroll nxt
    in
    sprintf "%s () {\n: Deleting all the temporary files:\n%s\n}" delete_fname
    @@ String.concat ~sep:"\n" (unroll (List.dedup t.tmp_file_db))

  let tmp_vars t =
    List.map (List.dedup t.tmp_file_db) ~f:(fun (v, dir) ->
        (v, dir, sprintf "%s=%s/tmp-%s" v dir v) )
end

(*md

The `Script` module defines an intermediate representation for the compiler:

- a sequence of “commands,” and
- a return value.


*)
module Script = struct
  type command =
    | Raw of string
    | Comment of string
    | Redirect of {block: command list; stdout: compiled_value}
    | If_then_else of
        { condition: string
        ; block_then: command list
        ; block_else: command list }
    | While of {condition: string; block: command list}
    | Sub_shell of command list
    (* As is in `( ... ; )` in POSIX shells. *)
    | Make_directory of string
    | Pipe of {blocks: command list list}

  and compiled_value =
    | Unit
    | Literal_value of string
    | File of string
    | Tmp_file_in_variable of string  (** File-path contained in variable. *)
    | Raw_inline of string
    | Octal_value_in_variable of string

  type t = {commands: command list; result: compiled_value}

  (*md

The function `to_argument` converts a return value into a piece of
shell script that can be used as the argument of a shell command.

The oddly named `~arithmetic` option instructs the function to remove
quoting. See the ``| Int_bin_op (ia, op, ib) ->`` case below,
``$(( ... ))`` shell constructs do not allow quoted arguments.


  *)
  let to_argument ?(arithmetic = false) = function
    | Unit -> "\"$(exit 42)\""
    | Literal_value s when String.exists s ~f:(( = ) '\x00') ->
        let oct = string_to_octal s in
        let v = sprintf "$(%s)" (expand_octal_command ~remove_l:false oct) in
        if not arithmetic then sprintf "\"%s\"" v else v
    | Literal_value s ->
        let v = Filename.quote s in
        if arithmetic then sprintf "$(printf -- %s)" v else v
    | File s ->
        let v = sprintf "$(cat %s)" (Filename.quote s) in
        if not arithmetic then sprintf "\"%s\"" v else v
    | Tmp_file_in_variable s ->
        (* Parameters.(tmp_file_db := s :: !tmp_file_db) ; *)
        let v = sprintf "$(cat ${%s})" s in
        if not arithmetic then sprintf "\"%s\"" v else v
    | Raw_inline s when not arithmetic -> s
    | Raw_inline s -> sprintf "$(printf -- '%%s' %s)" s
    | Octal_value_in_variable var ->
        (if arithmetic then sprintf "$(%s)" else sprintf "\"$(%s)\"")
          (expand_octal_command ~remove_l:true (sprintf "${%s}" var))

  (*md
  
There are special cases where `to_argument` does not work with
arbitrary content: when we need to compare strings with
`[ ... <op> ... ]` constructs.
In that case, we compare the octal representations.

*)
  let to_ascii = function
    | Unit -> assert false
    | Raw_inline s -> s
    | Literal_value s -> string_to_octal s
    | File f ->
        sprintf "$(cat %s | od -t o1 -An -v | tr -d ' \\n')" (Filename.quote f)
    | Tmp_file_in_variable f ->
        (* Parameters.(tmp_file_db := f :: !tmp_file_db) ; *)
        sprintf "$(cat \"${%s}\" | od -t o1 -An -v | tr -d ' \\n')" f
    | Octal_value_in_variable var -> sprintf "${%s}" var

  let commands s = s.commands

  let to_path_argument = function
    | Unit -> assert false
    | Raw_inline s -> s
    | Literal_value s -> assert false
    | File f -> Filename.quote f
    | Tmp_file_in_variable f ->
        (* Parameters.(tmp_file_db := f :: !tmp_file_db) ; *)
        sprintf "\"${%s}\"" f
    | Octal_value_in_variable var -> assert false

  (*md

 The last stage is `pp_posix`, it compiles the IR to a POSIX shell script:

 ```ocaml
   let compiled = (* ... *) in
   fprintf fmt "%a" Script.pp_posix compiled
   (* profit ! *)
```

 *)
  let pp_posix fmt script =
    let open Format in
    let mkdir_done = ref [] in
    let rec pp_command fmt = function
      | Raw s -> fprintf fmt "%s" s
      | Comment s ->
          fprintf fmt "%s"
            ( String.split ~on:(`Character '\n') s
            |> List.map ~f:(sprintf "##  %s")
            |> String.concat ~sep:"\n" )
      | Redirect {block; stdout} ->
          fprintf fmt ": redirect ; %a  > %s" pp_block block
            (to_path_argument stdout)
      | If_then_else {condition; block_then; block_else} ->
          fprintf fmt "if %s ; then\n%a\nelse\n%a\nfi" condition pp_block
            block_then pp_block block_else
      | While {condition; block} ->
          fprintf fmt "while %s ; do\n%a\ndone" condition pp_block block
      | Sub_shell c -> fprintf fmt "(\n%a\n)" pp_command_list c
      | Pipe {blocks} ->
          fprintf fmt "%a"
            (pp_print_list ~pp_sep:(fun fmt () -> fprintf fmt " | ") pp_block)
            blocks
      | Make_directory f when List.mem ~set:!mkdir_done f -> ()
      | Make_directory f ->
          mkdir_done := f :: !mkdir_done ;
          fprintf fmt "mkdir -p %s" f
    and pp_command_list fmt =
      pp_print_list ~pp_sep:(fun fmt () -> fprintf fmt "@\n") pp_command fmt
    and pp_block fmt = fprintf fmt "{\n%a\n}" pp_command_list in
    let pp_result fmt = function
      | Unit -> fprintf fmt "Unit"
      | Literal_value s -> fprintf fmt "Literal: %S" s
      | File s -> fprintf fmt "File: %S" s
      | Tmp_file_in_variable s -> fprintf fmt "File: ${%s}" s
      | Raw_inline s -> fprintf fmt "Raw: %S" s
      | Octal_value_in_variable var -> fprintf fmt "Octal in $%s" var
    in
    fprintf fmt "%a\n# Result: %a\n" pp_command_list script.commands pp_result
      script.result

  let rawf fmt = ksprintf (fun s -> Raw s) fmt

  let cmtf fmt = ksprintf (fun s -> Comment s) fmt

  let make commands result = {commands; result}

  let unit commands = make commands Unit

  let literal_value v = make [] (Literal_value v)

  let assert_unit s = assert (s.result = Unit)

  let redirect ~stdout block =
    { stdout with
      commands= stdout.commands @ [Redirect {block; stdout= stdout.result}] }

  let mktmp ~tmpdb ?expression ?script tag =
    let v = var_name ?expression ?script tag in
    let dir = sprintf "\"${TMPDIR:-%s}\"" (Tmp_db.default_tmpdir tmpdb) in
    Tmp_db.register_file tmpdb ~variable:v ~directory:dir ;
    make [] (Tmp_file_in_variable v)

  let with_tmp ~tmpdb ?expression ?script tag f =
    let tmp = mktmp ~tmpdb ?expression ?script tag in
    let cmds = f tmp in
    make (tmp.commands @ cmds) tmp.result

  let if_then_else cond t e =
    assert_unit t ;
    assert_unit e ;
    let condition = to_argument cond.result in
    let commands =
      cond.commands
      @ [ If_then_else
            {condition; block_then= t.commands; block_else= e.commands} ]
    in
    make commands Unit

  let bool_to_file b tmp = [rawf "printf %b > %s" b (to_path_argument tmp)]

  let bool_not ~tmp {commands; result} =
    let r, morecmds =
      match result with
      | Unit -> assert false
      | Literal_value "true" -> (Literal_value "false", [])
      | Literal_value "false" -> (Literal_value "true", [])
      | Literal_value s ->
          (Literal_value s, []) (* This should just be an error later *)
      | Raw_inline s -> (Raw_inline (sprintf "! %s" s), [])
      | (Tmp_file_in_variable _ | File _ | Octal_value_in_variable _) as p ->
          ( tmp.result
          , tmp.commands
            @ [ If_then_else
                  { condition= to_argument p
                  ; block_then= bool_to_file false tmp.result
                  ; block_else= bool_to_file true tmp.result } ] )
    in
    {commands= commands @ morecmds; result= r}

  let return_value_to_bool ~tmp {commands; result} v =
    { tmp with
      commands=
        tmp.commands @ commands
        @ [ If_then_else
              { condition= sprintf " [ $? -eq %d ]" v
              ; block_then= bool_to_file true tmp.result
              ; block_else= bool_to_file false tmp.result } ] }

  let make_bool ~condition ~tmp commands =
    let block_then = bool_to_file true tmp.result in
    let block_else = bool_to_file false tmp.result in
    { tmp with
      commands=
        tmp.commands @ commands
        @ [If_then_else {condition; block_then; block_else}] }

  let while_loop cond body =
    unit
      ( cond.commands
      @ [ While
            { condition= to_argument cond.result
            ; block=
                body.commands @ cond.commands
                (* We need re-evaluate the condition *) } ] )

  let sub_shell ~pre l = unit @@ pre @ [Sub_shell l]
end

type Language.raw_command_annotation += Octal_in_variable of string

(*md

The compilation from a `'a Language.t` to the intermediary
representation.

*)
let rec to_ir : type a. fail_commands:_ -> tmpdb:_ -> a t -> Script.t =
 fun ~fail_commands ~tmpdb e ->
  let continue : type a. a t -> _ = fun x -> to_ir ~fail_commands ~tmpdb x in
  let concat_string_list sl_script =
    let open Script in
    let list_file = to_path_argument sl_script.result in
    let tmp = mktmp ~tmpdb ~expression:e "c-string-concat" in
    let tmppatharg = to_path_argument tmp.result in
    let file_var = var_name ~expression:e "list_item" in
    let loop =
      [ rawf "rm -f %s ; touch %s ; for %s in $(cat %s) ; do %s >> %s\ndone"
          tmppatharg tmppatharg file_var list_file
          (expand_octal_command ~remove_l:true (sprintf "${%s}" file_var))
          tmppatharg ]
    in
    make (tmp.commands @ sl_script.commands @ loop) tmp.result
  in
  let result_to_file s =
    let open Script in
    (* let tmp = tmp_path ~tmpdb ~expression:e ~script:s "result-to-file" in *)
    let tmp = mktmp ~tmpdb ~expression:e "c-string-concat" in
    let tmparg = to_path_argument tmp.result in
    let mk (cmd, res) = Script.make (res.commands @ [cmd]) res.result in
    match s.result with
    | Unit -> mk (rawf "echo '' > %s" tmparg, tmp)
    | Literal_value v when String.exists v ~f:(( = ) '\x00') ->
        let esc = string_to_octal v ~prefix:"\\" in
        mk (rawf "printf -- '%s' > %s" esc tmparg, tmp)
    | Literal_value v ->
        mk (rawf "printf -- '%%s' %s > %s" (Filename.quote v) tmparg, tmp)
    | File p -> mk (rawf ":", make [] (File p))
    | Tmp_file_in_variable p -> mk (rawf "cp \"${%s}\" %s" p tmparg, tmp)
    | Raw_inline s -> mk (rawf "printf -- '%%s' %s > %s" s tmparg, tmp)
    | Octal_value_in_variable _ as v ->
        mk (rawf "printf -- '%%s' %s > %s" (to_argument v) tmparg, tmp)
  in
  match e with
  | Exec l ->
      let irs = List.map ~f:continue l in
      let cmd =
        String.concat ~sep:" "
          (List.map ~f:(fun c -> Script.to_argument c.result) irs)
      in
      let commands = List.concat_map ~f:Script.commands irs in
      Script.unit (commands @ [Raw cmd])
  | Raw_cmd (Some Magic_unit, s) -> Script.unit [Script.rawf "%s" s]
  | Raw_cmd (Some (Octal_in_variable filepathvar), _) ->
      Script.make [] (Octal_value_in_variable filepathvar)
  | Raw_cmd (_, s) -> Script.make [] (Raw_inline s)
  | Byte_array_to_c_string ba ->
      let open Script in
      let script = continue ba in
      let extra_check =
        match script.result with
        | Unit -> assert false
        | Literal_value li when String.exists li ~f:(( = ) '\x00') ->
            fail_commands "Cannot convert literal %S to C-String"
        | Literal_value li -> []
        | File f ->
            [ If_then_else
                { condition=
                    sprintf
                      "od -t o1 -An -v %s | grep ' 000' > /dev/null 2>&1 " f
                ; block_then=
                    ksprintf fail_commands
                      "Byte array in %s cannot be converted to a C-String" f
                ; block_else= [rawf ":"] } ]
        | Tmp_file_in_variable v ->
            [ If_then_else
                { condition=
                    sprintf
                      "od -t o1 -An -v ${%s} | grep ' 000' > /dev/null 2>&1 " v
                ; block_then=
                    ksprintf fail_commands
                      "Byte array in $%s cannot be converted to a C-String" v
                ; block_else= [rawf ":"] } ]
        | Raw_inline ri -> []
        | Octal_value_in_variable v ->
            [ If_then_else
                { condition=
                    sprintf "echo ${%s} | grep ' 000' > /dev/null 2>&1 " v
                ; block_then=
                    ksprintf fail_commands
                      "Byte array in $%s cannot be converted to a C-String" v
                ; block_else= [rawf ":"] } ]
      in
      make (script.commands @ extra_check) script.result
  | C_string_to_byte_array c -> continue c
  | Returns {expr; value} ->
      let es = continue expr in
      let tmp = Script.mktmp ~tmpdb ~expression:e "returns" in
      Script.return_value_to_bool ~tmp es value
  | Bool_operator (a, op, b) ->
      let asc = continue a in
      let bsc = continue b in
      let ops = match op with `And -> "&&" | `Or -> "||" in
      let open Script in
      let condition =
        sprintf "{ %s %s %s ; }" (to_argument asc.result) ops
          (to_argument bsc.result)
      in
      let tmp = mktmp ~tmpdb ~expression:e "boolop" in
      make_bool ~tmp ~condition (asc.commands @ bsc.commands)
  | String_operator (a, op, b) ->
      let asc = continue a in
      let bsc = continue b in
      let ops = match op with `Eq -> "=" | `Neq -> "!=" in
      let open Script in
      let condition =
        sprintf "[ \"%s\" %s \"%s\" ]" (to_ascii asc.result) ops
          (to_ascii bsc.result)
      in
      let tmp = mktmp ~tmpdb ~expression:e "boolop" in
      make_bool ~tmp ~condition (asc.commands @ bsc.commands)
  | No_op -> Script.unit [Raw ":"]
  | If (c, t, e) ->
      let sbool = continue c in
      let sthen = continue t in
      let selse = continue e in
      Script.if_then_else sbool sthen selse
  | While {condition; body} ->
      Script.while_loop (continue condition) (continue body)
  | Seq [] -> continue No_op
  | Seq l ->
      let cmds =
        List.concat_map l ~f:(fun e ->
            let s = continue e in
            match s.result with
            | Unit -> Script.commands s
            | Raw_inline cmd -> Script.(Raw cmd :: commands s)
            | other -> assert false )
      in
      Script.unit cmds
  | Not t ->
      Script.bool_not
        ~tmp:(Script.mktmp ~tmpdb ~expression:e "not")
        (continue t)
  | Redirect_output (unit_t, redirections) ->
      let pre_commands, sub_shell_commands =
        let open Script in
        List.fold ~init:([], []) redirections
          ~f:(fun (precmds, evals) {take; redirect_to} ->
            let take_script = continue take in
            let redirect_to_script, op =
              match redirect_to with
              | `Fd c -> (continue c, "&")
              | `Path p -> (continue p, "")
            in
            let print_exec_command =
              sprintf "printf 'exec %%s>%s%%s' %s %s" op
                (to_argument take_script.result)
                (to_argument redirect_to_script.result)
            in
            ( precmds @ take_script.commands @ redirect_to_script.commands
            , evals @ [rawf "eval $(%s)" print_exec_command] ) )
      in
      let uscript = continue unit_t in
      Script.assert_unit uscript ;
      Script.sub_shell ~pre:pre_commands (sub_shell_commands @ uscript.commands)
  | Write_output {expr; stdout; stderr; return_value} ->
      let retscript = Option.map return_value ~f:(fun v -> continue v) in
      let pre_ret_value, with_potential_return =
        match retscript with
        | None -> ([], expr)
        | Some scr ->
            ( scr.commands
            , Seq
                [ expr
                ; Comment
                    ( "Writing return value"
                    , Raw_cmd
                        ( None
                        , sprintf "printf -- \"$?\" > %s"
                            Script.(to_argument scr.result) ) ) ] )
      in
      let redirections =
        let make fd =
          Option.map ~f:(fun p -> {take= Construct.int fd; redirect_to= `Path p}
          )
        in
        [make 1 stdout; make 2 stderr] |> List.filter_opt
      in
      let redscript =
        continue (Redirect_output (with_potential_return, redirections))
      in
      Script.assert_unit redscript ;
      Script.unit (pre_ret_value @ redscript.commands)
  | Literal lit ->
      Script.literal_value
        Literal.(
          match lit with
          | Int i -> string_of_int i
          | String s -> s
          | Bool s -> string_of_bool s)
  | Output_as_string e ->
      let open Script in
      let ir = continue e in
      let cmds = commands ir in
      assert_unit ir ;
      let stdout = mktmp ~tmpdb ~expression:e "out2str" in
      redirect cmds ~stdout
  | Int_to_string i -> continue i
  | String_to_int s ->
      let open Script in
      let string_script = continue s in
      let check =
        continue
          (If
             ( Int_bin_comparison
                 ( Raw_cmd (None, to_argument string_script.result)
                 , `Eq
                 , Raw_cmd (None, to_argument string_script.result) )
             , No_op
             , Fail "string-to-int" ))
      in
      make (string_script.commands @ check.commands) string_script.result
  | Bool_to_string b ->
      let open Script in
      let bs = continue b in
      with_tmp ~tmpdb ~expression:e "bool-to-string" (fun tmp ->
          let tmparg = to_path_argument tmp.result in
          let extra =
            If_then_else
              { condition= to_argument bs.result
              ; block_then= [rawf "printf true > %s" tmparg]
              ; block_else= [rawf "printf false > %s" tmparg] }
          in
          bs.commands @ [extra] )
  | String_to_bool s ->
      let scr = continue s in
      let extra_check =
        let is v =
          String_operator
            ( Raw_cmd (None, Script.to_ascii scr.result)
            , `Eq
            , Literal Literal.(String v) )
        in
        If
          ( Bool_operator (is "true", `Or, is "false")
          , No_op
          , Fail (sprintf "String-to-Bool: %S" (Script.to_argument scr.result))
          )
      in
      let check = continue extra_check in
      Script.make (scr.commands @ check.commands) scr.result
  | List l ->
      let scripts = List.map ~f:continue l in
      let open Script in
      with_tmp ~tmpdb ~expression:e "list-make" (fun tmp ->
          let tmparg = to_path_argument tmp.result in
          let echos =
            let open Script in
            rawf "rm -f %s" tmparg
            ::
            ( match scripts with
            | [] -> [rawf "touch %s" tmparg]
            | _ ->
                List.concat_map scripts ~f:(fun s ->
                    let as_file = result_to_file s in
                    let as_arg = to_path_argument as_file.result in
                    (*
                       Stuff is put in a file and then “octaled”
                       We also prefix with `L` to make sure we “count”
                       empty strings.
                    *)
                    as_file.commands
                    @ [ rawf
                          "printf ' L%%s\\n' \"$(cat %s  | od -t o1 -An -v | \
                           tr -d ' \\n')\" >> %s"
                          as_arg tmparg ] ) )
          in
          List.concat_map scripts ~f:(fun c -> c.commands) @ echos )
  | List_to_string (l, f) -> continue l
  | String_to_list (s, f) -> continue s
  | C_string_concat sl ->
      let sl_script = continue sl in
      concat_string_list sl_script
  | Byte_array_concat sl ->
      let sl_script = continue sl in
      concat_string_list sl_script
  | List_append (la, lb) ->
      let open Script in
      let a_script = continue la in
      let b_script = continue lb in
      with_tmp ~tmpdb ~expression:e "list-append" (fun tmp ->
          let cat =
            rawf "cat %s %s > %s"
              (to_path_argument a_script.result)
              (to_path_argument b_script.result)
              (to_path_argument tmp.result)
          in
          a_script.commands @ b_script.commands @ [cat] )
  | List_iter (l, f) ->
      let open Script in
      let l_script = continue l in
      let list_file = to_path_argument l_script.result in
      let file_var = var_name ~expression:e "list_iter" in
      (* We iterate on the list of paths, for each path we pass the
         contents through the transformation function `f` and append the
         result to `tmp`. *)
      let convert_script =
        continue
          (f (fun () ->
               Raw_cmd
                 ( Some (Octal_in_variable file_var)
                 , "=== This should never be used ===" ) ))
      in
      let loop =
        [ Script.rawf ": list_iter ; for %s in $(cat %s) ; do\n{\n%s\n}\ndone"
            file_var list_file
            (Format.asprintf "%a" Script.pp_posix convert_script) ]
      in
      unit (l_script.commands @ loop)
  | Int_bin_op (ia, op, ib) ->
      let open Script in
      let a_script = continue ia in
      let b_script = continue ib in
      with_tmp ~tmpdb ~expression:e "list-append" (fun tmp ->
          let compute =
            rawf "printf -- \"$(( %s %s %s ))\" > %s"
              (to_argument ~arithmetic:true a_script.result)
              ( match op with
              | `Div -> "/"
              | `Minus -> "-"
              | `Mult -> "*"
              | `Plus -> "+"
              | `Mod -> "%" )
              (to_argument ~arithmetic:true b_script.result)
              (to_path_argument tmp.result)
          in
          a_script.commands @ b_script.commands @ [compute] )
  | Int_bin_comparison (ia, op, ib) ->
      let open Script in
      let a_script = continue ia in
      let b_script = continue ib in
      with_tmp ~tmpdb ~expression:e "int-bin-comparison" (fun tmp ->
          let compute =
            rawf
              "{ if [ %s %s %s ] ; then printf true ; else printf false ; fi \
               ; } > %s"
              (to_argument a_script.result)
              ( match op with
              | `Eq -> "-eq"
              | `Ge -> "-ge"
              | `Gt -> "-gt"
              | `Le -> "-le"
              | `Lt -> "-lt"
              | `Ne -> "-ne" )
              (to_argument b_script.result)
              (to_path_argument tmp.result)
          in
          a_script.commands @ b_script.commands @ [compute] )
  | Feed (string, u) ->
      let open Script in
      let string_script = continue string in
      let u_script = continue u in
      let as_file = result_to_file string_script in
      let filearg = to_path_argument as_file.result in
      unit
        ( string_script.commands @ as_file.commands
        @ [Pipe {blocks= [[rawf "cat %s" filearg]; u_script.commands]}] )
  | Pipe l ->
      let open Script in
      let scripts = List.map ~f:continue l in
      List.iter scripts ~f:assert_unit ;
      let blocks = List.map scripts ~f:(fun c -> c.commands) in
      unit [Pipe {blocks}]
  | Getenv s ->
      let open Script in
      let string_script = continue s in
      with_tmp ~tmpdb ~expression:e ~script:string_script "getenv" (fun tmp ->
          string_script.commands
          @ [ rawf "eval 'printf \"%%s\" \"$'%s'\"' > %s"
                (to_argument ~arithmetic:false string_script.result)
                (to_path_argument tmp.result) ] )
  | Setenv (variable, value) ->
      let open Script in
      let var_script = continue variable in
      let val_script = continue value in
      let val_as_file = val_script |> result_to_file in
      (* let tmp = tmp_path ~tmpdb ~expression:e "setenv" in *)
      let cmd =
        rawf "eval 'export '%s'=\"$(cat %s)\"'"
          (to_argument ~arithmetic:false var_script.result)
          (to_path_argument val_as_file.result)
      in
      make
        ( var_script.commands @ val_script.commands @ val_as_file.commands
        @ [cmd] )
        Unit
  | Fail s -> Script.unit (fail_commands s)
  | Comment (cmt, expr) ->
      let open Script in
      let script = continue expr in
      make (Comment cmt :: script.commands) script.result

(*md

The main entry point (still compilation from a `'a Language.t` to the
intermediary representation) but higher level.

It is accessed through `Compile.To_slow_flow.compile`:

```ocaml
  val compile :
       ?default_tmpdir:[`Fresh | `Use of string]
    -> ?signal_name:string
    -> ?trap:[`Exit_with of int | `None]
    -> 'a EDSL.t
    -> Script.t
  (** Compile and {!EDSL.t} value to a script. *)
```
*)
let compile ?(default_tmpdir = `Fresh) ?(signal_name = "USR1")
    ?(trap = `Exit_with 77) expr =
  let open Script in
  let tmpdb = Tmp_db.make default_tmpdir in
  let pid = var_name ~expression:expr "script_pid" in
  let tmp = mktmp ~tmpdb ~expression:expr "fail-msg" in
  let tmparg = to_path_argument tmp.result in
  let delete_fname = var_name ~expression:expr "cleanup" in
  let before =
    [ rawf "export %s=$$" pid
    ; ( match trap with
      | `None -> rawf ": No TRAP"
      | `Exit_with v ->
          rawf "trap 'cat %s >&2 ; %s ; exit %d' %s" tmparg delete_fname v
            signal_name ) ]
  in
  let fail_commands s =
    match trap with
    | `Exit_with _ ->
        [ rawf "printf '%%s\\n' %s > %s " (Filename.quote s) tmparg
        ; rawf "kill -s %s ${%s}" signal_name pid ]
    | `None ->
        failwith "You cannot use the `fail` construct with no `trap` strategy"
  in
  let s = to_ir ~fail_commands ~tmpdb expr in
  let delete_tmps = Tmp_db.delete_function tmpdb delete_fname |> rawf "%s" in
  let make_tmp_vars =
    Tmp_db.tmp_vars tmpdb
    |> List.concat_map ~f:(fun (v, dir, cmd) ->
           [cmtf "Making file %s" v; Make_directory dir; rawf "%s" cmd] )
  in
  let last_call = rawf "ret=$?\n%s\nexit $ret\n" delete_fname in
  make
    ( [delete_tmps] @ make_tmp_vars @ tmp.commands @ before @ s.commands
    @ [last_call] )
    s.result

(*md

Extra tests which can be activated for debugging purposes (option
`--run-slow-stack-tests` in the main tests).

*)
let test () =
  let open Format in
  let open Language.Construct in
  let exprs =
    let printf c l = call ([c_string "printf"; c] @ l) in
    let one = seq [call [c_string "printf"; c_string "hello"]; nop] in
    let two = call [c_string "echo"; c_string "echo"] in
    let three = call [c_string "printf"; c_string " world"] in
    [ one
    ; call [c_string "echo"; get_stdout one |> Byte_array.to_c_string]
    ; call
        [ get_stdout two |> Byte_array.to_c_string
        ; get_stdout (seq [one; three]) |> Byte_array.to_c_string ]
    ; if_then_else Byte_array.(get_stdout one =$= byte_array "hello") one three
    ; loop_seq_while
        ((not (bool true)) ||| returns ~value:0 (exec ["ls"; "/crazypath"]))
        [three]
    ; seq
        [ with_redirections
            (seq [exec ["ls"; "/crazypath"]; printf (c_string "HELLO") []])
            [ to_file (int 1) (c_string "/tmp/testgenspio5")
            ; to_fd (int 2) (int 1) ]
        ; printf (c_string "NOW CAT:\\n") []
        ; exec ["cat"; "/tmp/testgenspio5"] ]
    ; seq
        [ write_output
            ~stderr:(c_string "/tmp/testgenspio6-err")
            ~return_value:
              ( get_stdout (exec ["printf"; "/tmp/testgenspio6-ret"])
              |> Byte_array.to_c )
            (seq
               [ printf (c_string "hello test 6\n") []
               ; exec ["ls"; "/crazypath"] ])
        ; printf
            (c_string "ERR: <<%s>>\\nRET: <<%s>>\\n")
            [ get_stdout (exec ["cat"; "/tmp/testgenspio6-err"])
              |> Byte_array.to_c
            ; get_stdout (exec ["cat"; "/tmp/testgenspio6-ret"])
              |> Byte_array.to_c ] ]
    ; seq
        [ write_output
            ~stdout:(c_string "/tmp/testgenspio-7-out")
            (printf (c_string "s:%s:")
               [ C_string.concat_elist
                   (Elist.make
                      [c_string "hello"; c_string " "; c_string "world"]) ])
        ; if_then_else
            Byte_array.(
              get_stdout (exec ["cat"; "/tmp/testgenspio-7-out"])
              =$= byte_array "s:hello world:")
            (printf (c_string "SUCCESS\\n") [])
            (printf (c_string "FAILURE\\n") []) ]
    ; printf (c_string "`%s`")
        [ C_string.concat_elist
            Elist.(
              append
                (make [c_string "hel"; c_string "lo"])
                (append
                   (make [c_string " "; c_string "w"])
                   (make [c_string "orl"; c_string "d"]))) ]
    ; Elist.(
        iter
          (make
             [ int 1
             ; int 2
             ; Integer.(int 1 + int 2)
             ; Integer.(int 1 + int 1 + of_string (c_string "2")) ])
          ~f:(fun item ->
            printf (c_string "> %d\\n") [Integer.to_string (item ())] ))
    ; if_then_else
        Integer.(
          int 1 + int 1 + of_string (c_string "2")
          = int 4
          &&& (int 2 * int 2 = int 8 / int 2))
        (printf (c_string "SUCCESS\\n") [])
        (printf (c_string "FAILURE\\n") [])
    ; byte_array "Hello World" >> exec ["cat"]
    ; pipe
        [ printf (c_string "HELLX_WXRLD") []
        ; exec ["sed"; "s/_/ /g"]
        ; exec ["tr"; "X"; "O"] ]
    ; printf
        (c_string "HOME: '%s'\\nPWD: '%s'")
        [ getenv (c_string "HOME")
        ; getenv
            (C_string.concat_list [c_string "P"; c_string "W"; c_string "D"])
        ]
    ; seq
        [ setenv
            (C_string.concat_list [c_string "A"; c_string "A"; c_string "A"])
            (get_stdout (exec ["echo"; "HELLO WORLD"]) |> Byte_array.to_c)
        ; "Calling a sub-shell with `sh`,\nit should display HELLO WORLD"
          %%% exec ["sh"; "-c"; "echo \"$AAA\""] ]
    ; seq [printf (c_string "Returns 77?\\n") []; fail "Should fail indeed"]
    ; if_then_else
        (Bool.to_string (bool true) |> Bool.of_string)
        (printf (c_string "SUCESSSSS\\n") [])
        (printf (c_string "FAIL FAIL FAIL\\n") [])
    ; (let var = c_string "AAA" in
       let v1 = c_string "V1" in
       let v2 = c_string "V2" in
       seq
         [ setenv var v1
         ; loop_seq_while
             C_string.(getenv var =$= v1)
             [printf (c_string "Iteration\\n") []; setenv var v2] ]) ]
  in
  List.iteri exprs ~f:(fun idx expr ->
      let ir = compile expr in
      fprintf std_formatter "==== TEST %d ====\n%a\n%!" idx Script.pp_posix ir ;
      let script_file = sprintf "/tmp/script-%d.sh" idx in
      let o = open_out script_file in
      fprintf (formatter_of_out_channel o) "\n%a\n%!" Script.pp_posix ir ;
      flush o ;
      close_out o ;
      let res = ksprintf Sys.command "sh %s" script_file in
      fprintf std_formatter "\nRESULT: %d\n" res )
