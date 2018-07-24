open Common

type 'a t = 'a Language.t

let default_max_argument_length = Some 100_000

module To_posix = struct
  type internal_error_details = Language.internal_error_details =
    {variable: string; content: string; code: string}

  type death_message = Language.death_message =
    | User of string
    | C_string_failure of internal_error_details
    | String_to_int_failure of internal_error_details

  type death_function = comment_stack:string list -> death_message -> string

  type compilation_error = Language.compilation_error =
    { error:
        [ `No_fail_configured of death_message (* Argument of fail *)
        | `Max_argument_length of string (* Incriminated argument *)
        | `Not_a_c_string of string (* The actual string *) ]
    ; code: string option
    ; comment_backtrace: string list }

  let pp_error = Language.pp_error

  let error_to_string = Format.asprintf "%a" pp_error

  type parameters =
    { style: [`One_liner | `Multi_line]
    ; max_argument_length: int option
    ; fail_with: [`Nothing | `Trap_and_kill of int * string | `Kill of string]
    ; print_failure: death_function }

  let failure_to_stderr : death_function =
   fun ~comment_stack msg ->
    let summary s =
      match String.sub s 0 65 with Some s -> s ^ " …" | None -> s
    in
    let open Format in
    let big_string fmt s = Format.fprintf fmt "@[%s@]" (summary s) in
    let msg_str =
      Format.asprintf "@[Error:@ @[%a@]%a@]"
        (Language.pp_death_message ~style:`User ~big_string)
        msg
        (fun fmt () ->
          match comment_stack with
          | [] -> fprintf fmt ""
          | more ->
              fprintf fmt ";@ Comment-stack:@ @[[%a]@]"
                (pp_print_list
                   ~pp_sep:(fun fmt () -> fprintf fmt ",@ ")
                   (fun fmt s -> fprintf fmt "@[`%s`@]" s))
                more )
        ()
      |> Filename.quote
    in
    asprintf " printf -- '%%s\\n' %s >&2 " msg_str

  let one_liner =
    { style= `One_liner
    ; max_argument_length= Some 100_000
    ; fail_with= `Trap_and_kill (78, "USR2")
    ; print_failure= failure_to_stderr }

  let multi_line = {one_liner with style= `Multi_line}

  let default_options = one_liner

  let string_exn ?(options= default_options) term =
    let statement_separator =
      match options.style with `Multi_line -> "\n" | `One_liner -> " ; "
    in
    let {max_argument_length; print_failure} = options in
    let open Language in
    match options.fail_with with
    | `Nothing ->
        to_shell
          {statement_separator; die_command= None; max_argument_length}
          term
    | `Kill signal_name ->
        with_die_function ~print_failure ~statement_separator ~signal_name
          (fun ~die ->
            to_shell
              {statement_separator; die_command= Some die; max_argument_length}
              term )
    | `Trap_and_kill (ret, signal) ->
        with_die_function ~print_failure ~statement_separator
          ~signal_name:signal ~trap:(`Exit_with ret) (fun ~die ->
            to_shell
              {statement_separator; die_command= Some die; max_argument_length}
              term )

  let string ?options term =
    match string_exn ?options term with
    | s -> Ok s
    | exception Language.Compilation ce -> Error ce
end

let to_legacy style ?(max_argument_length= default_max_argument_length)
    ?(no_trap= false) e =
  To_posix.string e
    ~options:
      { style
      ; max_argument_length
      ; fail_with= (if no_trap then `Nothing else `Trap_and_kill (77, "USR1"))
      ; print_failure= To_posix.failure_to_stderr }
  |> function Ok s -> s | Error e -> failwith @@ To_posix.error_to_string e

let to_one_liner ?max_argument_length ?no_trap e =
  to_legacy `One_liner ?max_argument_length ?no_trap e

let to_many_lines ?max_argument_length ?no_trap e =
  to_legacy `Multi_line ?max_argument_length ?no_trap e

let quick_run_exn ?max_argument_length ?no_trap e =
  match to_many_lines ?max_argument_length ?no_trap e |> Sys.command with
  | 0 -> ()
  | other -> ksprintf failwith "Command returned %d" other

let pp_hum = Language.pp

let to_string_hum e = Format.asprintf "%a" pp_hum e

let to_one_line_hum e =
  let buf = Buffer.create 42 in
  let formatter = Format.formatter_of_buffer buf in
  Format.pp_set_margin formatter 10_000_000 ;
  Format.fprintf formatter "@[<h>%a@]@?" pp_hum e ;
  Buffer.contents buf
