
type 'a t = 'a Language.t


let default_max_argument_length = Some 100_000

module To_posix = struct

  type parameters = {
    style: [ `One_liner | `Multi_line ];
    max_argument_length: int option;
    fail_with: [
      | `Nothing
      | `Trap_and_kill of int * string
      | `Kill of string
    ];
  }

  let one_liner = {
    style = `One_liner;
    max_argument_length = Some 100_000;
    fail_with = `Trap_and_kill (78, "USR2");
  }
  let multi_line = {one_liner with style = `Multi_line }
  let default_options = one_liner

  let string ?(options = default_options) term =
    let statement_separator =
      match options.style with
      | `Multi_line -> "\n"
      | `One_liner -> " ; " in
    let {max_argument_length} = options in
    let open Language in
    match options.fail_with with
    | `Nothing ->
      to_shell {statement_separator; die_command = None; max_argument_length} term
    | `Kill signal_name ->
      with_die_function ~statement_separator ~signal_name (fun ~die ->
          to_shell {statement_separator;
                    die_command = Some die;
                    max_argument_length} term)
    | `Trap_and_kill (ret, signal) ->
      with_die_function ~statement_separator ~signal_name:signal
        ~trap:(`Exit_with ret)
        (fun ~die ->
           to_shell {statement_separator;
                     die_command = Some die;
                     max_argument_length} term)
end

let to_legacy style
    ?(max_argument_length = default_max_argument_length) ?(no_trap = false) e =
  To_posix.string e ~options:{style; max_argument_length;
    fail_with = begin
      if no_trap then `Nothing
      else `Trap_and_kill (77, "USR1")
    end}

let to_one_liner ?max_argument_length ?no_trap e =
  to_legacy `One_liner
    ?max_argument_length ?no_trap e
let to_many_lines ?max_argument_length ?no_trap e =
  to_legacy `Multi_line
    ?max_argument_length ?no_trap e



let pp_hum = Language.pp
let to_string_hum e = Format.asprintf "%a" pp_hum e