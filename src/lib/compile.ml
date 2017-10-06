
type 'a t = 'a Language.t


let default_max_argument_length = Language.default_max_argument_length
let to_one_liner = Language.to_one_liner

let to_many_lines = Language.to_many_lines


let pp_hum = Language.pp
let to_string_hum e = Format.asprintf "%a" pp_hum e