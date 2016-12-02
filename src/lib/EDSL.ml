type 'a t = 'a Language.t
type 'a cli_option = 'a Language.cli_option
type 'a option_spec = 'a Language.option_spec
type ('a, 'b) cli_options = ('a, 'b) Language.cli_options

include Language.Construct

open Nonstd

let case condition body = `Case (condition, seq body)
let default d = `Default (seq d)
let switch l =
  let default = ref None in
  let cases =
    List.filter_map l ~f:(function
      | `Default d when !default <> None ->
        failwith "Cannot build switch with >1 defaults"
      | `Default d -> default := (Some d); None
      | `Case t -> Some t)
  in
  make_switch ~default:(Option.value ~default:nop !default) cases


