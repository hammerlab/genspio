type 'a t = 'a Language.t
type 'a cli_option = 'a Language.cli_option
type 'a option_spec = 'a Language.option_spec
type ('a, 'b) cli_options = ('a, 'b) Language.cli_options
let (//) = Filename.concat

include Language.Construct

open Nonstd
module String = Sosa.Native_string

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


let tmp_file ?(tmp_dir = "/tmp") name =
  let path =
    let clean =
      String.map name ~f:(function
        | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '_' | '-' as c -> c
        | other -> '_') in
    tmp_dir
    // sprintf "genspio-tmp-file-%s-%s" clean Digest.(string name |> to_hex) in
  let tmp = path ^ "-tmp" |> string in
  object
    method get = output_as_string (exec ["cat"; path])
    method set v =
      seq [
        v >> exec ["cat"] |> write_output ~stdout:tmp;
        call [string "mv"; string "-f"; tmp; string path];
      ]
  end
