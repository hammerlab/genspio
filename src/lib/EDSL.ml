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

let string_concat sl =
  (* This is a pretty unefficient implementation: *)
  let out s = call [string "printf"; string "%s"; s] in
  seq (List.map sl ~f:out) |> output_as_string

let tmp_file ?(tmp_dir = string "/tmp") name =
  let path =
    let clean =
      String.map name ~f:(function
        | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '_' | '-' as c -> c
        | other -> '_') in
    string_concat [
      tmp_dir;
      string "/";
      string
        (sprintf "genspio-tmp-file-%s-%s" clean Digest.(string name |> to_hex));
    ]
  in
  let tmp = string_concat [path; string "-tmp"] in
  object
    method get = output_as_string (call [string "cat"; path])
    method set v =
      seq [
        call [string "echo"; string "Setting"];
        call [string "echo"; tmp];
        v >> exec ["cat"] |> write_output ~stdout:tmp;
        call [string "mv"; string "-f"; tmp; path];
      ]
  end

let if_seq ~t ?e c =
  match e with
  | None -> if_then c (seq t)
  | Some f -> if_then_else c (seq t) (seq f) 
