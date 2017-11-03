open Nonstd
module String = Sosa.Native_string

let examples = ref ([]: (out_channel -> unit) list)
let example ?show name description code =
  let f o =
    fprintf o
      "let () = examples := Example.make ~ocaml:%S %s %S %S %s :: !examples\n" code
      (match show with
      | None -> ""
      | Some s -> sprintf "~show:%s" s)
      name description code in
  examples := f :: !examples

let intro_blob =
  "EDSL Usage Examples\n\
   ===================\n\
   \n\
   The following examples show gradually complex uses of the EDSL.\n\
  "

let () =
  example "Exec"
    "Simple call to the `exec` construct."
{ocaml|
Genspio.EDSL.(
  exec ["ls"; "-la"]
)
|ocaml}

let () =
  example "Exec with Comment" ~show:"[`Pretty_printed; `Compiled]"
    "Adding comments with the `%%%` operator, we can see them in the \
     compiled output."
{ocaml|
Genspio.EDSL.(
  "This is a very simple command" %%%
  exec ["ls"; "-la"]
)
|ocaml}

let () =
  example ~show:"[`Stderr]" "Failure with Comment"
    "When an expression is wrapped with *“comments”* they also appear in \
     error messages (compilation *and* run-time) as “the comment stack.”"
{ocaml|
Genspio.EDSL.(
  "This is a very simple comment" %%% seq [
    exec ["ls"; "-la"];
    fail "asserting False ☺";
  ]
)
|ocaml}

let () =
  example "Call a command with C-Strings"
    ~show:"[`Stdout; `Pretty_printed]"
    "The `call` construct is a more general version of `exec` that can take \
     any EDSL string."
{ocaml|
Genspio.EDSL.(
  call [
    string "echo";
    string_concat [string "foo"; string "bar"]; (* A concatenation at run-time. *)
  ]
)
|ocaml}

let () =
  example "C-String Compilation Failure" ~show:"[]"
    "When a string literal cannot be converted to a “C-String” the compiler \
     tries to catch the error at compile-time."
{ocaml|
Genspio.EDSL.(
  "A sequence that will fail" %%% seq [
    call [string "ls"; string "foo\x00bar"]; (* A string containing `NUL` *)
  ]
)
|ocaml}

let () =
  let o = open_out Sys.argv.(1) in
  fprintf o "%s" {ocaml|
open Nonstd
module String = Sosa.Native_string
open Tests.Test_lib

let examples = ref []
|ocaml};
  fprintf o "let () = printf \"%%s\" %S\n" intro_blob;
  List.iter (List.rev !examples) ~f:(fun f -> f o);
  fprintf o "%s" {ocaml|
let () =
    List.iter (List.rev !examples) ~f:(Example.run Format.std_formatter)
|ocaml};
  close_out o;
  printf "%s: Done.\n%!" Sys.argv.(0)
