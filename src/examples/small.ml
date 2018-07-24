open Nonstd
module String = Sosa.Native_string

let examples = ref ([] : (out_channel -> unit) list)

let example ?show name description code =
  let f o =
    fprintf o
      "let () = examples := Example.make ~ocaml:%S %s %S %S %s :: !examples\n"
      code
      (match show with None -> "" | Some s -> sprintf "~show:%s" s)
      name description code
  in
  examples := f :: !examples

let intro_blob =
  "EDSL Usage Examples\n\
   ===================\n\n\
   The following examples show gradually complex uses of the EDSL.\n"

let () =
  example "Exec" "Simple call to the `exec` construct."
    {ocaml|
Genspio.EDSL.(
  exec ["ls"; "-la"]
)
|ocaml}

let () =
  example "Exec with Comment" ~show:"[`Pretty_printed; `Compiled]"
    "Adding comments with the `%%%` operator, we can see them in the compiled \
     output."
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
    "This comment provides a more precise pseudo-location" %%% seq [
       (* Here we use the `fail` EDSL facility: *)
       fail "asserting False ☺";
    ];
  ]
)
|ocaml}

let () =
  example "Call a command with C-Strings" ~show:"[`Stdout; `Pretty_printed]"
    "The `call` construct is a more general version of `exec` that can take \
     any EDSL string. As with `exec` the string will be checked for C-String \
     compatibilty, hence the calls to `byte-array-to-c-string` in the \
     pretty-printed output."
    {ocaml|
Genspio.EDSL.(
  call [
    string "echo";
    C_string.concat_list [string "foo"; string "bar"]; (* A concatenation at run-time. *)
  ]
)
|ocaml}

let () =
  example "C-String Compilation Failure" ~show:"[]"
    "When a string literal cannot be converted to a “C-String” the \
     compiler tries to catch the error at compile-time."
    {ocaml|
Genspio.EDSL.(
  "A sequence that will fail" %%% seq [
    call [string "ls"; string "foo\x00bar"]; (* A string containing `NUL` *)
  ]
)
|ocaml}

let () =
  example "Playing with the output of a command"
    ~show:"[`Pretty_printed; `Stdout]"
    {md|Here we use the constructs:

```ocaml
val get_stdout : unit t -> byte_array t
val Byte_array.to_c: byte_array t -> c_string t
val (||>) : unit t -> unit t -> unit t
```

We use `let (s : …) = …` to show the types; we see then that we need to “cast”
the output to a C-String with `Byte_array.to_c` in order to pass it to `call`.
Indeed, commands can output arbitrary byte-arrays but Unix commands
only accept `NUL`-terminated strings.

We then “pipe” the output to another `exec` call with `||>` (which is
a 2-argument shortcut for `EDSL.pipe`).
|md}
    {ocaml|
Genspio.EDSL.(
  let (s : byte_array t) = get_stdout (exec ["cat"; "README.md"]) in
  call [string "printf"; string "%s"; Byte_array.to_c s] ||> exec ["wc"; "-l"];
)
|ocaml}

let () =
  example "Feeding a string to a command's stdin"
    ~show:"[`Pretty_printed; `Stdout]"
    "The operator `>>` puts any byte-array into the `stdin` of any `unit t` \
     expression."
    {ocaml|
Genspio.EDSL.(
  (* Let's see wether `wc -l` is fine with a NUL in the middle of a “line:” *)
  byte_array "one\ntwo\nth\000ree\n" >> exec ["wc"; "-l"];
)
|ocaml}

let () =
  example "Comparing byte-arrays, using conditionals"
    ~show:"[`Pretty_printed; `Stdout]"
    "We show that `byte-array >> cat` is not changing anything and we try \
     `if_seq`; a version of `EDSL.if_then_else` more practical for \
     sequences/imperative code."
    {ocaml|
Genspio.EDSL.(
    (* With a 🐱: *)
  let original = byte_array "one\ntwo\nth\000ree\n" in
  let full_cycle = original >> exec ["cat"] |> get_stdout in
  if_seq
    Byte_array.(full_cycle =$= original)
    ~t:[
      exec ["echo"; "They are the same"];
    ]
    ~e:[
      exec ["echo"; "They are NOT the same"];
    ]
)
|ocaml}

let () =
  example "“While” loops" ~show:"[`Stdout]"
    "The default and simplest loop construct is `loop_while`, the EDSL has \
     also a simple API to manage temporary files and use them as \
     pseudo-global-variables."
    {ocaml|
Genspio.EDSL.(
  let tmp = tmp_file "genspio-example" in
  let body =
    seq [
      if_then_else C_string.(tmp#get_c =$= c_string "")
         (tmp#set_c (c_string "magic-"))
         (if_then_else C_string.(tmp#get_c =$= string "magic-")
            (tmp#append (c_string "string" |> C_string.to_bytes))
            nop);
      call [string "printf"; string "Currently '%s'\\n"; tmp#get_c];
    ] in
  seq [
    tmp#set (byte_array "");
    loop_while C_string.(tmp#get_c <$> c_string "magic-string") ~body
  ]
)
|ocaml}

let () =
  example "Arbitrary Redirections" ~show:"[`Pretty_printed; `Stdout]"
    {md|The function `EDSL.with_redirections` follows POSIX's `exec`
[semantics](http://pubs.opengroup.org/onlinepubs/9699919799/utilities/V3_chap02.html#exec).

The `printf` call will output to the file `/tmp/genspio-two` because
redirections are set in that order:

- file-descriptor `3` is set to output to `/tmp/genspio-one`,
- file-descriptor `3` is *then* set to output to `/tmp/genspio-two`
  (overriding the previous redirection),
- file-descriptor `2` is redirected to file-descriptor `3`,
- file-descriptor `1` is redirected to file-descriptor `2`,
- then, `printf` outputs to `1`.
|md}
    {ocaml|
Genspio.EDSL.(
  seq [
    with_redirections (exec ["printf"; "%s"; "hello"]) [
      to_file (int 3) (string "/tmp/genspio-one");
      to_file (int 3) (string "/tmp/genspio-two");
      to_fd (int 2) (int 3);
      to_fd (int 1) (int 2);
    ];
    call [string "printf"; string "One: '%s'\\nTwo: '%s'\\n";
          exec ["cat"; "/tmp/genspio-one"] |> get_stdout |> Byte_array.to_c;
          exec ["cat"; "/tmp/genspio-two"] |> get_stdout |> Byte_array.to_c];
  ]
)
|ocaml}

let () =
  example "Lists" ~show:"[`Pretty_printed; `Stdout]"
    {md|The module `EList` provides lists within the EDSL.

|md}
    {ocaml|
Genspio.EDSL.(
  let l = Elist.make [
    c_string "One";
    c_string "Two";
  ] in
  Elist.iter l ~f:begin fun current ->
    printf (c_string "Current: %s\\n") [current ()];
  end
)
|ocaml}

let () =
  example "Loop until something is true" ~show:"[`Stdout]"
    {md|The EDSL also provides high-level utilities implemented with
the API (like a standard library).

Here is an example with `loop_until_true` that fails after 4 attempts
(i.e. (4 - 1) × 1 = 3 seconds),
unless there is line containing `godot` in `/etc/passwd`.

We customize the output with an `~on_failed_attempt` function that (on
most terminals) erases the previous display (with `\r`).

<div><a href="https://user-images.githubusercontent.com/617111/33687734-09f78c48-daa7-11e7-9a49-4c8fd8a07f24.gif"><img
 width="80%"
  src="https://user-images.githubusercontent.com/617111/33687734-09f78c48-daa7-11e7-9a49-4c8fd8a07f24.gif"
></a></div>

|md}
    {ocaml|
Genspio.EDSL.(
  let the_condition who =
    exec ["cat"; "/etc/passwd"] ||> exec ["grep"; "^" ^ who]
    |> returns ~value:0
  in
  let the_wait who =
    loop_until_true
      ~attempts:4
      ~sleep:1
      ~on_failed_attempt:(fun nth ->
        printf (string "\rWaiting for '%s: %s-th attempt.")
         [c_string who; Integer.to_string nth])
      (the_condition who)
  in
  if_seq (the_wait "godot") ~t:[
      printf (c_string "It was worth waiting\\n") [];
    ]
     ~e:[
      printf (c_string "It was NOT worth waiting\\n") [];
    ]
  )
|ocaml}

let () =
  example "Check Sequence" ~show:"[`Stdout]"
    {md|Another function from the “extra constructs:”
[`check_sequence`](genspio/Genspio/EDSL/index.html#val-check_sequence).

We customize its output with the `~verbosity` (by adding a nice prompt) and
`~on_success` arguments.
|md}
    {ocaml|
Genspio.EDSL.(
   check_sequence
     ~verbosity:(`Announce "♦ Check-seq-example → ") (* Try also `Output_all or `Silent *)
     ~on_success:begin fun ~step:(name, expr) ~stdout ~stderr ->
       let code = Genspio.Compile.to_one_line_hum expr in
       printf (c_string "  ↳ Extra “On Success” for command `%s`\\n\
                        \    code: `%s`\\n\
                        \    stdout: `%s`\\n\
                        \    stderr: `%s`\\n")
          [c_string name; c_string code; stdout; stderr]
     end
     [
        "This will succeed", exec ["ls"; "/tmp"];
        "This too", exec ["ls"; "/"];
        "BUT NOT THIS", exec ["ls"; "/somecrazy path"];
        "This won't happen", exec ["ls"; "/etc"];
     ]
)
|ocaml}

let () =
  example "Read `stdin` Line by Line" ~show:"[`Stdout]"
    {md|Let's try now the
[`on_stdin_lines`](genspio/Genspio/EDSL/index.html#val-on_stdin_lines)
function, to read a *stream* of lines.

Note that for the word “lines” to really make sense, the input should
be proper “text,” in the example below the `'\000'` character is just
silently forgotten, not counted.
|md}
    {ocaml|
Genspio.EDSL.(
  printf (c_string "123\\n12345\\n1234\\00056\\n12\\n") []
  ||> on_stdin_lines begin fun line ->
    printf (c_string "→ %s bytes\\n")
      [C_string.to_byte_array line
       >> exec ["wc"; "-c"] ||> exec ["tr"; "-d"; "\\n"]
       |> get_stdout |> Byte_array.to_c]
  end
)
|ocaml}

(******************************************************************************)

let () =
  let o = open_out Sys.argv.(1) in
  fprintf o "%s"
    {ocaml|
open Nonstd
module String = Sosa.Native_string
open Tests.Test_lib

let examples = ref []
|ocaml} ;
  fprintf o "let () = printf \"%%s\" %S\n" intro_blob ;
  List.iter (List.rev !examples) ~f:(fun f -> f o) ;
  fprintf o "%s"
    {ocaml|
let () =
    List.iter (List.rev !examples) ~f:(Example.run Format.std_formatter)
|ocaml} ;
  close_out o ;
  printf "%s: Done.\n%!" Sys.argv.(0)
