(** The Embedded Domain Specific Lanaguage to create “shell-expressions.” *)


type 'a t = 'a Language.t
(** The type of a Genspio expression. *)

type byte_array
(** Abstract type to encode arbitrary byte-arrays in the EDSL as
    [byte_array t] values, OCaml literal strings or the outputs (as in
    [stdout]) of processes are byte-arrays. *)

type c_string
(** Abstract type to encode NUL-terminated strings in the EDSL as
    [c_string t] values. C-strings cannot contain the ['\x00'] character.
    The command line arguments of commands as well as the contents of
    environment variables must be C-strings. *)

(** {3 Literals } *)

val string : string -> c_string t
val c_string : string -> c_string t
val byte_array : string -> byte_array t
val int : int -> int t
val bool : bool -> bool t

(** {3 Comments} *)

val comment: string -> 'a t -> 'a t
(** Add a “comment” string to an expression (will be displayed in
    error messages happening inside the expression). *)

val (%%%): string -> 'a t -> 'a t
(** ["Some comment" %%% expr] is an alias for [comment "Some comment" expr]. *)

(** {3 Basic system Commands} *)

val call : c_string t list -> unit t
(** Call a command from its list of “arguments” (including the first
    argument being the actual command).

    Note that UNIX does not allow strings passed as arguments to
    executables to contain NUL-characters (['\x00']).
    The function {!Language.to_many_lines} raises an exception
    if an argument is a literal and contains a NUL, but if the
    argument is the result of some other expression the behavior is
    for now undefined.
*)

val exec : string list -> unit t
(** Like {!call} but with string literals; i.e. [exec ["a"; "b"]] is
    actually [call [string "a"; string "b"]] which is the usual shell command
    ["a b"] (with proper escaping). *)

val getenv: c_string t -> c_string t
(** Get the value of an environment variable as a string;
    it returns the empty string when the variable is not defined.
    If the argument is not a valid variable name, behavior is
    undefined.
 *)

val setenv: var:c_string t -> c_string t -> unit t
(** Set the value of an environment variable as a string;
    it returns the empty string is the variable is not defined.

    If the [~var] argument is not a valid variable name or if the value does
    not fit in a shell variable (newlines, ['\000']), behavior is undefined.
 *)

(** {3 Boolean Expressions} *)

val ( &&& ) : bool t -> bool t -> bool t
val ( ||| ) : bool t -> bool t -> bool t
val not : bool t -> bool t

val returns: 'a t -> value: int -> bool t
(** Check the return value of a command/expression/script. *)

val succeeds : 'a t -> bool t
(** [succeeds expr] is a equivalent to [returns expr ~value:0]. *)

val file_exists : c_string t -> bool t
(** Check whether a file exists, i.e. a shortcut for
    [call [c_string "test"; c_string "-f"; path] |> succeeds]. *)

(** Conversions of the [bool t] type. *)
module Bool: sig
  val to_string : bool t -> c_string t
  val of_string : c_string t -> bool t
end


(** {3 Integer Arithmetic} *)

module Integer : sig
  val to_string : int t -> c_string t
  val to_byte_array : int t -> byte_array t
  val of_string : c_string t -> int t
  val of_byte_array : byte_array t -> int t
  val bin_op : int t -> [ `Div | `Minus | `Mult | `Plus | `Mod ] -> int t -> int t
  val add : int t -> int t -> int t
  val ( + ) : int t -> int t -> int t
  val sub : int t -> int t -> int t
  val ( - ) : int t -> int t -> int t
  val mul : int t -> int t -> int t
  val ( * ) : int t -> int t -> int t
  val div : int t -> int t -> int t
  val ( / ) : int t -> int t -> int t
  val modulo : int t -> int t -> int t
  val (mod) : int t -> int t -> int t
  val cmp : [ `Eq | `Ge | `Gt | `Le | `Lt | `Ne ] -> int t -> int t -> bool t
  val eq : int t -> int t -> bool t
  val ne : int t -> int t -> bool t
  val lt : int t -> int t -> bool t
  val le : int t -> int t -> bool t
  val ge : int t -> int t -> bool t
  val gt : int t -> int t -> bool t
  val ( = ) : int t -> int t -> bool t
  val ( <> ) : int t -> int t -> bool t
  val ( < ) : int t -> int t -> bool t
  val ( <= ) : int t -> int t -> bool t
  val ( >= ) : int t -> int t -> bool t
  val ( > ) : int t -> int t -> bool t
end

(** {3 EDSL Lists} *)

module Elist : sig

  val make: 'a t list -> 'a list t
  (** Make an EDSL list out of an OCaml list. *)

  val append: 'a list t -> 'a list t -> 'a list t

  val iter: 'a list t -> f:((unit -> 'a t) -> unit t) -> unit t
  (** Iterate over a list, the body of the loop [~f] takes as argument
      function that returns the current eletment at the EDSL level. *)

  val to_string: 'a list t -> f:('a t -> byte_array t) -> byte_array t

  val of_string: byte_array t -> f:(byte_array t -> 'a t) -> 'a list t

end

(** {3 String Manipulation} *)


module Byte_array : sig
  val ( =$= ) : byte_array t -> byte_array t -> bool t
  val ( <$> ) : byte_array t -> byte_array t -> bool t
  val to_c_string: byte_array t -> c_string t
  val to_c: byte_array t -> c_string t
end


module C_string : sig
  val equals : c_string t -> c_string t -> bool t
  val ( =$= ) : c_string t -> c_string t -> bool t
  val ( <$> ) : c_string t -> c_string t -> bool t

  val to_byte_array: c_string t -> byte_array t
  val to_bytes: c_string t -> byte_array t

  val concat_list: c_string t list -> c_string t
  (** Concatenate an (OCaml) list of [c_string t] values. *)

  val concat_elist: c_string list t -> c_string t
  (** Concatenate a Genspio list of strings [c_string list t]. *)
end

(** {3 Control Flow} *)

val nop : unit t
(** The silent “no-operation.” *)

val if_then_else : bool t -> unit t -> unit t -> unit t

val if_then : bool t -> unit t -> unit t

val seq : unit t list -> unit t
(** Sequence a list of expressions into an expression. *)

val loop_while : bool t -> body:unit t -> unit t
(** Build a while loop. *)

val loop_seq_while : bool t -> unit t list -> unit t
(** [loop_seq_while condition body] is a shortcut for
    [loop_while condition ~body:(seq body)]. *)

val if_seq:
  t:unit t list ->
  ?e:unit t list ->
  bool t ->
  unit t
(** [if_seq c ~t ~e] is an alternate API for {!if_then_else} (when
    [?e] is provided) or {!if_then} (otherwise) that takes “then”
    and “else” bodies which are lists for the {!seq} construct. *)

(** {3 Switch Statements } *)

val switch :
  [ `Case of bool t * unit t | `Default of unit t ] list -> unit t
(** Create a switch statement from a list of {!case} and optionally a
    {!default} (the function raises an exception if there are more
    than one default cases). *)

val case :
  bool t ->
  unit t list ->
  [> `Case of bool t * unit t ]
(** Create a normal case for a {!switch} statement. *)

val default : unit t list -> [> `Default of unit t ]
(** Create the default case for a {!switch} statement. *)


(**/**)
val make_switch :
  (bool Language.t * unit Language.t) list ->
  default:unit Language.t -> unit Language.t
(**/**)

(** {3 Redirections and File Descriptors } *)

type fd_redirection
(** Abstract type of file-descriptor redirections. *)

val to_fd: int t -> int t -> fd_redirection
(** Create a file-descriptor to  file-descriptor redirection. *)

val to_file: int t -> c_string t -> fd_redirection
(** Create a file-descriptor to file redirection. *)

val with_redirections:
  unit t -> fd_redirection list -> unit t
(** 
   Run a [unit t] expression after applying a list of file-descriptor
   redirections.

   The redirections are applied in the list's order (which means they
   can be more easily {i followed} in reverse order), see the
   “Arbitrary Redirections” example.

   Invalid cases, like redirecting to a file-descriptor has not been
   opened, lead to undefined behavior; see
   {{:https://github.com/hammerlab/genspio/issues/41}issue #41}.
   If the shell is POSIX, the whole expression [with_redirections expr redirs]
   exits and its return value is in [[1, 125]]; if the shell is
   ["bash"] or ["zsh"], the failing redirection is just ignored and [expr] is
   executed with the remaining redirections if any.
*)

val write_output :
  ?stdout:c_string t ->
  ?stderr:c_string t ->
  ?return_value:c_string t -> unit t -> unit t
(** Redirect selected streams or the return value to files ([stdout],
    [stderr], [return_value] are paths). *)

val write_stdout : path: c_string t -> unit t -> unit t
(** [write_stdout ~path expr] is [write_output expr ~stdout:path]. *)

val pipe: unit t list -> unit t
(** Pipe commands together (["stdout"] into ["stdin"] exactly like the
    [" | "] operator). *)

val (||>) : unit t -> unit t -> unit t
(** [a ||> b] is a shortcut for [pipe [a; b]]. *)

val get_stdout : unit t -> byte_array t
(** Get the contents of [stdout] into a byte array (in previous
    versions this function was called [output_as_string]).  *)

val feed : string:byte_array t -> unit t -> unit t
(** Feed some content ([~string]) into the ["stdin"] filedescriptor of
    a [unit t] expression. *)

val ( >> ) : byte_array t -> unit t -> unit t
(** [str >> cmd] is [feed ~string:str cmd]. *)

val printf : c_string t -> c_string t list -> unit t
(**  [printf fmt l] is [call (string "printf" :: string "--" :: fmt :: l)]. *)

val eprintf : c_string t -> c_string t list -> unit t
(** Like {!printf} but redirected to ["stderr"]. *)

(** {3 Escaping The Execution Flow } *)

val fail: string -> unit t
(** Expression that aborts the whole script/command immediately, it
    will try to output its argument to [stderr] (but this may be
    silent depending on the redirections active at a given time). *)

(** {3 Temporary Files} *)

type file = <
  get : byte_array t; (** Get the current contents of the file *)
  get_c : c_string t;
  set : byte_array t -> unit t;
  set_c : c_string t -> unit t;
  append : byte_array t -> unit t;
  delete: unit t;
  path: c_string t;
>
(** Abstraction of a file, cf. {!tmp_file}. *)


val tmp_file: ?tmp_dir: c_string t -> string -> file
(** Create a temporary file that may contain arbitrary strings (can be
    used as variable containing [string t] values).
    
    [tmp_file "foo"] points to a path that is a {b function}
    of the string ["foo"]; it does not try to make temporary-files
    unique, on the contrary: two calls to [tmp_file "foo"] ensure that
    it is the same file.
 *)


(** {3 Command Line Parsing} *)

(** Typed command-line parsing for your shell scripts, à la {!Prtinf.scanf}. *)
module Command_line: sig
  (** 

     Use this module like OCaml's {!Printf.scanf} function.

     - Build a command-line “format specification” using the {!Arg} module.
     - Call the {!parse} function with an appropriately typed function.

     Example:
     Here is a potential argument specification for a shell script
     that downloads and unarchives them (see also ["src/test/examples.ml"]).
     {[
       let cli_spec =
         Command_line.Arg.(
           string
             ~doc:"The URL to the stuff" ["-u"; "--url"]
             ~default:no_value
           & flag ["-d"; "--remove-intermediary-files"]
               ~doc:"Remove intermediary files."
           & string ["-f"; "--local-filename"]
             ~doc:"Override the downloaded file-name"
             ~default:no_value
           & string ["-t"; "--tmp-dir"]
             ~doc:"Use <dir> as temp-dir"
             ~default:(Genspio.EDSL.string "/tmp/genspio-downloader-tmpdir")
           & usage "Download archives and decrypt/unarchive them.\n\
                    ./downloader -u URL [-c] [-f <file>] [-t <tmpdir>]"
         ) in
       (*
          `cli_spec` has type:

           (string Genspio.EDSL.t ->
            bool Genspio.EDSL.t ->
            string Genspio.EDSL.t -> string Genspio.EDSL.t -> unit Genspio.EDSL.t,
            unit Genspio.EDSL.t)
           Genspio.EDSL.Command_line.cli_options
          
           so the action function (the second argument to parse) must have type:

           anon:string list Genspio.EDSL.t ->
           string Genspio.EDSL.t ->
           bool Genspio.EDSL.t ->
           string Genspio.EDSL.t ->
           string Genspio.EDSL.t ->
           unit Genspio.EDSL.t
       *)
       Command_line.parse cli_spec
         (fun ~anon url all_in_tmp filename_ov tmp_dir ->
            (*
               ...
               your code
               ...
            *)
     ]}
  *)

  type 'a cli_option = {
    switches : string list;
    doc : string;
    default : 'a;
  }

  type _ option_spec =
      Opt_flag : bool t cli_option -> bool t option_spec
    | Opt_string : c_string t cli_option -> c_string t option_spec
  and (_, _) cli_options =
      Opt_end : string -> ('a, 'a) cli_options
    | Opt_cons : 'c option_spec *
        ('a, 'b) cli_options -> ('c -> 'a, 'b) cli_options

  module Arg :
    sig
      val string :
        ?default:c_string t -> doc:string -> string list -> c_string t option_spec
      val flag :
        ?default:bool t -> doc:string -> string list -> bool t option_spec
      val ( & ) :
        'a option_spec -> ('b, 'c) cli_options -> ('a -> 'b, 'c) cli_options
      val usage : string -> ('a, 'a) cli_options
    end

  val parse : ('a, unit t) cli_options -> (anon: c_string list t -> 'a) -> unit t
end

(** {3 Additional higher-level utilities} *)
module Extra_constructs: sig

  val loop_until_true :
    ?attempts:int ->
    ?sleep:int ->
    ?on_failed_attempt:(int t -> unit t) ->
    bool t -> bool t
  (** [loop_until_true eval_condition] tries to run [eval_condition]
      in a loop until it succeeds. It makes [~attempts] attemps
      (default 20), and sleeps for [sleep] seconds (default 2) after
      each failed attempt. The argument [~on_failed_attempt] can be
      used for instance to display something between each failed
      attempt and the call to [sleep], the default is {[
        fun nth -> printf (string "%d.") [Integer.to_string nth]
     ]}.
 *)

  val silently : unit t -> unit t
  (** [silently expr] is [expr] with [stdout] and [stderr] redirected to ["/dev/null"]. *)

  val succeeds_silently : unit t -> bool t
  (**  [succeeds_silently u] {i is} [silently u |> succeeds]. *)

  val seq_and: 'a t list -> bool t
  (** [seq_and [a; b; c]] is like [succeeds a &&& succeeds b &&& succeeds c]. *)

  val output_markdown_code: string -> unit t -> unit t
  (** [output_markdown_code "ocaml" (exec ["echo"; "let x = 42"])]
      runs its second argument within markdown-like code fences. *)

  val cat_markdown: string -> c_string t -> unit t
  (** [cat_markdown tag path] outputs the contents of the file at
      [path] (with ["cat"]) within a markdown code bloc. *)

  val check_sequence:
    ?verbosity:[ `Announce of string | `Output_all | `Silent ] ->
    ?on_failure:(step: string * unit t ->
                 stdout: c_string t ->
                 stderr: c_string t ->
                 unit t) ->
    ?on_success:(step: string * unit t ->
                 stdout: c_string t ->
                 stderr: c_string t ->
                 unit t) ->
    ?tmpdir:string ->
    (string * unit t) list ->
    unit t
  (** Run a sequence of expressions until the first that fails:

      {ul
        {li [?verbosity] configures the output behavior, {ul
             {li [`Announce prompt] uses [prompt] to output the name-tag
                 of the command, the output of the command is redirected
                 to temporary files (accessible through the [~on_success] and
                 [~on_failure] functions).
                 The default value is [`Announce ">> "].}
             {li [`Output_all] lets all the output of the commands go through.}
             {li [`Silent] is like [`Announce _] but without even the
                 “prompt” command annoucement.}
          }
        }
        {li [?on_failure] configures what to do when encountering the
            first failure, the default is to display on stdout the
            name-tag of the failing command and outputting the
            contents of its [stdout] and [stderr] log-files (if any)
            {b and then} call [exec ["false"]].}
        {li [?on_success] is a similar function as [?on_failure],
            called before starting the next command, the default is to
            do nothing.}
        {li [?tmpdir] configures where to create the logging files.}
      }
      

 *)


end


(** {3 Very Unsafe Operations} *)

(** The {!Magic} module is like OCaml's {!Obj.magic} function for the
    EDSL; it allows one to bypass typing. *)
module Magic : sig
  val unit: string -> unit t
  (** Put any string as a [unit t] command inline ([Magic.unit s]
      is different from [exec ["sh"; "-c"; s]] there is no escaping or
      protection). *)
end
