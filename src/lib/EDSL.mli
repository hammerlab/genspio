type 'a t = 'a Language.t


val fail: unit t
(** Abort the script/command immediately. *)

val with_signal:
  ?signal_name:string -> catch:unit t -> (unit t -> unit t) -> unit t
(** Use a UNIX signal (default ["USR1"]) to create a “jump.”

    [with_signal ~catch (fun throw -> (* more_code *))]
    executes [(* more_code *)] but if it uses [throw], the code behaves like
    a raised exception, and the [catch] argument is executed.
 *)

val call : string t list -> unit t
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

val getenv: string t -> string t
(** Get the value of an environment variable as a string;
    it returns the empty string when the variable is not defined.
    If the argument is not a valid variable name, behavior is
    undefined.
 *)

val setenv: var:string t -> string t -> unit t
(** Set the value of an environment variable as a string;
    it returns the empty string is the variable is not defined.

    If the [~var] argument is not a valid variable name or if the value does
    not fit in a shell variable (newlines, ['\000']), behavior is undefined.
 *)

val ( &&& ) : bool t -> bool t -> bool t
val ( ||| ) : bool t -> bool t -> bool t
val ( =$= ) : string t -> string t -> bool t
val ( <$> ) : string t -> string t -> bool t

val returns: 'a t -> value: int -> bool t
(** Check the return value of a command/expression/script. *)
    
val succeeds : 'a t -> bool t
(** [succeeds expr] is a equivalent to [returns expr ~value:0]. *)

val nop : unit t
val if_then_else :
  bool t -> unit t -> unit t -> unit t
val if_then : bool t -> unit t -> unit t

val seq : unit t list -> unit t
(** Sequence a list of expressions into an expression. *)

val if_seq:
  t:unit t list ->
  ?e:unit t list ->
  bool t ->
  unit t
(** [if_seq c ~t ~e] is an alternate API for {!if_then_else} (when
    [?e] is provided) or {!if_then} (otherwise) that assumes “then”
    and “else” bodies to be lists for {!seq} construct. *)

val not : bool t -> bool t

val file_exists : string t -> bool t

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


val write_output :
  ?stdout:string t ->
  ?stderr:string t ->
  ?return_value:string t -> unit t -> unit t

val write_stdout : path: string t -> unit t -> unit t

(** {3 Literals } *)

val string : string -> string t
val int : int -> int t
val bool : bool -> bool t

module Integer : sig
  val to_string : int t -> string t
  val of_string : string t -> int t
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

val output_as_string : unit t -> string t
val feed : string:string t -> unit t -> unit t
val ( >> ) : string t -> unit t -> unit t
val loop_while : bool t -> body:unit t -> unit t

type 'argument_type cli_option = 'argument_type Language.cli_option
type 'argument_type option_spec = 'argument_type Language.option_spec
type ('parse_function, 'return_type) cli_options = ('parse_function, 'return_type) Language.cli_options
module Option_list : sig
  val string :
    ?default: string t ->
    doc:string -> char ->
    string t option_spec
  val flag :
    ?default: bool t ->
    doc:string -> char ->
    bool t option_spec
  val ( & ) :
    'argument_type option_spec ->
    ('parse_function, 'return_type) cli_options ->
    ('argument_type -> 'parse_function, 'return_type) cli_options
  val usage : string -> ('last_return_type, 'last_return_type) cli_options
end

val parse_command_line :
  ('parse_function, unit t) cli_options -> 'parse_function -> unit t

val string_concat: string t list -> string t

type file = <
  get : string t;
  set : string t -> unit t;
  append : string t -> unit t;
  delete: unit t;
  path: string t;
>
(** Abstraction of a file, cf. {!tmp_file}. *)


val tmp_file: ?tmp_dir: string t -> string -> file
(** Create a temporary file that may contain arbitrary strings (can be
    used as variable containing [string t] values). *)

val with_failwith:
  ((message:string Language.t -> return:int Language.t -> unit Language.t) ->
   unit Language.t) ->
  unit Language.t
(** [with_failwith f] uses !{tmp_file} and {!with_signal} to call [f]
    with a function that exits the flow of execution and displays
    [~message] and returns [~return] (a bit like {!Pervasives.failwith}). *)

(** The {!Magic} module is like OCaml's {!Obj.magic} function for the
    EDSL; it allows one to bypass typing. *)
module Magic : sig
  val unit: string -> unit t
  (** Put any string as a [unit t] command inline ([Magic.unit s]
      is different from [exec ["sh"; "-c"; s]] there is no escaping or
      protection). *)
end
