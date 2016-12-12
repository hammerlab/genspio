type 'a t = 'a Language.t


val fail: unit t
(** Abort the script/command immediately. *)

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

val if_seq:
  t:unit t list ->
  ?e:unit t list ->
  bool t ->
  unit t
(** [condition c ~t ~e] is an alternate API for {!if_then_else} (when
    [?e] is provided) or {!if_then} (otherwise) that assumes “then”
    and “else” bodies to be lists for {!seq} construct. *)

val seq : unit t list -> unit t
val not : bool t -> bool t
val printf : ('a, unit, string, unit t) format4 -> 'a

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

val output_as_string : unit t -> string t
val feed : string:string t -> unit t -> unit t
val ( >> ) : string t -> unit t -> unit t
val loop_while : bool t -> body:unit t -> unit t

type 'a cli_option = 'a Language.cli_option
type 'a option_spec = 'a Language.option_spec
type ('a, 'b) cli_options = ('a, 'b) Language.cli_options
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
    'a option_spec ->
    ('b, 'c) cli_options -> ('a -> 'b, 'c) cli_options
  val usage : string -> ('a, 'a) cli_options
end

val parse_command_line :
  ('a, unit t) cli_options -> 'a -> unit t

val string_concat: string t list -> string t

type string_variable = <
    get : string Language.t;
    set : string Language.t -> unit Language.t;
  >
val tmp_file:
  ?tmp_dir: string t ->
  string ->
  string_variable
(** Create a temporary file that may contain arbitrary strings (can be
    used as variable containing [string t] values). *)
