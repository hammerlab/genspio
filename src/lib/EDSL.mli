type 'a t = 'a Language.t


val fail: unit t
(** Abort the script/command immediately. *)

val call : string t list -> unit t
(** Call a command from its list of “arguments” (including the first
    argument being the actual command). *)

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
val seq : unit t list -> unit t
val not : bool t -> bool t
val printf : ('a, unit, string, unit t) format4 -> 'a
val file_exists : string -> bool t
val switch :
  (bool t * unit t) list ->
  default:unit t -> unit t
val write_output :
  ?stdout:string ->
  ?stderr:string ->
  ?return_value:string -> unit t -> unit t
val write_stdout : path:string -> unit t -> unit t

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
