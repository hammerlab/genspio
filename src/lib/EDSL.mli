(** The Embedded Domain Specific Lanaguage to create “shell-expressions.” *)


type 'a t = 'a Language.t
(** The type of a typed expression. *)

(** {3 Literals } *)

val string : string -> string t
val int : int -> int t
val bool : bool -> bool t

(** {3 Basic system Commands} *)

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

(** {3 Boolean Expressions} *)

val ( &&& ) : bool t -> bool t -> bool t
val ( ||| ) : bool t -> bool t -> bool t
val ( =$= ) : string t -> string t -> bool t
val ( <$> ) : string t -> string t -> bool t

val returns: 'a t -> value: int -> bool t

module Bool: sig
  val to_string : bool t -> string t
  val of_string : string t -> bool t
end

(** Check the return value of a command/expression/script. *)
    
val succeeds : 'a t -> bool t
(** [succeeds expr] is a equivalent to [returns expr ~value:0]. *)

val not : bool t -> bool t

val file_exists : string t -> bool t

(** {3 Integer Arithmetic} *)

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

(** {3 Lists} *)

val list: 'a t list -> 'a list t

val list_append: 'a list t -> 'a list t -> 'a list t

val list_iter: 'a list t -> f:((unit -> 'a t) -> unit t) -> unit t

val list_to_string: 'a list t -> f:('a t -> string t) -> string t
val list_of_string: string t -> f:(string t -> 'a t) -> 'a list t

(** {3 String Manipulation} *)

val output_as_string : unit t -> string t
val feed : string:string t -> unit t -> unit t
val ( >> ) : string t -> unit t -> unit t

val string_concat: string t list -> string t

val string_concat_list: string list t -> string t

(** {3 Control Flow} *)

val nop : unit t
val if_then_else :
  bool t -> unit t -> unit t -> unit t
val if_then : bool t -> unit t -> unit t

val seq : unit t list -> unit t
(** Sequence a list of expressions into an expression. *)

val loop_while : bool t -> body:unit t -> unit t


val if_seq:
  t:unit t list ->
  ?e:unit t list ->
  bool t ->
  unit t
(** [if_seq c ~t ~e] is an alternate API for {!if_then_else} (when
    [?e] is provided) or {!if_then} (otherwise) that assumes “then”
    and “else” bodies to be lists for {!seq} construct. *)

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

(** {3 Redirections } *)

type fd_redirection
(** Abstract type of file-descriptor redirections. *)

val to_fd: int t -> int t -> fd_redirection
(** Create a file-descriptor to  file-descriptor redirection. *)

val to_file: int t -> string t -> fd_redirection
(** Create a file-descriptor to file redirection. *)

val with_redirections:
  unit t -> fd_redirection list -> unit t
(** 
   Run a [unit t] expression after applying a list of file-descriptor
   redirections.

   The redirections are applied in the list's order.
   
   Cf. the example:
   {[
       with_redirections (exec ["printf"; "%s"; "hello"]) [
         to_file (int 3) (string "/path/to/one");
         to_file (int 3) (string "/path/to/two");
         to_fd (int 2) (int 3);
         to_fd (int 1) (int 2);
       ];
   ]}
   
   ["printf '%s' 'hello'"] will output to the file ["/path/to/two"],
   because redirections are set in that order:

   - file-descriptor [3] is set to output to ["/path/to/one"],
   - file-descriptor [3] is set to output to ["/path/to/two"]
     (overriding the previous redirection),
   - file-descriptor [2] is redirected to file-descriptor [3],
   - file-descriptor [1] is redirected to file-descriptor [2],
   - then, ["printf"] outputs to [1].

   Invalid cases, like redirecting to a file-descriptor has not been
   opened, lead to undefined behavior; see
   {{:https://github.com/hammerlab/genspio/issues/41}issue #41}.
   If the shell is POSIX, the whole expression [with_redirections expr redirs]
   exits and its return value is in [[1, 125]]; if the shell is
   ["bash"] or ["zsh"], the failing redirection is just ignored and [expr] is
   executed with the remaining redirections if any.
*)

val write_output :
  ?stdout:string t ->
  ?stderr:string t ->
  ?return_value:string t -> unit t -> unit t

val write_stdout : path: string t -> unit t -> unit t

val eprintf : string t -> string t list -> unit t

(** {3 Escaping The Execution Flow } *)

val fail: unit t
(** Expression that aborts the whole script/command immediately. *)

val with_signal:
  ?signal_name:string -> catch:unit t -> (unit t -> unit t) -> unit t
(** Use a UNIX signal (default ["USR2"]) to create a “jump.”

    [with_signal ~catch (fun signal -> (* more_code *))]
    executes [(* more_code *)] but if it uses [signal], the code behaves like
    a raised exception, and the [catch] argument is executed.

    See the example:

    {[
        let tmp = tmp_file "appender" in
        seq [
          tmp#set (string "start");
          with_signal ~signal_name:"USR1" (fun signal ->
               seq [
                tmp#append (string "-signal");
                signal;
                tmp#append (string "-WRONG");
              ])
            ~catch:(seq [
                tmp#append (string "-caught")
              ]);
          call [string "printf"; string "tmp: %s\\n"; tmp#get];
          assert_or_fail "Timeline-of-tmp"
            (tmp#get =$= string "start-signal-caught");
        ]
    ]}
    
    Note that by default, the compiler functions use the signal ["USR1"] and
    having 2 calls to [trap] with the same signal in the same script
    do not play well, so use [~signal_name:"USR1"] at your own risk.
*)

val with_failwith:
  ((message:string Language.t -> return:int Language.t -> unit Language.t) ->
   unit Language.t) ->
  unit Language.t
(** [with_failwith f] uses !{tmp_file} and {!with_signal} to call [f]
    with a function that exits the flow of execution and displays
    [~message] and returns [~return] (a bit like {!Pervasives.failwith}). *)

(** {3 Temporary Files} *)

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


(** {3 Command Line Parsing} *)

(** Typed command-line parsing for your shell scripts, à la {!Prtinf.scanf}. *)
module Command_line: sig
  (** 

     Use this module like OCaml's {!Printf.scanf} function.

     - Build a command-line “format specification” using the {!Arg} module.
     - Call the {!parse} function with an appropriately typed function.

     Example: {[
       let cli_spec =
         Command_line.Arg.(
           string
             ~doc:"The URL to the stuff" ["-u"; "--url"]
             ~default:no_value
           & flag ["-c"; "--all-in-tmp"] ~doc:"Do everything in the temp-dir"
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
          
           so the second argument must have type:

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
    | Opt_string : string t cli_option -> string t option_spec
  and (_, _) cli_options =
      Opt_end : string -> ('a, 'a) cli_options
    | Opt_cons : 'c option_spec *
        ('a, 'b) cli_options -> ('c -> 'a, 'b) cli_options

  module Arg :
    sig
      val string :
        ?default:string t -> doc:string -> string list -> string t option_spec
      val flag :
        ?default:bool t -> doc:string -> string list -> bool t option_spec
      val ( & ) :
        'a option_spec -> ('b, 'c) cli_options -> ('a -> 'b, 'c) cli_options
      val usage : string -> ('a, 'a) cli_options
    end

  val parse : ('a, unit t) cli_options -> (anon: string list t -> 'a) -> unit t
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
