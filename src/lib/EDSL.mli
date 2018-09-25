(** The Embedded Domain Specific Lanaguage to create “shell-expressions.” *)

(** The type of a Genspio expression. *)
type 'a t = 'a Language.t

(** Type to encode arbitrary byte-arrays in the EDSL as
    [str t] values, OCaml literal strings or the outputs (as in
    [stdout]) of processes are byte-arrays.

    NUL-terminated “C-like” strings are encoded at a lower level than this EDSL (as
    [c_string t] values). C-strings cannot contain the ['\x00'] character.
    The command line arguments of commands as well as the contents of
    environment variables must be C-strings. Genspio treats them
    properly by failing when a wrong byte-array needs to be converted
    to a C-string. *)
type str = Language.byte_array

(** {3 Literals } *)

val str : string -> str t
(** Create a {!type:c_string} literal. *)

val string : string -> str t
(** [string] is an alias for {!function:str}. *)

val int : int -> int t

val bool : bool -> bool t

(** {3 Comments} *)

val comment : string -> 'a t -> 'a t
(** Add a “comment” string to an expression (will be displayed in
    error messages happening inside the expression). *)

val ( %%% ) : string -> 'a t -> 'a t
(** ["Some comment" %%% expr] is an alias for [comment "Some comment" expr]. *)

(** {3 Basic system Commands} *)

val call : str t list -> unit t
(** Call a command from its list of “arguments” (including the first
    argument being the actual command). *)

val exec : string list -> unit t
(** Like {!call} but with string literals; i.e. [exec ["a"; "b"]] is
    actually [call [string "a"; string "b"]] which is the usual shell command
    ["a b"] (with proper escaping). *)

val getenv : str t -> str t
(** Get the value of an environment variable as a string;
    it returns the empty string when the variable is not defined.
    If the argument is not a valid variable name, behavior is
    undefined.
 *)

val setenv : var:str t -> str t -> unit t
(** Set the value of an environment variable as a string;
    it returns the empty string is the variable is not defined.

    If the [~var] argument is not a valid variable name or if the value does
    not fit in a shell variable (e.g. newlines), behavior is undefined.
    
    Also, the total environment of a UNIX process counts towards the
    total size of the arguments passed on to a sub-process (see
    usually the result of ["getconf ARG_MAX"]). Genspio does not check
    for that limit which is not that high in some operating systems
    (e.g. about 200 KiB on the {i MacOSX Sierra} that the Travis CI
    runs …). You might prefer putting or accumulating things in a
    {!tmp_file}.
 *)

(** {3 Boolean Expressions} *)

val ( &&& ) : bool t -> bool t -> bool t

val ( ||| ) : bool t -> bool t -> bool t

val not : bool t -> bool t

val returns : 'a t -> value:int -> bool t
(** Check the return value of a command/expression/script. *)

val succeeds : 'a t -> bool t
(** [succeeds expr] is equivalent to [returns expr ~value:0]. *)

val file_exists : str t -> bool t
(** Check whether a file exists, i.e. a shortcut for
    [call [str "test"; str "-f"; path] |> succeeds]. *)

(** Conversions of the [bool t] type. *)
module Bool : sig
  val to_string : bool t -> str t

  val of_string : str t -> bool t
end

(** {3 Integer Arithmetic} *)

(** Functions on [int t] values (arithmetic, comparisons, conversions, etc.). *)
module Integer : sig
  val to_str : int t -> str t

  val of_str : str t -> int t

  val bin_op :
    int t -> [`Div | `Minus | `Mult | `Plus | `Mod] -> int t -> int t

  val add : int t -> int t -> int t

  val ( + ) : int t -> int t -> int t

  val sub : int t -> int t -> int t

  val ( - ) : int t -> int t -> int t

  val mul : int t -> int t -> int t

  val ( * ) : int t -> int t -> int t

  val div : int t -> int t -> int t

  val ( / ) : int t -> int t -> int t

  val modulo : int t -> int t -> int t

  val ( mod ) : int t -> int t -> int t

  val cmp : [`Eq | `Ge | `Gt | `Le | `Lt | `Ne] -> int t -> int t -> bool t

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

(** Functions on ['a list t] values. *)
module Elist : sig
  val make : 'a t list -> 'a list t
  (** Make an EDSL list out of an OCaml list. *)

  val append : 'a list t -> 'a list t -> 'a list t
  (** Concatenate two EDSL lists. *)

  val iter : 'a list t -> f:((unit -> 'a t) -> unit t) -> unit t
  (** Iterate over a list, the body of the loop [~f] takes as argument
      function that returns the current eletment at the EDSL level. *)

  val serialize_byte_array_list : str list t -> str t

  val deserialize_to_byte_array_list : str t -> str list t

  val serialize_str_list : str list t -> str t

  val deserialize_to_str_list : str t -> str list t

  val serialize_int_list : int list t -> str t

  val deserialize_to_int_list : str t -> int list t
end

(** {3 String Manipulation} *)

module Str : sig
  val equals : str t -> str t -> bool t

  val ( =$= ) : str t -> str t -> bool t

  val ( <$> ) : str t -> str t -> bool t

  val concat_list : str t list -> str t
  (** Concatenate an (OCaml) list of [str t] values. *)

  val concat_elist : str list t -> str t
  (** Concatenate a Genspio list of strings [str list t]. *)
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

val if_seq : t:unit t list -> ?e:unit t list -> bool t -> unit t
(** [if_seq c ~t ~e] is an alternate API for {!if_then_else} (when
    [?e] is provided) or {!if_then} (otherwise) that takes “then”
    and “else” bodies which are lists for the {!seq} construct. *)

(** {3 Switch Statements } *)

val switch : [`Case of bool t * unit t | `Default of unit t] list -> unit t
(** Create a switch statement from a list of {!case} and optionally a
    {!default} (the function raises an exception if there are more
    than one default cases). *)

val case : bool t -> unit t list -> [> `Case of bool t * unit t]
(** Create a normal case for a {!switch} statement. *)

val default : unit t list -> [> `Default of unit t]
(** Create the default case for a {!switch} statement. *)

(**/**)

val make_switch :
     (bool Language.t * unit Language.t) list
  -> default:unit Language.t
  -> unit Language.t

(**/**)

(** {3 Redirections and File Descriptors } *)

(** Abstract type of file-descriptor redirections. *)
type fd_redirection

val to_fd : int t -> int t -> fd_redirection
(** Create a file-descriptor to  file-descriptor redirection. *)

val to_file : int t -> str t -> fd_redirection
(** Create a file-descriptor to file redirection. *)

val with_redirections : unit t -> fd_redirection list -> unit t
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
  ?stdout:str t -> ?stderr:str t -> ?return_value:str t -> unit t -> unit t
(** Redirect selected streams or the return value to files ([stdout],
    [stderr], [return_value] are paths). *)

val write_stdout : path:str t -> unit t -> unit t
(** [write_stdout ~path expr] is [write_output expr ~stdout:path]. *)

val pipe : unit t list -> unit t
(** Pipe commands together (["stdout"] into ["stdin"] exactly like the
    [" | "] operator). *)

val ( ||> ) : unit t -> unit t -> unit t
(** [a ||> b] is a shortcut for [pipe [a; b]]. *)

val get_stdout : unit t -> str t
(** Get the contents of [stdout] into a byte array (in previous
    versions this function was called [output_as_string]).  *)

val feed : string:str t -> unit t -> unit t
(** Feed some content ([~string]) into the ["stdin"] filedescriptor of
    a [unit t] expression. *)

val ( >> ) : str t -> unit t -> unit t
(** [str >> cmd] is [feed ~string:str cmd]. *)

val printf : str t -> str t list -> unit t
(**  [printf fmt l] is [call (string "printf" :: string "--" :: fmt :: l)]. *)

val eprintf : str t -> str t list -> unit t
(** Like {!printf} but redirected to ["stderr"]. *)

(** {3 Escaping The Execution Flow } *)

val fail : string -> unit t
(** Expression that aborts the whole script/command immediately, it
    will try to output its argument to [stderr] (but this may be
    silent depending on the redirections active at a given time). *)

(** {3 Temporary Files} *)

(** Abstraction of a file, cf. {!tmp_file}. *)
type file =
  < get: str t  (** Get the current contents of the file *)
  ; set: str t -> unit t
  ; append: str t -> unit t
  ; delete: unit t
  ; path: str t >

val tmp_file : ?tmp_dir:str t -> string -> file
(** Create a temporary file that may contain arbitrary strings (can be
    used as variable containing [string t] values).
    
    [tmp_file "foo"] points to a path that is a {b function}
    of the string ["foo"]; it does not try to make temporary-files
    unique, on the contrary: two calls to [tmp_file "foo"] ensure that
    it is the same file.
 *)

(** {3 Command Line Parsing} *)

(** Typed command-line parsing for your shell scripts, à la {!Printf.scanf}. *)
module Command_line : sig
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

  type 'a cli_option = {switches: string list; doc: string; default: 'a}

  type _ option_spec =
    | Opt_flag : bool t cli_option -> bool t option_spec
    | Opt_string : str t cli_option -> str t option_spec

  and (_, _) cli_options =
    | Opt_end : string -> ('a, 'a) cli_options
    | Opt_cons :
        'c option_spec * ('a, 'b) cli_options
        -> ('c -> 'a, 'b) cli_options

  module Arg : sig
    val string :
      ?default:str t -> doc:string -> string list -> str t option_spec

    val flag :
      ?default:bool t -> doc:string -> string list -> bool t option_spec

    val ( & ) :
      'a option_spec -> ('b, 'c) cli_options -> ('a -> 'b, 'c) cli_options

    val usage : string -> ('a, 'a) cli_options
  end

  val parse : ('a, unit t) cli_options -> (anon:str list t -> 'a) -> unit t
end

(** {3 Additional Higher-Level Utilities} *)

val loop_until_true :
     ?attempts:int
  -> ?sleep:int
  -> ?on_failed_attempt:(int t -> unit t)
  -> bool t
  -> bool t
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

val seq_and : 'a t list -> bool t
(** [seq_and [a; b; c]] is like [succeeds a &&& succeeds b &&& succeeds c]. *)

val output_markdown_code : string -> unit t -> unit t
(** [output_markdown_code "ocaml" (exec ["echo"; "let x = 42"])]
    runs its second argument within markdown-like code fences. *)

val cat_markdown : string -> str t -> unit t
(** [cat_markdown tag path] outputs the contents of the file at
    [path] (with ["cat"]) within a markdown code bloc. *)

val check_sequence :
     ?verbosity:[`Announce of string | `Output_all | `Silent]
  -> ?on_failure:(   step:string * unit t
                  -> stdout:str t
                  -> stderr:str t
                  -> unit t)
  -> ?on_success:(   step:string * unit t
                  -> stdout:str t
                  -> stderr:str t
                  -> unit t)
  -> ?tmpdir:string
  -> (string * unit t) list
  -> unit t
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

val on_stdin_lines : (str t -> unit t) -> unit t
(** [on_stdin_lines body] builds a loop that iterates over the lines of the [stdin]
    file descriptor. The argument of `body` is the current line.
    Note that this is for text-like input, ['\000']
    characters in the input lead to undefined behavior. *)

val strs : string list -> str t list
(** [strs] is simply just [List.map ~f:str]. *)

val command_available : str t -> bool t
(** Call ["command -v"] to know if an executable is in ["$PATH"] (note that
    ["which"] is not POSIX, we use ["command -v ..."] which is
    {{:http://pubs.opengroup.org/onlinepubs/009696899/utilities/command.html}expected}
    to return a failure if the command is not found). *)

val get_stdout_one_line :
  ?first_line:bool -> ?remove_spaces:bool -> unit t -> str t
(** Get the output of a command as a string without new lines,
potentially cutting at the first line:
{[
  get_stdout
    ( (if first_line then u ||> exec ["head"; "-n"; "1"] else u)
    ||> exec ["tr"; "-d"; (if remove_spaces then " \\n" else "\\n")] )
]}
*)

val verbose_call : ?prefix:string -> ?verbose:bool t -> str t list -> unit t
(** Like {!call} but print on [stderr] the command being run. *)

val check_sequence_with_output : unit t list -> unit t
(** A shortcut for [check_sequence] with [~verbosity:`Output_all]
    hence ignoring the “names” of the commands. *)

val is_regular_file : str t -> bool t
(** Call ["test -f ..."]. *)

val is_directory : str t -> bool t
(** Call ["test -d ..."]. *)

val is_executable : str t -> bool t
(** Call ["test -x ..."]. *)

val is_readable : str t -> bool t
(** Call ["test -r ..."]. *)

val mkdir_p : str t -> unit t
(** Call ["mkdir -p ..."]. *)

val exit : int -> unit t
(** Call ["exit ..."], warning: depending on the compiler, the call
    maybe nested in various sub-shells, to really exit a script use
    {!fail}. *)

val home_path : unit -> str t
(** The value of ["$HOME"]. *)

val ( ^$^ ) : str t -> str t -> str t
(** Concatenate two EDSL strings. *)

val ( /// ) : str t -> str t -> str t
(** [a /// b] is [a ^$^ str "/" ^$^ b]. *)

val say : string -> str t list -> unit t
(** [say fmt l] is a shortcut to call [eprintf (str fmt) l]. *)

val ensure : string -> condition:bool t -> how:(string * unit t) list -> unit t
(** Ensure a [condition]:

- Test the condition.
- If [true] do nothing, succeed
- If [false], run [~how] with {!check_sequence}.
- Test the condition again, if [true] succeed, if [false] fail.
    
Failures happen thanks to the !{fail} call.
 *)

val greps_to : ?extended_re:bool -> str t -> unit t -> bool t
(** Test a string or regular expression again the output of an expression. *)

val pager :
     ?file_descriptor:str t
  -> ?disable:bool t
  -> ?default_command:unit t
  -> unit
  -> unit t
(** “Smart pager” command to pipe long outputs through, conditions are
    tested in this order:

    - If [~disable] is provided it can be used to make the pager
      behave like ["cat"].
    - If the [~file_descriptor] (default [str "1"]) is not a terminal,
      the command will be ["cat"] too.
    - If the environment variable ["PAGER"] it will be used as a shell
      command (["sh -c .."]).
    - The last resort default is [~default_command] (which by default
      value is [exec ["more"]]).
 *)

(** Make scripts that provide a ["--describe"] option/command.

The two functions provided can be used like this:

{[
let open Command_line in
let opts =
  let open Arg in
  flag ["--all"; "-a"] ~doc:"Kill everything, incl. the Screen session"
  & describe_option_and_usage ()
in
let kills = tmp_file "kill-list" in
parse opts (fun ~anon kill_em_all describe ->
    deal_with_describe describe
      [ if_seq kill_em_all
]}

See also the
{{:https://smondet.gitlab.io/genspio-doc/master/service-composer-example.html}Service
Composer Example} which is where the above snippet comes from.

 *)
module Script_with_describe (P : sig
  val name : string

  val description : string
end) : sig
  val name : string

  val description : string

  val describe_option_and_usage :
       ?more_usage:string list
    -> unit
    -> (bool t -> 'a, 'a) Command_line.cli_options
  (** Provide the ["--describe"] command line option. *)

  val deal_with_describe : bool t -> unit t list -> unit t
  (** Deal with the parsed result of the ["--describe"] option. *)
end

(** Create a script that mostly behaves like ["git"], it concatenates
    its name with its first argument to call ["${0}-${1}"]. *)
module Dispatcher_script : sig
  val make :
       ?aliases:(str t * str t) list
    -> name:string
    -> description:string
    -> unit
    -> unit Genspio__Language.t
  (** 
     Make a “toplevel” script that behaves a bit like ["git"] by
     calling [name ^ "-${1}"]. The search for the argument can be
     “hijacked” with the list of [~aliases]; with [~name:"hello"] and
     [~aliases:[str "W", str "wolrd"]], when ["hello W"] is called,
     the generated script with lool for ["hello-world"] in the
     ["$PATH"].
     
     Just like scripts made with {!Script_with_describe}, the
     [~description] argument is used to answer the ["--describe"]
     command line option.
     
     When called without arguments, with ["-h"], ["-help"], or with
     ["--help"], the script lists all the commands it can find and the
     aliases. E.g.:

{v
usage: cosc <cmd> [OPTIONS/ARGS]

Script that is a bit like Docker-compose but with GNU-Screen.

Sub-commands:
* cosc-attach: Attach to the Screen being managed.
* cosc-configuration: Manage the configuration.
* cosc-example: Show or run a full example.
* cosc-kill: Kill Jobs or the whole Screen session (-a).
* cosc-logs: Show logs for one or more jobs.
* cosc-manual: Show the manual.
* cosc-start: Start all or a given list of jobs.
* cosc-status: Get the status(es) of the processes.
* cosc-version: Show the version information.
Aliases:
* config -> configuration
* man -> manual
v}

     See also the
     {{:https://smondet.gitlab.io/genspio-doc/master/service-composer-example.html}Service
     Composer Example} (a.k.a. ["cosc"]) for many uses of this
     function.

*)
end

(** {3 Very Unsafe Operations} *)

(** The {!Magic} module is like OCaml's {!Obj.magic} function for the
    EDSL; it allows one to bypass typing. *)
module Magic : sig
  val unit : string -> unit t
  (** Put any string as a [unit t] command inline ([Magic.unit s]
      is different from [exec ["sh"; "-c"; s]] there is no escaping or
      protection). *)
end
