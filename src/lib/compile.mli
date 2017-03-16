(** Compilers from {!EDSL.t} to POSIX shell scripts. *)

val to_one_liner : ?no_trap:bool -> 'a Language.t -> string
(** Compile a Genspio expression to a single-line POSIX shell command.

    The shell command starts by using ["trap"] to allow the script to
    abort thorugh the {!EDSL.fail} construct; one can avoid this setup
    with [~no_trap:true]

    If [~no_trap:true] is used and the script used the {!EDSL.fail}
    construct, [to_one_liner] fails with an exception.
*)

val to_many_lines : ?no_trap:bool -> 'a Language.t -> string
(** Compile a Genspio expression to a multi-line POSIX shell script,
    slightly more readable than {!to_one_liner}.
*)
