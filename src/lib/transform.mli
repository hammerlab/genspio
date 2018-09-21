(** Manipulate the AST (['a EDST.t] values). *)

(** A generic AST visitor pattern, which by default does nothing. *)
module Visitor : sig
  class nothing_doer :
    ?trace:Format.formatter
    -> unit
    -> object
         method bool_operator :
           bool Language.t * [`And | `Or] * bool Language.t -> bool Language.t

         method bool_to_string :
           bool Language.t -> Language.c_string Language.t

         method byte_array_concat :
              Language.byte_array list Language.t
           -> Language.byte_array Language.t

         method byte_array_to_c_string :
           Language.byte_array Language.t -> Language.c_string Language.t

         method c_string_concat :
           Language.c_string list Language.t -> Language.c_string Language.t

         method c_string_to_byte_array :
           Language.c_string Language.t -> Language.byte_array Language.t

         method comment : string * 'a Language.t -> 'a Language.t

         method exec : Language.c_string Language.t list -> unit Language.t

         method expression : 'a Language.t -> 'a Language.t

         method fail : string -> unit Language.t

         method feed :
           Language.byte_array Language.t * unit Language.t -> unit Language.t

         method getenv :
           Language.c_string Language.t -> Language.c_string Language.t

         method if_ :
              bool Language.t * unit Language.t * unit Language.t
           -> unit Language.t

         method int_bin_comparison :
              int Language.t
              * [`Eq | `Ge | `Gt | `Le | `Lt | `Ne]
              * int Language.t
           -> bool Language.t

         method int_bin_op :
              int Language.t
              * [`Div | `Minus | `Mod | `Mult | `Plus]
              * int Language.t
           -> int Language.t

         method int_to_string : int Language.t -> Language.c_string Language.t

         method list : 'a Language.t list -> 'a list Language.t

         method list_append :
           'a list Language.t * 'a list Language.t -> 'a list Language.t

         method list_iter :
              'a list Language.t * ((unit -> 'a Language.t) -> unit Language.t)
           -> unit Language.t

         method list_to_string :
              'a list Language.t
              * ('a Language.t -> Language.byte_array Language.t)
           -> Language.byte_array Language.t

         method literal : 'a Language.Literal.t -> 'a Language.t

         method no_op : unit Language.t

         method not : bool Language.t -> bool Language.t

         method output_as_string :
           unit Language.t -> Language.byte_array Language.t

         method pipe : unit Language.t list -> unit Language.t

         method raw_cmd :
           Language.raw_command_annotation option * string -> 'a Language.t

         method redirect_output :
           unit Language.t * Language.fd_redirection list -> unit Language.t

         method returns : expr:'a Language.t -> value:int -> bool Language.t

         method seq : unit Language.t list -> unit Language.t

         method setenv :
              Language.c_string Language.t * Language.c_string Language.t
           -> unit Language.t

         method string_operator :
              Language.byte_array Language.t
              * [`Eq | `Neq]
              * Language.byte_array Language.t
           -> bool Language.t

         method string_to_bool :
           Language.c_string Language.t -> bool Language.t

         method string_to_int : Language.c_string Language.t -> int Language.t

         method string_to_list :
              Language.byte_array Language.t
              * (Language.byte_array Language.t -> 'a Language.t)
           -> 'a list Language.t

         method while_ :
           condition:bool Language.t -> body:unit Language.t -> unit Language.t

         method write_output :
              expr:unit Language.t
           -> stdout:Language.c_string Language.t option
           -> stderr:Language.c_string Language.t option
           -> return_value:Language.c_string Language.t option
           -> unit Language.t
       end
end

(** A basic implementation of constant propagation. *)
module Constant_propagation : sig
  val process : ?trace:Format.formatter -> 'a Language.t -> 'a Language.t
  (** Simplify an ['a EDSL.t] expression by propagating some of the
      constant values. *)

  val test : unit -> unit
  (** Some tests specific to the module, see option
      ["--run-constant-propagation-tests"] of the main tests. *)
end
