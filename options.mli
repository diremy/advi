val all : unit -> (string * Arg.spec * string) list
val set : string -> Arg.spec -> string -> unit
(* 
   [set_option opt spec man] add the option [opt] to the command line
   with specification [spec] and man info [man]   
*)
    
val pretty : ((string * Arg.spec * string) list as 'a) -> 'a
(* 
   To pretty print options, use '\t' in the info string, the part
   of the info message starting after tabulation will be aligned. 
   Typically, the option first part of the message is the name of
   the argument to the option. If no '\t' is present, the message
   will not be aligned.
*)
    
val debug : string -> string -> (string -> bool)
(*
   [make_debug option message] create an option flag that is false by
   default and that can be set with [option] with info [message]; 
   then it returns a function to that prints its argument on stderr, 
   but only when the [option] is set. 
*) 

val flag : bool -> string -> string -> bool ref
(*
   [option_flag init opt mes] creates a boolean flag with [init] 
   as initial value declares the optional argument [opt] with info
   message [mes] that sets or unsets the flag, according to the value 
   of [init]  
*)
val pson : bool ref;;
(* [nogs] when set means do not call gs for drawing inline Postscript. *)
val dops : bool ref;;
(* temporary value, reset to pson when reloading the file. *)

(** tells whether display must always happen in foreground *)
val global_display_mode : bool ref
