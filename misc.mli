val has_prefix : string -> string -> bool
val has_suffix : string -> string -> bool
exception Match
val get_suffix : string -> string -> string
val split_string : string -> char -> int -> string list

val parse_shell_command : string -> string array
    
val all_options : unit -> (string * Arg.spec * string) list
val set_option : string -> Arg.spec -> string -> unit

val pretty_options : ((string * Arg.spec * string) list as 'a) -> 'a

(* To pretty print options, use '\t' in the info string, the part
   of the info message starting after tabulation will be aligned. 
   Typically, the option first part of the message is the name of
   the argument to the option. If no '\t' is present, the message
   will not be aligned *)

    
val debug_option : string -> string -> (string -> bool)
(* 
   [debug_option option message] create an option flag that is false by
   default and that can be set with [option] with info [message]; 
   then it returns a function to that prints its argument on stderr, 
   but only when the [option] is set. 
*) 
val debug : string -> bool
(* [debug mes] prints mes to [stderr] if debug mode in turn on *)
    
    
val option_flag : bool -> string -> string -> bool ref
(*
   [option_flag init opt mes] creates a boolean flag with [init] 
   as initial value declares the optional argument [opt] with info
    message [mes] that sets or unsets the flag, according to the value 
    of [init]  
*)
val dops : bool ref;;
(* [nogs] when set means do not call gs for drawing inline Postscript. *)
    
val handle_fatal_error : (unit -> unit) -> unit -> unit
val fatal_error : string -> 'a

val warning : string -> unit    
    
    
    
    
