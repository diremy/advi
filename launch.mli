val parse_shell_command : string -> string array
val fork_process : string -> int
val advi_process : int

val exit : int -> unit 
(* Same as [Pervasives.exit], but does not execute the functions
   registered by [at_exit] when the exiting process is forked one. 
   In the ADVI program, you MUST use this function instead of
   [Pervasives.exit] !!!! *)
