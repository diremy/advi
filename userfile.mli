val user_dir : string
(* advi user directory *)

val cache_dir : string
(* advi cache directory *)

val fullpath : string -> string -> string
(* [fululpath dir path] returns the normalized full path name of
   [path] which is relative to the directory [dir]. *)

val tilde_subst : string -> string
(* replace the occurences of "~/" or "~username" to the corresponding
   path names *)

val digdir : string -> int -> unit
(* Same as [Unix.mkdir], but it also creates parent directories as needed *) 

val prepare_file : string -> unit
(* [prepare_file file] prepares the directory for [file] *)
