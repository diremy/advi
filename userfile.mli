(***********************************************************************)
(*                                                                     *)
(*                             Active-DVI                              *)
(*                                                                     *)
(*                   Projet Cristal, INRIA Rocquencourt                *)
(*                                                                     *)
(*  Copyright 2002 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the GNU Lesser General Public License.          *)
(*                                                                     *)
(*  Jun Furuse, Didier Rémy and Pierre Weis.                           *)
(*  Contributions by Roberto Di Cosmo, Didier Le Botlan,               *)
(*  Xavier Leroy, and Alan Schmitt.                                    *)
(*                                                                     *)
(*  Based on Mldvi by Alexandre Miquel.                                *)
(***********************************************************************)

val user_dir : Misc.dir_name;;
(* Advi user directory. *)

val get_cache_dir : unit -> Misc.dir_name;;
(* Advi cache directory. *)

val load_init_files : (string * Arg.spec * string) list ->
  (string -> unit) -> string -> unit;;
(* [load_init_files options anon usage_message] loads the init files
   [~/.advirc] and [~/.advi/advirc]. *)

val load_options_file : (string * Arg.spec * string) list ->
  (string -> unit) -> string -> Misc.file_name -> unit;;
(* [load_options_file options anon usage_message fname] loads the init file
   [fname]. *)

val fullpath : Misc.dir_name -> Misc.file_name -> Misc.file_name;;
(* [fullpath dir path] returns the normalized full path name of
   [path] which is relative to the directory [dir]. *)

val tilde_subst : Misc.file_name -> Misc.file_name;;
(* Replaces the occurences of "~/" or "~username" to the corresponding
   path names. *)

val digdir : Misc.dir_name -> int -> unit;;
(* Same as [Unix.mkdir], but it also creates parent directories as needed *) 

val prepare_file : Misc.file_name -> unit;;
(* [prepare_file file] prepares the directory for [file]: if
   necessary, sub-directories are created as required to create [file]. *)

val save_page_number : int -> unit;;
 (* [save_page_number n] saves the page number [n] to file
    [cache_dir ^ "/advi_page_number". *)

