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

val user_dir : string;;
(* Advi user directory. *)

val cache_dir : string;;
(* Advi cache directory. *)

val default_option_file : string;;
(* Advi user default options file ("~/.advirc"). *)

val options_files : unit -> string list;;
(* [options_files ()] returns the list of files to find options to set. *)

val fullpath : string -> string -> string;;
(* [fullpath dir path] returns the normalized full path name of
   [path] which is relative to the directory [dir]. *)

val tilde_subst : string -> string;;
(* Replaces the occurences of "~/" or "~username" to the corresponding
   path names. *)

val digdir : string -> int -> unit;;
(* Same as [Unix.mkdir], but it also creates parent directories as needed *) 

val prepare_file : string -> unit;;
(* [prepare_file file] prepares the directory for [file]: if
   necessary, sub-directories are created as required to create [file]. *)

val save_page_number : int -> unit;;
 (* [save_page_number n] saves the page number [n] to file
    [cache_dir ^ "/advi_page_number". *)

