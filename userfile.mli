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
(* advi user directory *)

val cache_dir : string;;
(* advi cache directory *)

val fullpath : string -> string -> string;;
(* [fululpath dir path] returns the normalized full path name of
   [path] which is relative to the directory [dir]. *)

val tilde_subst : string -> string;;
(* replace the occurences of "~/" or "~username" to the corresponding
   path names *)

val digdir : string -> int -> unit;;
(* Same as [Unix.mkdir], but it also creates parent directories as needed *) 

val prepare_file : string -> unit;;
(* [prepare_file file] prepares the directory for [file] *)

val save_page_no : int -> unit;;
 (* [save_page_no no] saves the page number [no] to file
    [cache_dir ^ "/advi_page_no". *)
