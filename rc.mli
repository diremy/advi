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

(* Init file loading. *)
open Arg;;

val parse_file : string ->
  (string * spec * string) list -> (string -> unit) -> string -> unit
(** [Rc.parse_file fname speclist anonfun usage_msg] parses the file
  fname as if it were the command line.
  Syntax is exactly similar to a shell call to the command, except
  that the command call can spread on more than one line and comments
  are allowed (a comment starts with a sharp sign and ends at the end
  of the line). *)

val parse_string : string ->
  (string * spec * string) list -> (string -> unit) -> string -> unit
(** [Rc.parse_string s speclist anonfun usage_msg] parses the string [s]
  as if it were the command line. *)
