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

(* $Id$ *)

(* A simple terminal to handle simple editions and user's interaction. *)

type term;;

val htab : term -> int -> unit;;
val vtab : term -> int -> unit;;

val get_line : term -> string;;
val ask : term -> string -> string;;

val print_str : term -> string -> unit;;
val print_chr : term -> char -> unit;;

val make_term : int -> int -> int -> int -> term;;
 (** [make_term x y nl ncol] build a terminal at position [x, y] with
  [nl] lines and [ncol] columns. *)
val draw_term : term -> unit;;
val set_title : term -> string -> unit;;
val edit : term -> unit;;

val make_term_gen :
 Graphics.color -> Graphics.color ->
 int -> Graphics.color ->
 Graphics.color -> Graphics.color ->
 int -> int -> int -> int -> term;;

 (** [make_term_gen fg bg bw bc tc x y nl ncol] build a terminal at
  position [x, y] with [nl] lines and [ncol] columns.  Colors [fg],
  [bg], [bc], [tc], and [cc] are respectively the background, foreground,
  border, title, and cursor colors. [bw] is the width of the border width. *)
