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
(*  Jun Furuse, Didier R�my and Pierre Weis.                           *)
(*  Contributions by Roberto Di Cosmo, Didier Le Botlan,               *)
(*  Xavier Leroy, and Alan Schmitt.                                    *)
(*                                                                     *)
(*  Based on Mldvi by Alexandre Miquel.                                *)
(***********************************************************************)

(* $Id$ *)

exception Terminated;;
val kill : unit -> unit;;
val draw : string -> int -> int -> unit;;
val add_headers : (bool * string) list -> unit;;
val newpage : (bool * string) list -> int -> float -> int -> int -> unit;;
val flush : unit -> unit;;
val toggle_antialiasing : unit -> unit;;
val current_x : int ref;;
val current_y : int ref;;

