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

exception Pause;;
exception Wait of float
type cooked_dvi;;
val cook_dvi : Dvi.t -> cooked_dvi;;
val render_page : cooked_dvi -> int -> float -> int -> int -> unit;;
val render_step : cooked_dvi -> int -> ?trans:Transitions.direction -> 
         ?chst:(Dvi.known_status -> Dvi.known_status) -> float -> int -> int -> (unit -> bool);;
val unfreeze_fonts : cooked_dvi -> unit;;
val unfreeze_glyphs : cooked_dvi -> float -> unit;;
val scan_special_pages : cooked_dvi -> int -> unit;;
val clear_symbols : unit -> unit;;
