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


module type FONT = sig
  type t
  type graymap
  type char_def = 
      { code : int;
	dx : int;
	dy : int;
	width : int;
	height : int;
	hoffset : int;
	voffset : int;
	graymap : graymap
      } 
  val find : string -> int -> t
  val find_char_def : t -> int -> char_def
end ;;

module type GLYPH = sig
  type t
  type char_def
  val from_char_def : char_def -> float -> t
end ;;

module type DEVFONT = sig
  type glyph
  val find_metrics : string -> float -> (int * int) Table.t
  val find_glyphs : string -> float -> glyph Table.t
end ;;

module Make (Font : FONT) (Glyph : GLYPH with type char_def = Font.char_def) :
    DEVFONT with type glyph = Glyph.t;;
