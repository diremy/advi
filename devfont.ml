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
      (* find metrics table of given font at given dpi *)
  val find_glyphs : string -> float -> glyph Table.t
      (* find glyph table of given font at given dpi *)
end ;;

module Make (Font : FONT) (Glyph : GLYPH with type char_def = Font.char_def) =
  struct
  type glyph = Glyph.t

  let base_dpi = 600

  let find_metrics =
    let htable = Hashtbl.create 257 in
    fun fontname dpi ->
      let sdpi = int_of_float (ldexp dpi 16) in
      let dpi = ldexp (float sdpi) (-16) in
      try Hashtbl.find htable (fontname, sdpi)
      with Not_found ->
	let font = Font.find fontname base_dpi
	and ratio = dpi /. float base_dpi in
	let build code =
	  let cdef = Font.find_char_def font code in
	  (int_of_float (ratio *. float cdef.Font.dx),
	   int_of_float (ratio *. float cdef.Font.dy)) in
	let table = Table.make build in
	Hashtbl.add htable (fontname, sdpi) table ;
	table

  let find_glyphs =
    let htable = Hashtbl.create 257 in
    fun fontname dpi ->
      let sdpi = int_of_float (ldexp dpi 16) in
      let dpi = ldexp (float sdpi) (-16) in
      try Hashtbl.find htable (fontname, sdpi)
      with Not_found ->
	let font = Font.find fontname base_dpi
	and ratio = dpi /. float base_dpi in
	let build code =
	  let cdef = Font.find_char_def font code in
          Glyph.from_char_def cdef ratio in
	let table = Table.make build in
	Hashtbl.add htable (fontname, sdpi) table ;
	table
end ;;
