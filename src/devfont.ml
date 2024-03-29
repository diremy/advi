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

module type DEVICE = sig
  type glyph
  val make_glyph : Glyph.t -> glyph
end ;;

module type DEVFONT = sig
  type glyph
  val find_metrics : string -> float -> (int * int) Table.t
  val find_glyphs : string -> float -> glyph Table.t
end ;;

module Make (Dev : DEVICE) = struct
  type glyph = Dev.glyph

  let base_dpi = 600

  let find_metrics =
    let htable = Hashtbl.create 257 in
    fun fontname dpi ->
      let sdpi = int_of_float (ldexp dpi 16) in
      let dpi = ldexp (float sdpi) (-16) in
      try Hashtbl.find htable (fontname, sdpi)
      with Not_found ->
	let font = Font.find fontname base_dpi (* always 600 *)
	and ratio = dpi /. float base_dpi in
	let build code =
	  let cdef = Font.find_char_def font code in
	  (Misc.round (ratio *. float cdef.Font.dx),
	   Misc.round (ratio *. float cdef.Font.dy)) in
	let table = Table.make build in
	Hashtbl.add htable (fontname, sdpi) table ;
	table

  let find_glyphs =
    let htable = Hashtbl.create 257 in
    fun fontname dpi ->
      let sdpi = Misc.round (ldexp dpi 16) in
      let dpi = ldexp (float sdpi) (-16) in
      try Hashtbl.find htable (fontname, sdpi)
      with Not_found ->
	let font = Font.find fontname base_dpi
	and ratio = dpi /. float base_dpi in
	let build code =
	  let cdef = Font.find_char_def font code in
          Dev.make_glyph (Glyph.from_char_def cdef ratio) in
	let table = Table.make build in
	Hashtbl.add htable (fontname, sdpi) table ;
	table
end ;;
