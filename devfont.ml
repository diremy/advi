(*
 * advi - A DVI previewer
 * Copyright (C) 2000  Alexandre Miquel
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
 *
 * See the GNU Lesser General Public License version 2.1 for more
 * details (enclosed in the file LGPL).
 *)

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
          Dev.make_glyph (Glyph.from_char_def cdef ratio) in
	let table = Table.make build in
	Hashtbl.add htable (fontname, sdpi) table ;
	table
end ;;
