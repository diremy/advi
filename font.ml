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

type char_def = {
    code : int ;
    dx : int ;
    dy : int ;
    width : int ;
    height : int ;
    hoffset : int ;
    voffset : int ;
    bitmap : string
  } ;;

type t = {
    name : string ;
    dpi : int ;
    table : char_def Table.t
  } ;;

(*** Converting PK fonts to abstract fonts ***)

let make_def_from_pk cdef =
  Pkfont.unpack cdef ;
  let bitmap =
    match cdef.Pkfont.bitmap with
    | Pkfont.Unpacked s -> s
    | Pkfont.Packed _ -> assert false in
  { code = cdef.Pkfont.code ;
    dx = cdef.Pkfont.dx ;
    dy = cdef.Pkfont.dy ;
    width = cdef.Pkfont.width ;
    height = cdef.Pkfont.height ;
    hoffset = cdef.Pkfont.hoffset ;
    voffset = cdef.Pkfont.voffset ;
    bitmap = bitmap } ;;

let make_font_from_pk font name dpi =
  let build code =
    make_def_from_pk (Pkfont.find_char_def font code) in
  { name = name ;
    dpi = dpi ;
    table = Table.make build }

(*** Finding a given font ***)

let find =
  let htable = Hashtbl.create 257 in
  fun fontname dpi ->
    try Hashtbl.find htable (fontname, dpi)
    with Not_found ->
      try
	let filename = Search.font_path fontname dpi in
	let pk_font = Pkfont.load filename in
	let font = make_font_from_pk pk_font fontname dpi in
	Hashtbl.add htable (fontname, dpi) font ;
	font
      with _ -> raise Not_found ;;

(*** Searching for a char_def in a given font ***)

let find_char_def font code =
  Table.get font.table code ;;
