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

module Japanese = struct
  (* Temporal hack for Japanese DVI (of pTeX) 
     This is really inefficient because we convert 
  
     freetype -[rendering]-> abstract font -[grayscale]-> graymap -> screen
  
     even though we can create graymap directly from freetype
  *)
  
  type jfonttype = Mincho | Gothic
  
  let japanese_fontfiles = [
    "min", Mincho, "/usr/share/fonts/TrueType/msmincho.ttc";
    "goth", Gothic, "/usr/share/fonts/TrueType/msgothic.ttc"
  ] 
  ;;
  
  let make_font =
    let facetable = Hashtbl.create 17 in
  
    fun fontname dpi ->
  
      let face,typ,pt,jfm =
  	try Hashtbl.find facetable fontname
  	with Not_found ->
  	  let rec search = function
  	    | [] -> raise Not_found
  	    | (pref,typ,file)::xs ->
  		try
  		  let name = String.sub fontname 0 (String.length pref) in
  		  let pt = 
  		    int_of_string (String.sub fontname (String.length name)
  		      (String.length fontname - String.length name)) in
  		  if name = pref then file, typ, pt
  		  else raise Exit
  		with
  		| _ -> search xs
  	  in
  	  let fontfile, typ, pt = search japanese_fontfiles in
 	  let face = Ttfont.load_face fontfile in
  	  let jfmname = 
  	    let jfm = fontname ^ ".tfm" in
  	    match Search.true_file_names [] [jfm] with
  	    | [n] -> n
  	    | _ -> 
  		prerr_endline (jfm ^ " not found");
  		raise Not_found 
  	  in
  	  let jfm = Jfm.load_jfm_file jfmname in
  	  Hashtbl.add facetable fontname (face,typ,pt,jfm);
  	  face,typ,pt,jfm
      in
  
      let build jiscode =
  	  let unicode = Ttfont.jis2uni jiscode in

	  (* metrics *) 
  	  let width = Jfm.find_width jfm jiscode in
  	  let dx = 
  	    Pervasives.truncate (float width *. float pt *. float dpi 
				   /. 1152.0) (* 72x16 *)
  	  in
  	  let x_fix =
  	    let fix = try List.assoc jiscode (match typ with
  					      | Mincho -> Jfm.monospace_fix
  					      | Gothic -> Jfm.monospace_fix)
  		      with _ -> 0.0
  	    in
  	    Pervasives.truncate (float pt *. float dpi /. 72.0 *. fix /. 1000.0)
  	  in

	  (* drawing using ttfont.build *)
	  let chardef = Ttfont.build face dpi pt unicode in

	  { code= chardef.Ttfont.code;
	    dx= chardef.Ttfont.dx + dx;
	    dy= chardef.Ttfont.dy;
	    width= chardef.Ttfont.width;
	    height= chardef.Ttfont.height;
	    hoffset= chardef.Ttfont.hoffset - x_fix;
	    voffset= chardef.Ttfont.voffset;
	    bitmap= chardef.Ttfont.bitmap }
      in
      { name= fontname; dpi= dpi; table= Table.make build }
  ;;

  (* wrapper : any error raises Not_found *)
  let make_font fontname dpi =
    try make_font fontname dpi with _ -> raise Not_found
  ;;
 
end

let find =
  let htable = Hashtbl.create 257 in

  fun fontname dpi ->
    try Hashtbl.find htable (fontname, dpi) with Not_found ->
    try 
      let font = Japanese.make_font fontname dpi in
      Hashtbl.add htable (fontname, dpi) font ;
      font
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
