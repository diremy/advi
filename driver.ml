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

open Misc;;

(* number of steps before checking for user interrutions *)
let checkpoint_frequency = 10;;

(*** Some utilities for specials ***)

let split_string s start = Misc.split_string s ' ' start;;

(* "hello world" is one world *)
let rec split_string_quoted s start =
  let len = String.length s
  and i = ref start in
  (* find a space *)
  while !i < len && s.[!i] = ' ' do incr i; done; 
  if !i >= len then [] else begin
    let i0 = !i in
    while !i < len && s.[!i] <> ' ' do 
      if s.[!i] = '"' then begin
	incr i;
	while !i < len && s.[!i] <> '"' do incr i done;
	if s.[!i] <> '"' then failwith "parse error (split_string_quoted)";
	incr i
      end else incr i 
    done ;
    let i1 = !i in
    String.sub s i0 (i1 - i0) :: split_string_quoted s i1
  end ;;
(* '"' *)

(* "\"hello world\"" -> "hello world" *)
let unquote v =
  let s = if v.[0] = '"' then 1 else 0 in
  let l = if v.[String.length v - 1] = '"' then 
    String.length v - 1 - s else String.length v - s
  in
  String.sub v s l
;;

let rec split_record s =
  let tokens = split_string_quoted s 0 in
  List.map (fun token ->
    try 
      let i = String.index token '=' in
      String.sub token 0 i,
      String.sub token (i+1) (String.length token - i - 1)
    with
      _ -> token, "") tokens
;;


let named_colors = [
  "Black", 0x000000 ;
  "White", 0xffffff ;
  "Red", 0xff0000 ;
  "Green", 0x00ff00 ;
  "Blue", 0x0000ff ;
  "Cyan", 0x00ffff ;
  "Magenta", 0xff00ff ;
  "Yellow", 0xffff00
] ;;

let rgb r g b =
  (r lsl 16) + (g lsl 8) + b ;;

let cmyk c m y k =
  let r = 255 - c
  and g = 255 - m
  and b = 255 - y in
  (* k is ignored *)
  rgb r g b ;;

let parse_color_args = function
  | ["rgb"; rs; gs; bs] ->
      let r = int_of_float (255.0 *. float_of_string rs)
      and g = int_of_float (255.0 *. float_of_string gs)
      and b = int_of_float (255.0 *. float_of_string bs) in
      rgb r g b
  | ["cmyk"; cs; ms; ys; ks] ->
      let c = int_of_float (255.0 *. float_of_string cs)
      and m = int_of_float (255.0 *. float_of_string ms)
      and y = int_of_float (255.0 *. float_of_string ys)
      and k = int_of_float (255.0 *. float_of_string ks) in
      cmyk c m y k
  | ["gray"; gs] ->
      let g = int_of_float (255.0 *. float_of_string gs) in
      rgb g g g
  | [s] ->
      begin
        try List.assoc s named_colors
        with Not_found ->
          Format.eprintf "unknown color %s@." s ;
          0x000000
      end
  | _ -> 0x000000 ;;


module Dev = Grdev
module Symbol = Dev.Symbol
module DFont = Devfont.Make(Dev)
let base_dpi = 600
    
  (*** Cooked fonts ***)
    
exception Pause
    
type cooked_font = {
    name : string ;
    ratio : float ;
    mtable : (int * int) Table.t ;
    mutable gtables : (int * Dev.glyph Table.t) list
  }
      
let dummy_mtable = Table.make (fun _ -> raise Not_found)
let dummy_gtable = Table.make (fun _ -> raise Not_found)
let dummy_font =
  { name = "--nofont--" ; ratio = 1.0 ; mtable = dummy_mtable ; gtables = [] }
    
let cook_font fdef dvi_res =
  let name = fdef.Dvi.name
  and sf = fdef.Dvi.scale_factor
  and ds = fdef.Dvi.design_size in
  let ratio = float sf /. float ds in
  let mtable =
    try DFont.find_metrics name (dvi_res *. ratio)
    with Not_found -> dummy_mtable in
  { name = name ;
    ratio = ratio ;
    mtable = mtable ;
    gtables = [] }
    
let get_gtable cfont sdpi =
  try List.assoc sdpi cfont.gtables
  with Not_found ->
    let dpi = ldexp (float sdpi) (-16) in
    let table =
      try DFont.find_glyphs cfont.name (dpi *. cfont.ratio)
      with Not_found -> dummy_gtable in
    cfont.gtables <- (sdpi, table) :: cfont.gtables ;
    table
      
  (*** Cooked DVI's ***)
      
type cooked_dvi = {
    base_dvi : Dvi.t ;
    dvi_res : float ;
    font_table : cooked_font Table.t
  }
      
let cook_dvi dvi =
  let dvi_res = 72.27 in
  let build n =
    cook_font (List.assoc n dvi.Dvi.font_map) dvi_res in
  { base_dvi = dvi ;
    dvi_res = dvi_res ;
    font_table = Table.make build }
    
  (*** The rendering state ***)
    
type reg_set = {
    reg_h : int ;
    reg_v : int ;
    reg_w : int ;
    reg_x : int ;
    reg_y : int ;
    reg_z : int
  }
      
type color = int
      
type state = {
    cdvi : cooked_dvi ;
    sdpi : int ;
    conv : float ;
    x_origin : int ;
    y_origin : int ;
      (* Current font attributes *)
    mutable cur_font : cooked_font ;
    mutable cur_mtable : (int * int) Table.t ;
    mutable cur_gtable : Dev.glyph Table.t ;
      (* Registers *)
    mutable h : int ;
    mutable v : int ;
    mutable w : int ;
    mutable x : int ;
    mutable y : int ;
    mutable z : int ;
      (* Register stack *)
    mutable stack : reg_set list ;
      (* Color & Color stack *)
    mutable color : color ;
    mutable color_stack : color list;
      (* Other attributes *)
    mutable alpha : float;
    mutable alpha_stack : float list;
    mutable blend : Dev.blend;
    mutable blend_stack : Dev.blend list;
    mutable epstransparent : bool;
    mutable epstransparent_stack : bool list;
    mutable transition : Transitions.t;
    mutable transition_stack : Transitions.t list;
      (* TPIC specials state *)
    mutable tpic_pensize : float;
    mutable tpic_path : (float * float) list;
    mutable tpic_shading : float;
      (* PS specials page state *)
    mutable status : Dvi.known_status; 
    mutable headers : string list;
    mutable html : (Dev.H.tag * int) option ;
    mutable draw_html : (int * int * Dev.glyph) list;
    mutable checkpoint : int; 
    }
        
  type proc_unit = {
      escaped_register : reg_set;
      escaped_cur_font : cooked_font ;
      escaped_cur_mtable : (int * int) Table.t;
      escaped_cur_gtable : Dev.glyph Table.t;
      mutable escaped_commands : Dvi.command list
    }	
	
let procs = Hashtbl.create 107
let current_recording_proc_name = ref None
let current_recording_proc_unit = ref None
let hidden = ref false	
let is_hidden () = !hidden	
let is_recording () = !current_recording_proc_name <> None
      
  (*** Rendering primitives ***)
    
let last_height = ref 0 
let clear_symbols() = last_height := 2 
    
let add_char st x y code glyph =
  let g : Symbol.g = 
    { Symbol.fontname = st.cur_font.name ;
      Symbol.fontratio = st.cur_font.ratio ;
      Symbol.glyph = glyph
    } in
  last_height := (Dev.get_glyph glyph).Glyph.voffset ;
  let s : Symbol.symbol = Symbol.Glyph g in
  Symbol.add st.color x y code s;
  ()

let add_line st (line, file) =
  let x = st.x_origin + int_of_float (st.conv *. float st.h)
  and y = st.y_origin + int_of_float (st.conv *. float st.v) in
  Symbol.add st.color x y 0 (Symbol.Line (line, file))
    
let add_blank nn st width =
  let x = st.x_origin + int_of_float (st.conv *. float st.h)
  and y = st.y_origin + int_of_float (st.conv *. float st.v)
  and w = int_of_float (st.conv *. float width) in
  Symbol.add st.color x y nn (Symbol.Space (w, !last_height))
    
let add_rule st x y w h =
  Symbol.add st.color x y 0 (Symbol.Rule (w, h))
    
let get_register_set st =
  { reg_h = st.h ; reg_v = st.v ;
    reg_w = st.w ; reg_x = st.x ;
    reg_y = st.y ; reg_z = st.z }
    
let set_register_set st rset =
  st.h <- rset.reg_h ;
  st.v <- rset.reg_v ;
  st.w <- rset.reg_w ;
  st.x <- rset.reg_x ;
  st.y <- rset.reg_y ;
  st.z <- rset.reg_z
      
let push st =
  st.stack <- (get_register_set st) :: st.stack
                                        
let pop st =
  match st.stack with
  | [] -> ()
  | rset :: rest ->
      set_register_set st rset;
      st.stack <- rest
          
let color_push st col =
  st.color_stack <- st.color :: st.color_stack ;
  st.color <- col ;
  if not (is_hidden ()) then Dev.set_color col
      
let color_pop st =
  match st.color_stack with
  | [] -> ()
  | col :: rest ->
      st.color <- col ;
      if not (is_hidden ()) then Dev.set_color col ;
      st.color_stack <- rest
          
let alpha_push st v =
  st.alpha_stack <- st.alpha :: st.alpha_stack ;
  st.alpha <- v ;
  if not (is_hidden ()) then Dev.set_alpha v
      
let alpha_pop st =
  match st.alpha_stack with
  | [] -> ()
  | v :: rest ->
      st.alpha <- v ;
      if not (is_hidden ()) then Dev.set_alpha v ;
      st.alpha_stack <- rest
          
let blend_push st v =
  st.blend_stack <- st.blend :: st.blend_stack ;
  st.blend <- v ;
  if not (is_hidden ()) then Dev.set_blend v
      
let blend_pop st =
  match st.blend_stack with
  | [] -> ()
  | v :: rest ->
      st.blend <- v ;
      if not (is_hidden ()) then Dev.set_blend v ;
      st.blend_stack <- rest
          
let epstransparent_push st v =
  st.epstransparent_stack <- st.epstransparent :: st.epstransparent_stack ;
  st.epstransparent <- v ;
  if not (is_hidden ()) then Dev.set_epstransparent v
      
let epstransparent_pop st =
  match st.epstransparent_stack with
  | [] -> ()
  | v :: rest ->
      st.epstransparent <- v ;
      if not (is_hidden ()) then Dev.set_epstransparent v ;
      st.epstransparent_stack <- rest
          
let transition_push st v =
  st.transition_stack <- st.transition :: st.transition_stack ;
  st.transition <- v ;
  if not (is_hidden ()) then Dev.set_transition v
      
let transition_pop st =
  match st.transition_stack with
  | [] -> ()
  | v :: rest ->
      st.transition <- v ;
      if not (is_hidden ()) then Dev.set_transition v ;
      st.transition_stack <- rest
          
let fnt st n =
  let (mtable, gtable, cfont) =
    try
      let cfont = Table.get st.cdvi.font_table n in
      (cfont.mtable, get_gtable cfont st.sdpi, cfont)
    with Not_found -> (dummy_mtable, dummy_gtable, dummy_font) in
  st.cur_mtable <- mtable ;
  st.cur_gtable <- gtable ;
  st.cur_font <- cfont ;
  ()
    
let put st code =
  try
    let x = st.x_origin + int_of_float (st.conv *. float st.h)
    and y = st.y_origin + int_of_float (st.conv *. float st.v)
    and glyph = Table.get st.cur_gtable code in
    if not (is_hidden ()) then
      begin
        begin match st.html with
        | Some _ -> 
            st.draw_html <- (x, y, glyph) :: st.draw_html
        | None -> ()
        end;
        Dev.draw_glyph (glyph : Dev.glyph) x y;
	add_char st x y code glyph ;
      end
  with _ -> ()
      
let set st code =
  put st code ;
  try
    let (dx, dy) = Table.get st.cur_mtable code in
    st.h <- st.h + dx ;
    st.v <- st.v + dy
  with _ -> ()
      
let put_rule st a b =
  let x = st.x_origin + int_of_float (st.conv *. float st.h)
  and y = st.y_origin + int_of_float (st.conv *. float st.v)
  and w = int_of_float (ceil (st.conv *. float b))
  and h = int_of_float (ceil (st.conv *. float a)) in
  add_rule st x (y-h) w h ;
  if not (is_hidden ()) then Dev.fill_rect x (y - h) w h
      
let set_rule st a b =
  put_rule st a b ;
  st.h <- st.h + b
      
  (*** Specials ***)
      
    let line_special st s =
      match split_string s 0 with
      | key :: line :: rest ->
          begin try
            let l = int_of_string line in
            let f = match rest with file :: _ -> Some file | _ -> None in
            add_line st (l, f)
          with Failure _ ->
            Misc.warning
              (Printf.sprintf "Ill formed special <<%s>>" s)
          end
      | _ -> Misc.warning "Ill formed line: special" 
            
            
    let color_special st s =
      match split_string s 0 with
      | "color" :: "push" :: args ->
          color_push st (parse_color_args args)
      | "color" :: "pop" :: [] ->
          color_pop st
      | _ -> ()
            ;;

  let alpha_special st s =
    match split_string s 0 with
    | ["advi:"; "alpha"; "push"; arg] ->
	alpha_push st 
	  (try float_of_string arg 
	  with _ -> raise (Failure "advi: invalid alpha"))
    | ["advi:"; "alpha"; "pop"] ->
	alpha_pop st
    | _ -> ()
  ;;

  let parse_blend s =
    match String.lowercase s with
    | "normal" -> Dev.Normal
    | "multiply" -> Dev.Multiply
    | "screen" -> Dev.Screen
    | "overlay" -> Dev.Overlay
    | "dodge" -> Dev.ColorDodge
    | "burn" -> Dev.ColorBurn
    | "darken" -> Dev.Darken
    | "lighten" -> Dev.Lighten
    | "difference" -> Dev.Difference
    | "exclusion" -> Dev.Exclusion
    | _ -> raise (Failure "blend: invalid blend mode")
  ;;
  	  
  let blend_special st s =
    match split_string s 0 with
    | ["advi:"; "blend"; "push"; arg] ->
	blend_push st (parse_blend arg)
    | "advi:" :: "blend" :: "pop" :: [] ->
	blend_pop st
    | _ -> ()
  ;;

  let parse_epstransparent s =
    match String.lowercase s with
    | "true" -> true
    | "false" -> false
    | _ -> raise (Failure "epstransparent: invalid mode")
  ;;
  	  
  let epstransparent_special st s =
    match split_string s 0 with
    | ["advi:"; "epstransparent"; "push"; arg] ->
	epstransparent_push st (parse_epstransparent arg)
    | "advi:" :: "epstransparent" :: "pop" :: [] ->
	epstransparent_pop st
    | _ -> ()
  ;;

  let psfile_special st s =
    let records = List.map (fun (k,v) -> 
      String.lowercase k, v) (split_record s)
    in
    let file = 
      try
  	  unquote (List.assoc "psfile" records)
      with Not_found -> raise (Failure "psfile: invalid special")
    in
    (* prerr_endline ("PSFILE=" ^ file); *)
    (* bbox *)
    let llx, lly, urx, ury =
      try
  	  let llx = int_of_string (List.assoc "llx" records) 
  	  and lly = int_of_string (List.assoc "lly" records) 
  	  and urx = int_of_string (List.assoc "urx" records) 
  	  and ury = int_of_string (List.assoc "ury" records) 
  	  in
  	  (* prerr_endline
              ("BBOX=" ^ Printf.sprintf "%d %d %d %d" llx lly urx ury); *)
  	  llx, lly, urx, ury
      with
      |	_ -> raise (Failure "psfile: no bbox")
    in
    let width, height = (* return Big Points *)
      let w = try int_of_string (List.assoc "rwi" records) with _ -> 0
      and h = try int_of_string (List.assoc "rhi" records) with _ -> 0
      in
      match w,h with
      |	0,0 -> float (urx - llx), float (ury - lly)
      |	0,_ -> 
  	    let h = float h /. 10.0 in
  	    let w = float (urx - llx) *. (h /. float (ury - lly)) in
  	    w, h
      |	_,0 -> 
  	    let w = float w /. 10.0 in
  	    let h = float (ury - lly) *. (w /. float (urx - llx)) in
  	    w, h
      |	_,_ -> float w /. 10.0, float h /. 10.0
    in
    let dpi = ldexp (float st.sdpi) (-16) in
    let width_pixel = truncate (width /. 72.0 *. dpi) in
    let height_pixel = truncate (height /. 72.0 *. dpi) in
    (* prerr_endline (Printf.sprintf "%dx%d pixel" width_pixel height_pixel);*)
    file, (llx, lly, urx, ury), (width_pixel, height_pixel)

  let kill_embed_special st s =
    (* advi: kill name=? *)
    let records = List.map (fun (k,v) -> 
      String.lowercase k, v) (split_record s)
    in
    let app_name =
      try
  	 unquote (List.assoc "name" records)
      with Not_found -> raise (Failure ("No command to kill in " ^ s))
    in
    Dev.kill_embedded_app app_name
    

  let app_type_of_string = function
    | "sticky" -> Dev.Sticky
    | "persistent" -> Dev.Persistent
    | "ephemeral" -> Dev.Ephemeral
    | s -> raise (Failure ("Unknown embedding type " ^ s))

  let embed_special st s =
    (* advi: embed type=? width=? height=? command="command string" *)
    let records = List.map (fun (k,v) -> 
      String.lowercase k, v) (split_record s)
    in
    let app_type =
      try
  	 app_type_of_string (List.assoc "type" records)
      with Not_found ->
        raise (Failure ("embed: no embedding type in special " ^ s))
    in
    let app_name =
      try
  	 unquote (List.assoc "name" records)
      with Not_found -> ""
    in
   
    let command =
      try
  	  unquote (List.assoc "command" records)
      with Not_found ->
          raise (Failure ("embed: no command to embed in special " ^ s))
    in
    (* prerr_endline ("embed command=" ^ command); *)
    let width_pixel, height_pixel =
      let w, h =
	try
	  let width = 
	    match 
	      Dimension.normalize (Dimension.dimen_of_string 
				     (List.assoc "width" records)) 
	    with
	      Dimension.In w -> w
            | _ -> assert false 
	  in
	  let height = 
	    match
  	      Dimension.normalize (Dimension.dimen_of_string
  				     (List.assoc "height" records))
	    with
	      Dimension.In h -> h
            | _ -> assert false 
	  in
	  width, height
	with
	| _ -> raise (Failure ("embed: no size in special " ^ s))
      in
      let dpi = ldexp (float st.sdpi) (-16) in
      let width_pixel = truncate (w *. dpi) in
      let height_pixel = truncate (h *. dpi) in
   (* prerr_endline (Printf.sprintf "%d x %d pixel" width_pixel height_pixel);*)
      width_pixel, height_pixel
    in
    let x = st.x_origin + int_of_float (st.conv *. float st.h)
    and y = st.y_origin + int_of_float (st.conv *. float st.v) in
    if not (is_hidden ()) then
      Dev.embed_app command app_type app_name width_pixel height_pixel x y


  let parse_transition mode record =
    let parse_steps default =
      try 
	let stepsstr = List.assoc "steps" record in
	try int_of_string stepsstr 
	with _ -> 
	  warning "special: trans push: steps parse failed"; 
	  default
      with Not_found -> default
    in
    let parse_direction key default =
      try 
	match String.lowercase (List.assoc key record) with
	| "left" -> Transitions.DirLeft
	| "right" -> Transitions.DirRight
	| "top" -> Transitions.DirTop
	| "bottom" -> Transitions.DirBottom
	| "topleft" -> Transitions.DirTopLeft
	| "topright" -> Transitions.DirTopRight
	| "bottomleft" -> Transitions.DirBottomLeft
	| "bottomright" -> Transitions.DirBottomRight
	| "center" -> Transitions.DirCenter
	| _ -> 	
	    warning "special: trans push: steps parse failed" ;
	    raise Exit
      with _ -> Transitions.DirNone
    in
    match String.lowercase mode with
    | "slide" -> Transitions.TransSlide (parse_steps 20, 
			     parse_direction "from" Transitions.DirRight)
    | "wipe" -> Transitions.TransWipe (parse_steps 20, 
			     parse_direction "from" Transitions.DirRight)
    | "block" -> Transitions.TransBlock (parse_steps 5000,
			     parse_direction "from" Transitions.DirNone)
    | "none" ->  Transitions.TransNone
    | _ -> warning "special: trans push: mode parse failed"; Transitions.TransNone
  ;;

  let transition_special st s =
    match split_string s 0 with
    | "advi:" :: "trans" :: "push" :: mode :: args ->
	let record = split_record (String.concat " " args) in 
	let trans = parse_transition mode record in
	transition_push st trans
    | "advi:" :: "trans" :: "pop" :: [] ->
	transition_pop st
    | _ -> ()
  ;;

    let eval_command_ref = ref (fun _ _ -> ())

    let proc_clean () =
      current_recording_proc_name := None;
      current_recording_proc_unit := None;
      Hashtbl.clear procs

    let proc_special st s =
      let records = List.map (fun (k,v) -> 
  	String.lowercase k, v) (split_record s)
      in
      let procname = 
      	try
  	  unquote (List.assoc "proc" records)
      	with Not_found -> raise (Failure "proc: invalid special")
      in
      try
	let v = List.assoc "record" records in
	match v with
	| "start" ->
	    if !current_recording_proc_name <> None ||
	    !current_recording_proc_unit <> None then begin
	      prerr_endline
               (Printf.sprintf "proc=%s record=start: cannot be recorded"
                 procname)
            end else begin
	      hidden := 
		(try ignore (List.assoc "play" records); false with _ -> true);
	      current_recording_proc_name := Some procname;
	      current_recording_proc_unit := 
		 Some { escaped_register= get_register_set st;
			escaped_cur_mtable= st.cur_mtable;
			escaped_cur_gtable= st.cur_gtable;
			escaped_cur_font = st.cur_font;
		      	escaped_commands= [] }
	    end
	| "end" ->
	    if !current_recording_proc_name <> Some (procname) ||
	    !current_recording_proc_unit = None then begin
	      prerr_endline
               (Printf.sprintf "proc=%s record=end: not recorded" procname)
            end else begin
	      let v = 
		try 
		  let v = Hashtbl.find procs procname in
		  Hashtbl.remove procs procname;
		  v
		with Not_found -> [] 
	      in
	      match !current_recording_proc_unit with
	      |	Some u ->
		  Hashtbl.add procs procname (v @ [u]);
		  current_recording_proc_name := None;
		  current_recording_proc_unit := None;
		  hidden := false
	      |	None -> assert false
	    end
	| _ -> ()
      with
      |	Not_found -> 
	  try 
	    ignore (List.assoc "play" records);
	    let us = Hashtbl.find procs procname in
	    let escaped_cur_font = st.cur_font
	    and escaped_cur_mtable = st.cur_mtable
	    and escaped_cur_gtable = st.cur_gtable in
	    let escaped_stack = push st; st.stack in
	    List.iter (fun u -> 
	      set_register_set st u.escaped_register;
	      st.cur_mtable <- u.escaped_cur_mtable;
	      st.cur_gtable <- u.escaped_cur_gtable;
	      st.cur_font <- u.escaped_cur_font;
	      List.iter (fun com -> 
		!eval_command_ref st com) u.escaped_commands
	      ) us;
            st.stack <- escaped_stack; pop st;
	    st.cur_mtable <- escaped_cur_mtable;
	    st.cur_gtable <- escaped_cur_gtable;
	    st.cur_font <- escaped_cur_font;
	  with
	  | Not_found -> 
	      prerr_endline
                (Printf.sprintf "proc=%s play: not recorded" procname)

    let wait_special st s =
      let records = List.map (fun (k,v) -> 
  	(* prerr_endline k; prerr_endline v; *)
  	String.lowercase k, v) (split_record s)
      in
      let second = 
      	try
  	  float_of_string (List.assoc "sec" records)
      	with Not_found -> raise (Failure "wait: invalid special")
      in
      Dev.synchronize();
      if not (is_hidden()) then Dev.sleep second;
      st.checkpoint <- 0
    ;;




    (* Background object configuration. RDC *)
    
    let setup_bkgd status = (* propagate bkgd preferences to graphics device *)
      Dev.copy_bkgd_data status.Dvi.bkgd_prefs Dev.bkgd_data;
      Dev.set_bg_options status.Dvi.bkgd_local_prefs;
      Dev.copy_bkgd_data Dev.bkgd_data status.Dvi.bkgd_prefs
      ;;

    let bkgd_alist = [
      ("col",fun s -> fun st -> [Dev.BgColor (parse_color_args (split_string (unquote s) 0))]);
      ("img",fun s -> fun st -> [Dev.BgImg s]);
      ("reset",fun s -> fun st -> Dev.copy_bkgd_data (Dev.default_bkgd_data ()) st.Dvi.bkgd_prefs; []) 
      (***RDC: is this code ---/\ doing the rest of data we want to see ? ***)
    ] ;;


    let filter_alist alist falist =
      let aux k alist okalist = 
        try
          (k,List.assoc k alist)::okalist
        with Not_found -> okalist
      in
      List.fold_left (fun l -> fun k -> aux k alist l) [] (List.map (fun (k,v) -> k) falist);;
      

    (* When scanning the page, we just fill the info structure for backgrounds *)
    let scan_bkgd_special st s =
      let records = List.map (fun (k,v) -> String.lowercase k, v) (split_record s)
      in
      st.Dvi.bkgd_local_prefs <- 
          (List.flatten (List.map (fun (k,v) -> (List.assoc k bkgd_alist) v st) (filter_alist records bkgd_alist)))
          @st.Dvi.bkgd_local_prefs
    ;;

    (* When not scanning, we ignore the background information *)
    let bkgd_special st s = ()
    ;;

    (* Support for TPIC specials.  XL. *)

    let milli_inch_to_sp = Units.from_to Units.IN Units.SP 1e-3

    let tpic_milli_inches s = float_of_string s *. milli_inch_to_sp

    let tpic_pen st =
      int_of_float (st.conv *. st.tpic_pensize +. 0.5)
    let tpic_x st x =
      st.x_origin + int_of_float (st.conv *. (float st.h +. x))
    let tpic_y st y =
      st.y_origin + int_of_float (st.conv *. (float st.v +. y))

    let tpic_flush_path st cntr =
      let path = Array.of_list (List.rev st.tpic_path) in
      (* Convert points in path to pixel coordinates *)
      let pixpath =
        Array.map (fun (x, y) -> (tpic_x st x, tpic_y st y)) path in
      (* If shading requested and path is closed, fill *)
      if st.tpic_shading > 0.0 &&
         Array.length path >= 2 &&
         path.(0) = path.(Array.length path - 1)
      then Dev.fill_path pixpath ~shade:st.tpic_shading;
      (* If requested, draw outline of path *)
      if cntr then Dev.draw_path pixpath ~pensize:(tpic_pen st);
      (* Reset path *)
      st.tpic_path <- [];
      st.tpic_shading <- 0.0

    let dist (x0,y0) (x1,y1) = abs(x0 - x1) + abs(y0 - y1)

    let tpic_spline_path st =
      (* Code shamelessly stolen from xdvi *)
      let path =
        Array.of_list
          (List.map (fun (x, y) -> (tpic_x st x, tpic_y st y))
                    (List.rev st.tpic_path)) in
      let p =
        Array.concat [[|path.(0)|]; path; [|path.(Array.length path - 1)|]] in
      let r = ref [] in
      for i = 0 to Array.length p - 3 do
        let steps = (dist p.(i) p.(i+1) + dist p.(i+1) p.(i+2)) / 4 in
        let (x2, y2) = p.(i+2)
        and (x1, y1) = p.(i+1)
        and (x0, y0) = p.(i) in
        for j = 0 to steps - 1 do
          let w = (j * 1000 + 500) / steps in
          let t1 = w * w / 20 in
          let w = w - 500 in
          let t2 = (750000 - w * w) / 10 in
          let w = w - 500 in
          let t3 = w * w / 20 in
          let xp = (t1 * x2 + t2 * x1 + t3 * x0 + 50000) / 100000
          and yp = (t1 * y2 + t2 * y1 + t3 * y0 + 50000) / 100000 in
          r := (xp, yp) :: !r
        done
      done;
      Dev.draw_path (Array.of_list (List.rev !r)) ~pensize:(tpic_pen st);
      st.tpic_path <- [];
      st.tpic_shading <- 0.0

    let rad_to_deg = 45.0 /. atan 1.0

    let tpic_arc st x y rx ry s e cntr =
      let x = tpic_x st x
      and y = tpic_y st y
      and rx = int_of_float (st.conv *. rx)
      and ry = int_of_float (st.conv *. ry)
      and s = int_of_float (s *. rad_to_deg)
      and e = int_of_float (e *. rad_to_deg) in
      (* If shading requested, fill the arc *)
      if st.tpic_shading > 0.0 then
        Dev.fill_arc ~x ~y ~rx ~ry ~start:s ~stop:e ~shade:st.tpic_shading;
      (* If requested, draw outline of arc *)
      if cntr then
        Dev.draw_arc ~x ~y ~rx ~ry ~start:s ~stop:e ~pensize:(tpic_pen st);
      (* Reset shading *)
      st.tpic_shading <- 0.0

    let tpic_specials st s =
      match split_string s 0 with
        "pn" :: size :: _ ->
          st.tpic_pensize <- tpic_milli_inches size
      | "pa" :: x :: y :: _ ->
          st.tpic_path <-
            (tpic_milli_inches x, tpic_milli_inches y) :: st.tpic_path
      | "fp" :: _ ->
          tpic_flush_path st true
      | "ip" :: _ ->
          tpic_flush_path st false
      | "da" :: _ -> (* TODO: dashed lines *)
          tpic_flush_path st true
      | "dt" :: _ -> (* TODO: dotted lines *)
          tpic_flush_path st true
      | "sp" :: _ -> (* TODO: dashed/dotted splines *)
          tpic_spline_path st
      | "ar" :: x :: y :: rx :: ry :: s :: e :: _ ->
          tpic_arc st (tpic_milli_inches x) (tpic_milli_inches y) 
                   (tpic_milli_inches rx) (tpic_milli_inches ry) 
                   (float_of_string s) (float_of_string e)
                   true
      | "ia" :: x :: y :: rx :: ry :: s :: e :: _ ->
          tpic_arc st (tpic_milli_inches x) (tpic_milli_inches y) 
                   (tpic_milli_inches rx) (tpic_milli_inches ry) 
                   (float_of_string s) (float_of_string e)
                   true
      | "sh" :: s :: _ ->
          st.tpic_shading <- float_of_string s
      | "wh" :: _ ->
          st.tpic_shading <- 0.0
      | "bk" :: _ ->
          st.tpic_shading <- 1.0
      | _ ->
          ()
    (* End of TPIC hacks *)

    let moveto_special st s =
      if !Misc.dops then
        begin
          let x, y = Dev.current_pos() in
          st.h <- int_of_float (float (x - st.x_origin) /. st.conv) ;
          st.v <- int_of_float (float (y - st.y_origin) /. st.conv) ;
(*
      st.h <- st.h + int_of_float (float x /. st.conv) ;
      st.v <- st.v + int_of_float (float y /. st.conv) ;
*)
        end


    let ps_special st s = 
      if !Misc.dops && st.status.Dvi.hasps then
          let x = int_of_float (st.conv *. float st.h) in
          let y = int_of_float (st.conv *. float st.v) in
          if not (is_hidden ()) then
            begin try
              Dev.exec_ps s x y
            with Dev.GS ->
              st.status.Dvi.hasps <- false
            end

    let header_special st s = ()      (* header are not "rendered", only stored during scan *)

    (* For html specials *)


  (* Should check that a pause is not in the middle of html code *)

  let open_html st html tag tag_string = 
    let name = String.sub html 9 (String.length html -11) in
    let x = st.x_origin + int_of_float (st.conv *. float st.h)
    and y = st.y_origin + int_of_float (st.conv *. float st.v) in
    begin match st.html with
    | Some (t, k) ->
        st.html <- Some (t, succ k)
    | None -> 
        st.html <- Some (tag name, 0)
    end


  let close_html st = 
    match st.html with
    | Some (tag, k) when k > 0 ->
        st.html <- Some (tag, k-1)
    | Some (tag, 0) ->
        Dev.H.add {Dev.H.tag =tag; Dev.H.draw = List.rev st.draw_html};
        st.html <- None;
        st.draw_html <- []
    | Some (_, k) -> assert false
    | None -> warning ("Closing html tag that is not open")

  let html_special st html = 
    if has_prefix "<A name=\"" html || has_prefix "<a name=\"" html then
      open_html st html (fun x -> Dev.H.Name x) "Name"
    else if  has_prefix "<A href=\"" html || has_prefix "<a href=\"" html then
        open_html st html (fun x -> Dev.H.Href x) "Href"
    else if  has_prefix "<A advi=\"" html || has_prefix "<a advi=\"" html then
      let advi x =
        let play() = proc_special st ("advi: proc="^x^" play") in
        Dev.H.Advi (x, play) in
        open_html st html advi "Advi"
    else if has_prefix "</A>" html || has_prefix "</a>" html then
      close_html st 
    else
      warning ("Unknown html suffix" ^ html)

    let scan_special_html (headers,xrefs) page s =
       let name = String.sub s 14 (String.length s - 16) in
       Hashtbl.add  xrefs name page

    let scan_special status (headers,xrefs) pagenum s =
      (* Embedded Postscript, better be first for speed when scanning *)
      if has_prefix "\" " s || has_prefix "ps: " s then 
          (if !Misc.dops then status.Dvi.hasps <- true)
      else if has_prefix "header=" s then 
          (if !Misc.dops then
              headers := (get_suffix "header=" s) :: !headers)
      else if has_prefix "advi: setbg " s then scan_bkgd_special status s
      else if has_prefix "html:<A name=\"" s || has_prefix "html:<a name=\"" s then 
        scan_special_html (headers,xrefs) pagenum s

    let scan_special_page cdvi globals pagenum =
       let page = cdvi.base_dvi.Dvi.pages.(pagenum) in
           match page.Dvi.status with
            | Dvi.Unknown ->  
               let status = {Dvi.hasps=false;Dvi.bkgd_local_prefs=[];Dvi.bkgd_prefs=Dev.bkgd_data} in
               let eval = function
                  Dvi.C_xxx s -> scan_special status globals pagenum s
                  | _ -> () 
               in
               Dvi.page_iter eval cdvi.base_dvi.Dvi.pages.(pagenum);
               page.Dvi.status <- Dvi.Known status; 
               status
            | Dvi.Known stored_status -> stored_status

    let special st s =
      if has_prefix "\" " s || has_prefix "ps: " s then ps_special st s
      else if has_prefix "advi: moveto" s then moveto_special st s 
      else

      (* Other specials *)
      if has_prefix "color " s then color_special st s
      else if has_prefix "html:" s then html_special st (get_suffix "html:" s)
      else if has_prefix "PSfile=" s || has_prefix "psfile=" s then
        begin
  	  let file, bbox, size = psfile_special st s in
      	  let x = st.x_origin + int_of_float (st.conv *. float st.h)
      	  and y = st.y_origin + int_of_float (st.conv *. float st.v) in
	  if not (is_hidden ()) then Dev.draw_ps file bbox size x y
        end
      else if has_prefix "advi: " s then
        begin
	  if has_prefix "advi: alpha" s then
	    alpha_special st s
	  else if has_prefix "advi: blend" s then
	    blend_special st s
	  else if has_prefix "advi: epstransparent" s then
	    epstransparent_special st s
      	  else if has_prefix "advi: pause" s then raise Pause
      	  else if has_prefix "advi: proc=" s then proc_special st s
      	  else if has_prefix "advi: wait " s then wait_special st s
	  else if has_prefix "advi: embed " s then embed_special st s
	  else if has_prefix "advi: trans " s then transition_special st s
	  else if has_prefix "advi: kill " s then kill_embed_special st s
	  else if has_prefix "advi: setbg " s then bkgd_special st s
      	  else if has_prefix "advi:" s then 
	    raise (Failure ("unknown special: "^ s))
        end
      else if has_prefix "line: " s then line_special st s
      else if
        has_prefix "pn " s || has_prefix "pa " s
      || s = "fp" || s = "ip"
      || has_prefix "da " s || has_prefix "dt " s
      || s = "sp" || has_prefix "sp " s || has_prefix "ar " s
      || has_prefix "ia " s || has_prefix "sh " s
      || s = "wh" || s = "bk" then
        tpic_specials st s

  (*** Page rendering ***)

  let eval_command st = function
    | Dvi.C_set code -> set st code
    | Dvi.C_put code -> put st code
    | Dvi.C_set_rule(a, b) -> set_rule st a b
    | Dvi.C_put_rule(a, b) -> put_rule st a b
    | Dvi.C_push -> push st
    | Dvi.C_pop -> pop st
    | Dvi.C_right k -> add_blank 1 st k; st.h <- st.h + k
    | Dvi.C_w0 ->  add_blank 2 st st.w; st.h <- st.h + st.w
    | Dvi.C_w k -> st.w <- k ; add_blank 3 st st.w; st.h <- st.h + st.w
    | Dvi.C_x0 ->  add_blank 4 st st.x; st.h <- st.h + st.x
    | Dvi.C_x k -> st.x <- k ;  add_blank 5 st st.x; st.h <- st.h + st.x
    | Dvi.C_down k -> st.v <- st.v + k
    | Dvi.C_y0 -> st.v <- st.v + st.y
    | Dvi.C_y k -> st.y <- k ; st.v <- st.v + st.y
    | Dvi.C_z0 -> st.v <- st.v + st.z
    | Dvi.C_z k -> st.z <- k ; st.v <- st.v + st.z
    | Dvi.C_fnt n -> fnt st n
    | Dvi.C_xxx s -> special st s
    | _ -> ()

  let scan_command st = function
    | Dvi.C_xxx s -> special st s
    | _ -> ()

  let eval_command st c =
     begin match !current_recording_proc_unit with
     | None -> ()
     | Some u ->	
         match c with
	    (* The advi: proc specials are not recorded *)  
         | Dvi.C_xxx s when has_prefix "advi: proc=" s -> ()
         |  _ -> u.escaped_commands <- u.escaped_commands @ [c]
     end;
     eval_command st c

  let _ = eval_command_ref := eval_command

  let newpage h st magdpi x y =
    try Dev.newpage h st.sdpi magdpi x y
    with Dev.GS -> st.status.Dvi.hasps <- false

  let find_prologues l =
    let l' = Search.true_file_names [] l in
    if List.length l <> List.length l' then begin
      Misc.warning "Cannot find postscript prologue. Continuing without Postscript specials";
      dops := false;
      []
    end else l'

(* function to be removed in the future, or replaced by the proper iteration of render_step *)
  let render_page cdvi num dpi xorig yorig =
    failwith "Render_page is deprecated.";;


  let render_step cdvi num dpi xorig yorig =
    proc_clean ();
    if num < 0 || num >= Array.length cdvi.base_dvi.Dvi.pages then
      invalid_arg "Driver.render_step" ;
    let mag = float cdvi.base_dvi.Dvi.preamble.Dvi.pre_mag /. 1000.0
    and page = cdvi.base_dvi.Dvi.pages.(num) in
    let status =
      let headers = ref [] 
      and xrefs = cdvi.base_dvi.Dvi.xrefs in
      let s = scan_special_page cdvi (headers,xrefs) num
      in if !headers <> [] then
        Dev.add_headers (find_prologues (List.rev !headers));
        s
    in
    if not !Misc.dops then status.Dvi.hasps <-false;
    let st =
      { cdvi = cdvi ;
	sdpi = int_of_float (mag *. ldexp dpi 16) ;
	conv = mag *. dpi /. cdvi.dvi_res /. 65536.0 ;
	x_origin = xorig ; y_origin = yorig ;
	cur_mtable = dummy_mtable ;
	cur_gtable = dummy_gtable ;
	cur_font = dummy_font ;
	h = 0 ; v = 0 ; w = 0 ; x = 0 ; y = 0 ; z = 0 ;
	stack = [] ; color = 0x000000 ; color_stack = [];
	alpha = 1.0; alpha_stack = [];
	blend = Dev.Normal; blend_stack = [];
	epstransparent = false; epstransparent_stack = [];
	transition= Transitions.TransNone; transition_stack = [];
        tpic_pensize = 0.0; tpic_path = []; tpic_shading = 0.0;
        status = status;
        headers = [];
        html = None;
        draw_html = [];
        checkpoint = 0;
      } in
    setup_bkgd st.status; (* RDC apply the background preferences in Dev *)
    Dev.clear_dev();      (* and redraw the background                   *)
    Dev.set_color st.color ;
    Dev.set_transition st.transition ;
    if st.status.Dvi.hasps then 
      newpage [] st  (mag *. dpi) xorig yorig;
    st.checkpoint <- 0;
    let check() =
      begin try Dev.continue() with
      | Dev.Stop as exn -> raise exn
      end; 
      st.checkpoint <- checkpoint_frequency in
    let eval st x =
      st.checkpoint <- st.checkpoint -1;
      let b = eval_command st x in
      if st.checkpoint < 0 then check();
      b in
    Dvi.page_step (eval st) page

  let unfreeze_font cdvi n =
    try
      let cfont = Table.get cdvi.font_table n in
      ignore (Table.get cfont.mtable (Char.code 'A'))
    with _ -> ()

  let unfreeze_fonts cdvi =
    List.iter (fun (n, _) -> unfreeze_font cdvi n)
      cdvi.base_dvi.Dvi.font_map

    let scan_special_pages cdvi lastpage =
      let headers = ref [] 
      and xrefs = cdvi.base_dvi.Dvi.xrefs in
      for n = 0 to min lastpage (Array.length cdvi.base_dvi.Dvi.pages) - 1 do
        ignore (scan_special_page cdvi (headers,xrefs) n);
      done ;
      if !headers <> [] then
        Dev.add_headers (find_prologues (List.rev !headers))

  let unfreeze_glyphs cdvi dpi =
    let mag = float cdvi.base_dvi.Dvi.preamble.Dvi.pre_mag /. 1000.0 in
    let sdpi = int_of_float (mag *. ldexp dpi 16)
    and mtable = ref dummy_mtable
    and gtable = ref dummy_gtable in
    let headers = ref []
    and xrefs = cdvi.base_dvi.Dvi.xrefs in
    let eval page = function
      |	Dvi.C_fnt n ->
	  let (mt, gt) =
	    try
	      let cfont = Table.get cdvi.font_table n in
	      (cfont.mtable, get_gtable cfont sdpi)
	    with Not_found -> (dummy_mtable, dummy_gtable) in
	  mtable := mt ;
	  gtable := gt
      |	Dvi.C_set code ->
	  begin try ignore (Table.get !mtable code) with _ -> () end ;
	  begin try ignore (Table.get !gtable code) with _ -> () end
      | Dvi.C_xxx s -> 
          prerr_endline "Scan_special is not yet properly implemented inside unfreeze_glyphs: should merge with scan_special_pages";
          (* scan_special cdvi page headers s *)
          ()
      |	_ -> () in
    for n = 0 to Array.length cdvi.base_dvi.Dvi.pages - 1 do
      mtable := dummy_mtable ;
      gtable := dummy_gtable ;
      Dvi.page_iter (eval n) cdvi.base_dvi.Dvi.pages.(n);
    done ;
    if !headers <> [] then
      Dev.add_headers (Search.true_file_names [] (List.rev !headers))


