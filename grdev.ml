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

module G = GraphicsY11

let ignore_background = Misc.option_flag false
    "--ignore_background"
    "\tIgnore background for antialiasing";;

let show_busy = Misc.option_flag true
    "-nowatch"
    "\tDon't display a watch when busy";;



type color = int ;;
let href_frame = 0x00ff00
let advi_frame = 0xaaaaff
let href_emphasize = 0xffff00
let name_emphasize = 0xffaaaa

(*** Device configuration ***)

let opened = ref false ;;
let display_mode = Misc.option_flag false "-bg" "Draw in the background"
let set_mode b =  display_mode := b;;


let size_x = ref 0 ;;
let size_y = ref 0 ;;
let color = ref 0x000000 ;;

let xmin = ref 0 ;;
let xmax = ref 0 ;;
let ymin = ref 0 ;;
let ymax = ref 0 ;;

(* Communication with GS *)
exception Stop

let dvi = true;;
let ps = false;;
let psused = ref false;;
let last_is_dvi = ref true

let flush_ps() = if not !psused then psused := true;  Gs.flush();;
let flush_dvi() = G.flush();;
let flush_last() = if !last_is_dvi then flush_dvi() else flush_ps();;

let sync b =
  if !last_is_dvi = b then ()
  else begin flush_last(); last_is_dvi := b end;;

let synchronize () =
  Gs.flush();
  Graphics.synchronize();;

let control_cursor = G.Cursor_left_ptr
let move_cursor = G.Cursor_fleur
let select_cursor = G.Cursor_xterm

type mode = Control | Selection
let free_cursor = ref control_cursor
let mode = ref Control
let set_selection_mode m =
  mode := m; 
  free_cursor :=
    begin match m with
    | Control ->  control_cursor
    | Selection -> select_cursor
    end

type busy = Free | Busy | Pause | Disk

let set_busy sw = 
  if !show_busy then
    match sw with
    | Pause -> G.set_cursor G.Cursor_right_side
    | Disk -> G.set_cursor G.Cursor_exchange
    | Busy -> G.set_cursor G.Cursor_watch
    | Free -> G.set_cursor !free_cursor
  else ();;

let set_title s = Graphics.set_window_title s ;;

(* for refreshed signal on usr1 *)
exception Usr1
let usr1_status = ref false;;
let clear_usr1() = usr1_status :=  false;;
let waiting = ref false;;
let test_usr1() =
  if !usr1_status then
    begin
      clear_usr1();
      true
    end
  else false;;

let usr1 = 10;;
let set_usr1() =
  Sys.set_signal usr1  
    (Sys.Signal_handle
       (fun _ -> usr1_status := true; if !waiting then raise Usr1));;
set_usr1();;

(*** Private glyphs ***)

type cache =
  | No_cache
  | Cached of (color * color) * Graphics.image ;;

type glyph = {
    width : int ;
    height : int ;
    hoffset : int ;
    voffset : int ;
    graymap : string ;
    glyph : Glyph.t ;
    mutable cache : cache ;
    mutable img_list : ((color * color) * Graphics.image) list
  } ;;

let default_bg_color = Graphics.white;;
let bg_color = ref  default_bg_color;;
let bg_colors = ref [];;
let push_bg_color c =
  bg_colors := !bg_color ::!bg_colors;
  bg_color := c;;
let pop_bg_color() = 
  match !bg_colors with
  | h::t -> bg_color := h; bg_colors := t
  | [] -> bg_color := default_bg_color
;;

let background_colors = ref [];;
let add_background_color x y w h c =
  background_colors := (x, y, w, h, c) :: !background_colors;;

let find_bg_color x y w h=
  let rec find_color = function
    (x0, y0, w0, h0, c) :: t ->
      if x0 <= x && y0 <= y && x + w <= x0 + w0 && y + h <= y0 + h0 then c
      else find_color t
    | [] -> !bg_color in
  find_color !background_colors;;


let get_bg_color x y w h =
  if !ignore_background then Graphics.white
  else
    begin
      sync dvi;
      if !psused then
        let c = Graphics.point_color (x+1) (y+1) in
        if Graphics.point_color (x+w-1) (y+h-1) = c then c
        else Graphics.white
      else
        find_bg_color x y w h
      
    end;;

let get_color_table =
  let htable = Hashtbl.create 257 in
  function (bg, fg as col) ->
    try Hashtbl.find htable col
    with Not_found ->
      let table = Array.make 256 Graphics.transp in
      let r0 = (bg lsr 16) land 0xff
      and g0 = (bg lsr 8) land 0xff
      and b0 = bg land 0xff in
      let r1 = (fg lsr 16) land 0xff
      and g1 = (fg lsr 8) land 0xff
      and b1 = fg land 0xff in
      for i = 1 to 255 do
	let k = (255 - i) in
	let r = (k*r0 + i*r1)/255
	and g = (k*g0 + i*g1)/255
	and b = (k*b0 + i*b1)/255 in
	table.(i) <- (r lsl 16) + (g lsl 8) + b
      done ;
      Hashtbl.add htable col table ;
      table ;;

let get_image g col =
  match g.cache with
  | Cached(c, img) when c = col -> img
  | _ ->
      let img =
	try List.assoc col g.img_list
	with Not_found ->
	  let gmap = g.graymap
	  and w = g.width
	  and h = g.height in
         (* We enforce [h <> 0] and [w <> 0] because
	    Caml graphics don't like zero-sized pixmaps. *)
	  let dst = Array.make_matrix (max 1 h) (max 1 w) Graphics.transp
	  and table = get_color_table col
	  and p = ref 0 in
	  for i = 0 to h - 1 do
	    for j = 0 to w - 1 do
	      dst.(i).(j) <- table.(Char.code gmap.[!p]) ;
	      incr p
	    done
	  done ;
	  let img = Graphics.make_image dst in
	  g.img_list <- (col, img) :: g.img_list ;
	  img in
      g.cache <- Cached(col, img) ;
      img ;;

let make_glyph g =
  { width = g.Glyph.width ;
    height = g.Glyph.height ;
    hoffset = g.Glyph.hoffset ;
    voffset = g.Glyph.voffset ;
    graymap = g.Glyph.graymap ;
    glyph = g ;
    cache = No_cache ;
    img_list = [] } ;;

let get_glyph g = g.glyph

(*** Device manipulation ***)

let set_bbox bbox =
  if not !opened then
    failwith "Grdev.set_bbox: no window" ;
  match bbox with
  | None ->
      xmin := 0 ; xmax := !size_x ;
      ymin := 0 ; ymax := !size_y
  | Some(x0, y0, w, h) ->
      xmin := x0 ; xmax := x0 + w ;
      ymin := !size_y - (y0 + h) ; ymax := !size_y - y0
 ;;

(*** Drawing ***)

let set_color col =
  if not !opened then
    failwith "Grdev.set_color: no window" ;
  color := col ;
  Graphics.set_color col ;;
  
let draw_glyph g x0 y0 =
  if not !opened then
    failwith "Grdev.draw_glyph: no window" ;
  let w = g.width
  and h = g.height in
  let x = x0 - g.hoffset
  and y = !size_y - y0 + g.voffset - h in
  if x + w > !xmin && x < !xmax && y + h > !ymin && y < !ymax
  then begin
    let bg = get_bg_color x y w h in
    let img = get_image g (bg, !color) in
    Graphics.draw_image img x y ;
  end ;; 

let fill_rect x0 y0 w h =
  if not !opened then
    failwith "Grdev.fill_rect: no window" ;
  let x = x0
  and y = !size_y - y0 - h in
  let x' = x + w
  and y' = y + h in
  (* clipping *)
  let x = max !xmin x
  and y = max !ymin y
  and x' = min x' !xmax
  and y' = min y' !ymax in
  let w = x' - x
  and h = y' - y in
  if w > 0 && h > 0 then
    begin 
      Graphics.fill_rect x y w h; 
      add_background_color x y w h !color;
    end;;


(* TODO: implement clipping in the following primitives? *)

let adjust_path path =
  let newpath = Array.copy path in
  for i = 0 to Array.length newpath - 1 do
    let (x,y) = path.(i) in
    newpath.(i) <- (x, !size_y - y)
  done;
  newpath

let draw_path path ~pensize =
  if not !opened then
    failwith "Grdev.draw_path: no window" ;
  let path = adjust_path path in
  Graphics.set_line_width pensize;
  Graphics.draw_poly_line path;
  Graphics.set_line_width 1

let set_shade shade =
  let r = 0xFF - (!color lsr 16) land 0xFF
  and g = 0xFF - (!color lsr 8) land 0xFF
  and b = 0xFF - !color land 0xFF in
  let r = 0xFF - int_of_float (shade *. float r)
  and g = 0xFF - int_of_float (shade *. float g)
  and b = 0xFF - int_of_float (shade *. float b) in
  Graphics.set_color (Graphics.rgb r g b)

let fill_path path ~shade =
  if not !opened then
    failwith "Grdev.fill_path: no window" ;
  let path = adjust_path path in
  set_shade shade;
  Graphics.fill_poly path;
  Graphics.set_color !color

let draw_arc ~x ~y ~rx ~ry ~start:s ~stop:e ~pensize =
  if not !opened then
    failwith "Grdev.draw_arc: no window" ;
  Graphics.set_line_width pensize;
  Graphics.draw_arc x (!size_y - y) rx ry (- s) (- e);
  Graphics.set_line_width 1

let fill_arc ~x ~y ~rx ~ry ~start:s ~stop:e ~shade =
  if not !opened then
    failwith "Grdev.fill_arc: no window" ;
  set_shade shade;
  Graphics.fill_arc x (!size_y - y) rx ry (- s) (- e);
  Graphics.set_color !color

let epstransparent = ref false;;
let set_epstransparent s = epstransparent := s;;

let alpha = ref 1.0;;
let set_alpha a = alpha := a;;

type blend =
  | Normal | Multiply | Screen | Overlay (* | SoftLight | HardLight *)
  | ColorDodge | ColorBurn | Darken | Lighten | Difference 
  | Exclusion (* | Luminosity | Color | Saturation | Hue *)
;;

let blend = ref Normal;;
let set_blend b = blend := b;;

(* look at gxblend.c of ghostscript *)
let blend_func = function
  | Normal -> fun dst src -> src
  | Multiply -> 
      fun dst src -> 
	let t = dst * src + 0x80 in
      	let t = t + t lsr 8 in
      	t lsr 8
  | Screen ->
      fun dst src -> 
      	let t = (0xff - dst) * (0xff - src) + 0x80 in
      	let t = t + t lsr 8 in
      	0xff - t lsr 8
  | Overlay ->
      fun dst src -> 
      	let t = 
	  if dst < 0x80 then 2 * dst * src
	  else 0xf301 - 2 * (0xff - dst) * (0xff - src) in
      	let t = t + 0x80 in
      	let t = t + t lsr 8 in
      	t lsr 8
(*
   | SoftLight ->
   if s < 0x80 then begin
   let t = (0xff - (src lsl 1)) * art_blend_sq_diff_8[dst] in
   let t = t + 0x8000 in
   dst - t lsr 16
   end else begin
   let t = ((src lsl 1) - 0xff) * art_blend_soft_light_8[dst] in
   let t = t + 0x80 in
   let t = t + t lsr 8 in
   dst + t lsr 8
   end
*)
  | ColorDodge ->
      fun dst src ->
       	if dst = 0 then 0 else if dst >= src then 0xff 
      	else (0x1fe * dst + src) / (src lsl 1)
  | ColorBurn ->
      fun dst src -> 
	let dst = 0xff - dst in
      	if dst = 0 then 0xff else if dst >= src then 0
      	else 0xff - (0x1fe * dst + src) / (src lsl 1)
  | Darken ->
      fun dst src -> if dst < src then dst else src
  | Lighten ->
      fun dst src -> if dst > src then dst else src
  | Difference ->
      fun dst src -> 
	let t = dst - src in
      	if t < 0 then -t else t
  | Exclusion ->
      fun dst src -> 
	let t = (0xff - dst) * src + dst * (0xff - src) in
      	let t = t + 0x80 in
      	let t = t + t lsr 8 in
      	t lsr 8
          
let draw_ps file bbox (w,h) x0 y0 = 
  if not !opened then
    failwith "Grdev.fill_rect: no window" ;
  let x = x0 
  and y = !size_y - y0 + h in
  try Drawps.f file !epstransparent !alpha 
      (if !blend = Normal then None else Some (blend_func !blend))
      bbox (w,h) x y 
  with 
  | Not_found -> Misc.warning ("ps file " ^ file ^ " not found")
  | _ -> Misc.warning ("error happend in drawing ps file " ^ file)
          
let clean_ps_cache () = Drawps.clean_cache ()
    

let sleep_watch b n =
  let start = Unix.gettimeofday() in
  let rec delay t =
    try
      if b && (!usr1_status || Graphics.key_pressed()) then ()
      else ignore (Unix.select [] [] [] t)
    with Unix.Unix_error(Unix.EINTR, _, _) ->
      let now = Unix.gettimeofday() in
      let remaining = start +. n -. now in
      if remaining > 0.0 then delay remaining in
  delay n;;

let sleep = sleep_watch true;;

(* Embedded (tcl/tk) applications *)

type app_type = Sticky | Persistent | Embedded
let app_table = Hashtbl.create 17

let embed_app command app_type width height x y =
  let string_replace pat templ str =
    let result = Buffer.create (String.length str * 2) in
    let patlen = String.length pat in
    let find pat str at =
      let rec find_aux pos =
	if String.sub str pos patlen = pat then pos
	else find_aux (pos + 1)
      in
      try
	find_aux at
      with
	_ -> raise Not_found
    in
      let rec replace pos =
	try
	  let fpos = find pat str pos in
	  Buffer.add_string result (String.sub str pos (fpos - pos));
	  Buffer.add_string result templ;
	  replace (fpos + patlen)
	with
	| Not_found ->
	    Buffer.add_string result
	      (String.sub str pos (String.length str - pos));
	    Buffer.contents result 
      in
      replace 0
  in

  let wid = GraphicsX11.open_subwindow ~x ~y:(y - height) ~width ~height in

  (*** !x commands
    !g : geometry like 100x100+20+30
    !p : embedding target window id (in digit)
    !w : width  of the target window in pixel
    !h : height of the target window in pixel
    !x : x of the application against the root
    !y : y of the application against the root
    Why "!"?  '\' is for TeX. "%" is for TeX. "$" is for TeX...
  ***)

  let opt_parent = 
    Printf.sprintf "%s" wid
  in
  let command0 = string_replace "!p" opt_parent command in
  
  (* if there is no !p, the application geometry will be treated 
     by the WM. In such cases, we try to fix the geoemtry
     so that it is against the root. *)

  let opt_geometry, opt_x, opt_y = 
    let px,py = 
      let against_root = (command0 = command) in
      if against_root then begin
        (* fix the geometry *)
	let (ww,wh,wx,wy) = G.get_geometry () in
	x+wx, y - height + wy
      end else begin
	0,0
      end
    in
    Printf.sprintf "%dx%d+%d+%d" width height px py,
    string_of_int px,
    string_of_int py
  in

  let command =
    string_replace "!g" opt_geometry  
	(string_replace "!w" (string_of_int width)
	    (string_replace "!h" (string_of_int height) 
	       (string_replace "!x" opt_x
		  (string_replace "!y" opt_y
		     command0))))
  in
  
  (* prerr_endline command; *)

  let command_tokens = Misc.parse_shell_command command in
  let pid = Unix.fork () in
  if pid = 0 then begin (* child *)
    try
      Unix.execvp command_tokens.(0) command_tokens
    with
    | _ -> exit 127
  end else begin
    if Hashtbl.mem app_table pid then 
      raise (Failure (Printf.sprintf 
			"pid %d is already in the app_table!" pid));
    Hashtbl.add app_table pid (app_type, wid)
  end

let kill_app pid wid =
  (try Unix.kill pid 9 with _ -> ());
  GraphicsX11.close_subwindow wid
;;

let kill_apps app_type = 
  let removed = ref [] in
  Hashtbl.iter (fun pid (apt, wid) -> 
    if app_type = apt then begin
      removed := pid :: !removed;
      kill_app pid wid
    end) app_table;
  List.iter (Hashtbl.remove app_table) !removed;
  while
    try
      let pid, _ = Unix.waitpid [Unix.WNOHANG] 0 in
      pid <> 0
    with
      Unix.Unix_error(Unix.ECHILD,_,_) -> false
  do () done
;;  

let kill_embedded_apps () = 
  kill_apps Embedded 
;;

let kill_persistent_apps () = 
  kill_apps Sticky; kill_apps Persistent
;;

(*** HTML interaction ***)

(* Should be improve later using quad-tree or similar 2d structure *)
module type ACTIVE = 
  sig 
    type 'a active =
        { x : int;
          y : int;
          w : int;
          h : int;
          action : 'a
        } 

    type 'a t
    val empty : 'a t
    val add : 'a active -> 'a t -> 'a t
    val same_location : 'a active -> 'b active -> bool
    val find : int -> int -> 'a t -> 'a active
    val find_action : ('a -> bool) -> 'a t -> 'a active
    val inside : int -> int -> 'a active -> bool
    val iter : ('a active -> unit) -> 'a t -> unit
  end;;
    
module A : ACTIVE =
  struct
    type 'a active =
        { x : int;
          y : int;
          w : int;
          h : int;
          action : 'a
        } 
    type 'a t = 'a active list
    let empty = []
    let add a t = a :: t
    let inside x y a  = 
      a.x <= x && a.y <= y && x <= a.x + a.w && y <= a.y + a.h
    let find x y t = List.find (inside x y) t
    let find_action f l = List.find (fun x -> f x.action) l
    let iter = List.iter
    let same_location a b = 
(*
      a.x = b.x && a.y = b.y && a.w = b.w && a.h = b.h
*)
      a.x <= b.x && a.y <= b.y &&
      a.w >= b.x - a.x + b.w && a.h >= b.y - a.y + b.h

  end
    
module H =
  struct
    type tag =
      | Name of string 
      | Href of string
      | Advi of string * (unit -> unit)
            
    type anchor = {
        tag : tag;
        draw : (int * int * glyph) list
      } 
          
    let anchors = ref A.empty
    let clear() = anchors := A.empty
        
    let frame_rect e x y w h =
      if e > 0 && w > e && h > e then
        begin
          Graphics.fill_rect x y e h;      
          Graphics.fill_rect x y w e;
          Graphics.fill_rect (x + w - e) y e h;
          Graphics.fill_rect x (y + h - e) w e;
        end
      else Graphics.draw_rect x y w h
          
    let draw_anchor c e a =
      Graphics.set_color c;
      frame_rect e a.A.x a.A.y a.A.w a.A.h;
      Graphics.set_color !color
        
    let rec make_anchors tag all_draw =
      let make_anchor  draw (x, y as orig) width height voffset = 
        let anchor = {tag = tag; draw = List.rev draw } in
        let e =
          match tag with
          | Href s -> 0
          | Advi (s,f) -> 0
          | Name s -> 0
        in
        let y' = y - voffset -1 in
        let height' = height + 2 in
        let a = 
          { A.x = x - e;
            A.y =  (!size_y - y' - height') - e ;
            A.w = width + e + e;
            A.h = height' + e + e;
            A.action = anchor;
          } in
        anchors := A.add a !anchors; 
        match tag with
        | Href _ -> draw_anchor href_frame 1 a
        | Advi _ -> draw_anchor advi_frame 1 a
        | _ -> ()
      in
      let rec split draw (x, y as orig) width height voffset = function
        | [] ->
            make_anchor  draw orig width height voffset 
              
        | (x1, y1, g1 as d):: rest ->
            if x1 + g1.width > x then 
              split (d::draw) orig
                (max width ((x1-x)+g1.width))
                (max height g1.height)
                (max voffset g1.voffset) rest
            else
              begin
                make_anchor  (d::draw) orig width height voffset;
                start rest
              end
      and start = function
          [] -> ()
        | (x, y, g as d)::rest ->
            split [d] (x,y) g.width g.height g.voffset rest
      in
      start all_draw
        
    let add anchor =
      make_anchors anchor.tag anchor.draw 
        
    let find x y = A.find x y !anchors
        
    let find_tag t =
      A.find_action (fun x -> x.tag = t) !anchors
        
        
    type backup =
      | Nil
      | Rect of Graphics.image * anchor A.active *
            (Graphics.image * anchor A.active) list
      | Screen of Graphics.image * anchor A.active * anchor A.t 
            
    let up_to_date act = function
      | Rect (_, a, l) -> A.same_location a  act
      | Screen (_, a, _) -> A.same_location a act
      | Nil -> false
            
            
    let deemphasize now emph =
      if emph = Nil then ()
      else begin
	Graphics.display_mode now;
	begin match emph with
	| Rect (ima, act, l) ->
            List.iter
              (function ima, act -> Graphics.draw_image ima act.A.x act.A.y) l;
            Graphics.draw_image ima act.A.x act.A.y;
	| Screen (ima, act, all_anchors) ->
            anchors := all_anchors;
            Gs.flush();
            Graphics.draw_image ima 0 0;
	| Nil -> raise (Failure "this must not be called") 
	end;
	G.set_cursor !free_cursor;
	Graphics.display_mode false
      end
        
    let emphasize c act =
      let ima = Graphics.get_image act.A.x act.A.y act.A.w act.A.h in
      Graphics.set_color c;
      Graphics.display_mode true;
      Graphics.fill_rect act.A.x act.A.y act.A.w act.A.h;
      Graphics.set_color !color;
      push_bg_color c;
      List.iter (function x, y, g -> draw_glyph g x y) act.A.action.draw;
      pop_bg_color();
      G.set_cursor G.Cursor_hand2;
      Graphics.display_mode false;
      Rect (ima, act, []) 
        
    let save_screen_exec act a =
      Gs.flush();
      let ima = Graphics.get_image 0 0 !size_x !size_y in
      G.flush();
      (* it seems that the image is saved ``lazily'' and further instruction
         could be capture in the image *) 
      sleep_watch false 0.05;
      let all_anchors = !anchors in
      a();
      synchronize();
      Screen (ima, act, all_anchors)
        
    let light t =
      try
        match t with Name n ->
          let t = Name (Misc.get_suffix "#" n) in
          emphasize name_emphasize (find_tag t)
        | _ -> Nil
      with
        Not_found | Misc.Match -> Nil

    let flashlight t =
      deemphasize false (light t)
            
    let emphasize_and_flash color act =
      let emph = emphasize color act in
      let m = 
        begin
          match act.A.action.tag with
          | Href n -> light (Name n)
          | _ -> Nil
        end in
      match emph, m with
        Rect (ima, a, l), Rect (ima', a', l') ->
          Rect (ima, a, (ima', a') :: (l' @ l))
      | x, _ -> x
            
    let reemphasize emph act =
      deemphasize true emph;
      emphasize_and_flash href_emphasize act
  end;;
        
(*** Clearing device ***)


let open_dev geom =
  if !opened then
    Graphics.close_graph () ;
  Graphics.open_graph geom ;
  size_x := Graphics.size_x () ;
  size_y := Graphics.size_y () ;
(*  
  xmin := 0 ; xmax := !size_x ;
  ymin := 0 ; ymax := !size_y ;
*)
  Graphics.remember_mode true ;
  Graphics.display_mode !display_mode ;
  opened := true ;;

let close_dev () =
  if !opened then begin
    kill_embedded_apps ();
    kill_persistent_apps ();
    Graphics.close_graph ();
  end;
  opened := false ;;

let clear_dev () =
  if not !opened then
    failwith "Grdev.clear_dev: no window" ;
  kill_embedded_apps ();
  Graphics.display_mode !display_mode ;
  Graphics.clear_graph ();
  H.clear(); 
  bg_color := default_bg_color;
  bg_colors := [];
  background_colors := [];
  size_x := Graphics.size_x () ;
  size_y := Graphics.size_y () ;
(*
  xmin := 0 ; xmax := !size_x ;
  ymin := 0 ; ymax := !size_y ;
*)
;;

(*** Events ***)

type status = Graphics.status = {
    mouse_x : int ;
    mouse_y : int ;
    button : bool ;
    keypressed : bool ;
    key : char
  } ;;


type area = Bottom_right | Bottom_left | Top_right | Top_left | Middle
type button = Button1 | Button2 | Button3
type event =
    Resized of int * int
  | Refreshed
  | Key of char
  | Move of int * int
  | Region of int * int * int * int
  | Href of string
  | Advi of string * (unit -> unit)
  | Click of area * button
  | Nil
;;

type option_event = Final of event | Raw of status
        
let all_events = [
  Graphics.Button_down ;
  Graphics.Button_up ;
  Graphics.Mouse_motion; 
  Graphics.Key_pressed ;
] ;;

let button_up_motion = [
  Graphics.Button_up ;
  Graphics.Mouse_motion; 
] ;;

let button_up = [
  Graphics.Button_up ;
] ;;

let event = ref [];;
let push_back_event ev =
  if List.length !event > 1 then Misc.warning "STACK";
  event :=  ev :: !event;;
let event_waiting() =
  match !event with [] -> false | _ -> true;;
let rec pop_event () =
  match !event with
  | [] -> assert false
  | h::t -> event := t; h;;

let mouse_x = ref 0;;
let mouse_y = ref 0;;
let button = ref false;;

let resized() = 
  let x = Graphics.size_x() and y = Graphics.size_y() in
  let b = x <> !size_x || y <> !size_y in
  if b then
    begin
      size_x := x;
      size_y := y;
      Gs.kill();
      Some (x,y)
    end
  else None;;

(* pour sauver une image_rectiligne *)
type rectangular_image =
    { north : Graphics.image;
      south : Graphics.image;
      east : Graphics.image;
      west : Graphics.image; };;
let rec save_rectangle x y dx dy =
  let x = min x (x+dx) in
  let y = min y (y+dy) in
  let dx = max 1 (abs dx) in
  let dy = max 1 (abs dy) in
  {
  south = Graphics.get_image x y dx 1;
  west = Graphics.get_image x y 1 dy;
  east = Graphics.get_image (x+dx) y 1 dy;
  north =  Graphics.get_image x (y+dy) (succ dx) 1;
} ;;

let rec restore_rectangle r x y dx dy =
  let x = min x (x+dx) in
  let y = min y (y+dy) in
  let dx = max 1 (abs dx) in
  let dy = max 1 (abs dy) in
  begin
    Graphics.draw_image r.south x y;
    Graphics.draw_image r.west x y;
    Graphics.draw_image r.east (x+dx) y;
    Graphics.draw_image r.north x (y+dy);
  end
    ;;

let rec draw_rectangle x y dx dy =
  Graphics.moveto x y;
  Graphics.lineto (x+dx) y;
  Graphics.lineto (x+dx) (y+dy);
  Graphics.lineto (x) (y+dy);
  Graphics.lineto (x) (y);;

let rec wait_signal_event events =
    match resized(), !usr1_status, event_waiting() with
    | Some (x,y), _,_ -> Final (Resized (x,y))
    | _, true, _ -> clear_usr1(); Final (Refreshed)
    | _, _, true -> Raw (pop_event())
    | _, _, _ -> 
        try 
          waiting := true;
          let ev = Graphics.wait_next_event events in
          waiting := false;
          match resized () with
          | Some (x,y) ->
              push_back_event ev;
              Final (Resized (x,y))
          | None ->
              Raw ev
        with
        | Usr1 ->
            waiting := false;
            Final Refreshed
        | exn -> 
            waiting := false;
            raise exn
          ;;

let wait_select_button_up x y =
  let rec select dx dy =
    let buf = save_rectangle x y dx dy in
    draw_rectangle x y dx dy;
    let ev = wait_signal_event button_up_motion in
    restore_rectangle buf x y dx dy;
    match ev with
    | Raw e ->
        let dx' = e.Graphics.mouse_x - x in 
        let dy' = e.Graphics.mouse_y - y in 
        if e.Graphics.button then select dx' dy'
        else Final (Region (x, y, dx', 0 - dy'))
    | x -> x
  in
  set_color Graphics.black;
  Graphics.display_mode true;
  G.set_cursor select_cursor;
  let restore() =
    Graphics.display_mode false;
    set_color !color;
    G.set_cursor !free_cursor
  in
  try let e = select 0 0 in restore(); e
  with exn -> restore(); raise exn
        ;;  

let wait_move_button_up x y =
  let w = !xmax - !xmin and h = !ymax - !ymin in
  let rec move dx dy =
    let x' = !xmin + dx and y' = !ymin + dy in
    let buf = save_rectangle x' y' w h in
    draw_rectangle x' y' w h;
    let ev = wait_signal_event button_up_motion in
    restore_rectangle buf x' y' w h;
    match ev with
    | Raw e ->
        let dx' = e.Graphics.mouse_x - x in 
        let dy' = e.Graphics.mouse_y - y in 
        if e.Graphics.button then move dx' dy'
        else Final (Move (dx', 0 - dy'))
    | x -> x
  in
  set_color Graphics.black;
  G.set_cursor move_cursor;
  Graphics.display_mode true;
  let restore() =
    Graphics.display_mode false;
    set_color !color;
    G.set_cursor !free_cursor
  in
  try let e = move 0 0 in restore(); e
  with exn -> restore(); raise exn


let near x x' = abs (x - x') < !size_x / 4;;
let click_area x y =
  if near x 0 then
    if near y 0 then Bottom_left
    else if near y !size_y then Top_left
    else Middle
  else if near x !size_x then
    if near y 0 then Bottom_right
    else if near y !size_y then Top_right
    else Middle
  else Middle

let button m =
  if m land G.button1 <> 0 then Button1
  else if m land G.button2 <> 0 then Button2
  else if m land G.button3 <> 0 then Button3
  else Button2

let test() = 
  for i = 1 to 76
  do
    print_int (i+i); print_newline();
    G.set_cursor (G.Cursor_id (i + i));
    ignore (wait_signal_event [ Graphics.Button_up ]);
    ignore (wait_signal_event [ Graphics.Button_down ]);
  done

let wait_button_up m x y =
  if m land G.control <> 0 then wait_move_button_up x y
  else if m land G.shift <> 0 then wait_select_button_up x y
  else
    begin
      match wait_signal_event button_up with
        Raw e ->
          Final (Click (click_area x y, button m))
      | x -> x 
    end

let wait_event () =
  let rec event emph =
    
    let send ev =
      H.deemphasize true emph; ev in
    let rescan() =
      H.deemphasize true emph; event H.Nil in
    match wait_signal_event all_events with
    | Raw ev ->
        begin
          if ev.Graphics.keypressed then
            send (Key ev.Graphics.key)
          else
            let ev' = 
              { mouse_x = ev.Graphics.mouse_x ;
                mouse_y = !size_y - ev.Graphics.mouse_y ;
                button = ev.Graphics.button ;
                keypressed = ev.Graphics.keypressed ;
                key = ev.Graphics.key } in
            begin try
              begin match H.find ev.mouse_x ev.mouse_y with
              | {A.action = {H.tag = H.Href h; H.draw = d }} as act ->
                  if ev.button then send (Href h)
                  else if H.up_to_date act emph then
                    event emph
                  else
                    begin
                      H.deemphasize true emph;
                      event (H.emphasize_and_flash href_emphasize act)
                    end
              | {A.action = {H.tag = H.Advi (s,a); H.draw = d }} as act ->
                  if H.up_to_date act emph then event emph
                  else
                    begin
                      H.deemphasize true emph;
                      event (H.save_screen_exec act a)
                    end

              | _ ->
                  rescan()
              end with Not_found ->
                if ev'.button then
                  let m = G.get_modifiers() in
                  match wait_button_up m ev.mouse_x ev.mouse_y with
                  | Final (Region (x, y, dx, dy) as e) -> send e
                  | Final (Move (dx, dy) as e) -> send e
                  | Final (Click (a,b) as e) -> send e
                  | Final e ->
                      push_back_event ev; 
                      send e
                  | Raw _ -> rescan()
                else
                  rescan()
            end
        end
    | Final e -> send e in
  event H.Nil
    ;;

(* To be changed *)
exception GS = Gs.Terminated

let resized() =
  Graphics.size_x() <> !size_x || Graphics.size_y() <> !size_y

let continue () =
  if resized() || !usr1_status || Graphics.key_pressed() then
    begin
      Gs.flush();
      raise Stop
    end
;;


(* Calling GS *)

let current_pos () =
  if not !last_is_dvi then flush_ps();
  let x = !Gs.current_x and y = !Gs.current_y in
  x, y
;;

let newpage x y z t w =
  Gs.newpage x y z t w;
  Gs.flush();
  last_is_dvi := false; 
  psused := false;;

let add_headers l =
  Gs.add_headers l;;

let exec_ps s x0 y0 =
  sync ps;
  if not !opened then failwith "Grdev.exec_ps: no window" ;
  Gs.draw s x0 y0;
 ;; 

