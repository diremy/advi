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

module GY = GraphicsY11;;

let ignore_background = Misc.option_flag false
    "--ignore_background"
    "\tIgnore background for antialiasing";;

let show_busy = Misc.option_flag true
    "-nowatch"
    "\tDon't display a watch when busy";;

let busy_delay = ref 0.5;;

Misc.set_option
  "-watch"
  (Arg.Float (fun x -> busy_delay := x))
  "FLOAT\tDelay before the watch cursor appears (default 0.5s)";;

type color = int;;
let href_frame = 0x00ff00;;
let advi_frame = 0xaaaaff;;
let href_emphasize = 0xffff00;;
let name_emphasize = 0xffaaaa;;
let cut_emphasize = Graphics.cyan;;

(*** Device configuration ***)

let opened = ref false;;

let size_x = ref 0;;
let size_y = ref 0;;

let xmin = ref 0;;
let xmax = ref 0;;
let ymin = ref 0;;
let ymax = ref 0;;

(* Communication with GS *)
exception Stop;;

let dvi = true;;
let ps = false;;
let psused = ref false;;
let last_is_dvi = ref true;;

let flush_ps () =
  if not !psused then psused := true;
  Gs.flush ();;

let flush_dvi () = GY.flush ();;
let flush_last () = if !last_is_dvi then flush_dvi () else flush_ps ();;

let sync b =
  if !last_is_dvi <> b then begin flush_last (); last_is_dvi := b end;;

let control_cursor = GY.Cursor_left_ptr;;
let move_cursor = GY.Cursor_fleur;;
let select_cursor = GY.Cursor_xterm;;
let free_cursor = ref control_cursor;;

type busy = Free | Busy | Pause | Disk;;

let busy_timeout = ref 0.;;

let last_cursor = ref control_cursor;;

let busy_set_cursor cursor =
  busy_timeout := 0.;
  last_cursor := cursor;
  GY.set_cursor cursor;;

let reset_cursor () = GY.set_cursor !last_cursor;;

(* To be called before system calls that make take a long time *)
let busy_timeout = ref None;;
let busy_start () =
  try
    busy_timeout := 
      Some (Timeout.add !busy_delay 
	      (fun () -> busy_set_cursor GY.Cursor_watch))
  with
  | _ -> ();;

let busy_end () =
  match !busy_timeout with
  | Some timeout -> 
      begin try Timeout.remove timeout with Not_found -> () end
  | None -> ();;
  
let set_busy sw =
  if !show_busy then
    match sw with
    | Pause ->
	busy_end ();
        busy_set_cursor GY.Cursor_right_side
    | Free ->
	busy_end ();
        busy_set_cursor !free_cursor
    | Disk ->
        busy_set_cursor GY.Cursor_exchange
    | Busy ->
        busy_start ();;

let title = ref "Advi";;
let set_title s = title := s;;

(* Applications function handlers. *)
let embeds = ref [];;
let persists = ref [];;
let unmap_embeds = ref [];;

let launch_embedded_apps() = 
  List.iter (fun f -> f ()) (List.rev !embeds); embeds := [];
  List.iter (fun f -> f ()) (List.rev !persists); persists := [];;

let synchronize () =
  Gs.flush ();
  Transimpl.synchronize_transition ();
  GY.synchronize ();
  launch_embedded_apps ();;

(* for refreshed signal on usr1 *)
exception Usr1;;
let waiting = ref false;;
let usr1 = 10;;
let usr1_status = ref false;;

let clear_usr1 () = usr1_status :=  false;;

let set_usr1 () =
  Sys.set_signal usr1
    (Sys.Signal_handle
       (fun _ -> usr1_status := true; if !waiting then raise Usr1));;

set_usr1 ();;

let sleep_broken = ref false;;
let clear_sleep () = sleep_broken := false;;

(* returns false if sleep is fully performed. returns true if interrupted *)
let sleep_watch breakable sync n =
  let start = Unix.gettimeofday () in
  let interrupted () = 
    if breakable && (!usr1_status || !sleep_broken || GY.key_pressed ())
    then begin
      if GY.key_pressed () then ignore (GY.read_key ());
      sleep_broken := true;
      true
    end else false
  in
  let rec delay t =
    if interrupted () then raise Exit (* if there is a sig or key press, exit*)
    else begin
      try ignore (Unix.select [] [] [] t) 
      with Unix.Unix_error(Unix.EINTR, _, _) -> ()
    end;
    let now = Unix.gettimeofday () in
    let remaining = start +. n -. now in
    if remaining > 0.0 then delay remaining
    else false
  in
  interrupted () || (* if it is interrupted, synchronization is not done *)
  begin
    if sync then synchronize ();
    try delay n with Exit -> true
  end;;

let sleep = sleep_watch true true;;

(* trans *)

Transimpl.sleep := sleep_watch true false;;

let set_transition trans = Transimpl.current_transition := trans;;

let transbox_save x y width height = 
  synchronize ();
  let x' = x and y' = !size_y - y in
  Transimpl.transbox_save x' y' width height;;
  
let transbox_go trans = 
  Gs.flush ();
  Transimpl.transbox_go trans;
  synchronize ();;

(*** Private glyphs ***)

type cache =
   | No_cache
   | Cached of (color * color) * Graphics.image;;

module Glyph =
  struct
    type t = {
        glyph : Glyph.t;
        mutable cache : cache;
        mutable img_list : ((color * color) * Graphics.image) list
      }

    let width g = g.glyph.Glyph.width
    let height g = g.glyph.Glyph.height
    let hoffset g = g.glyph.Glyph.hoffset
    let voffset g = g.glyph.Glyph.voffset
    let graymap g = g.glyph.Glyph.graymap
  end;;

type glyph = Glyph.t;;
open Glyph;;

let make_glyph g =
  { glyph = g;
    cache = No_cache;
    img_list = [] };;

let get_glyph g = g.glyph;;

(* Blending *)
type blend =
   | Normal | Multiply | Screen | Overlay (* | SoftLight | HardLight *)
   | ColorDodge | ColorBurn | Darken | Lighten | Difference
   | Exclusion (* | Luminosity | Color | Saturation | Hue *);;

let blend = ref Normal;;
let set_blend b = blend := b;;

(* look at gxblend.c of ghostscript *)
let blend_func = function
  | Normal -> raise Exit (* this case is optimized *)
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
        t lsr 8;;

(* Background implementation *)
type bkgd_prefs = {
  mutable bgcolor : int;
  mutable bgimg : string option;
  mutable bgratio : Drawimage.ratiopts;
  mutable bgwhitetrans : bool;
  mutable bgalpha : float;
  mutable bgblend : blend;
};;

(* The Background preferences                        *)
(* to be extended, should contain gradients etc. RDC *)

let set_default_color r s =
   r :=
     match String.lowercase s with
     | "black" -> Graphics.black
     | "white" -> Graphics.white
     | "red" -> Graphics.red
     | "green" -> Graphics.green
     | "blue" -> Graphics.blue
     | "yellow" -> Graphics.yellow
     | "cyan" -> Graphics.cyan
     | "magenta" -> Graphics.magenta
     | s -> int_of_string s;;

let default_bgcolor = ref Graphics.white;;
let default_fgcolor = ref Graphics.black;;
let fgcolor () = !default_fgcolor;;

let color = ref !default_fgcolor;;

Misc.set_option
  "-fgcolor"
  (Arg.String (set_default_color default_fgcolor))
  "STRING\tSet default foreground color (Named or RGB)";;

let default_bkgd_data () =
  { bgcolor = !default_bgcolor;
    bgimg = None;
    bgratio = Drawimage.ScaleAuto;
    bgwhitetrans = false;
    bgalpha = 1.0;
    bgblend = Normal };;

let blit_bkgd_data s d =
  d.bgcolor <- s.bgcolor;
  d.bgimg   <- s.bgimg;
  d.bgratio <- s.bgratio;
  d.bgwhitetrans <- s.bgwhitetrans;
  d.bgalpha <- s.bgalpha;
  d.bgblend <- s.bgblend;;

let bkgd_data = default_bkgd_data ();;

let copy_of_bkgd_data () =
  let c = default_bkgd_data () in
  blit_bkgd_data bkgd_data c;
  c;;

let draw_img file ratio whitetrans alpha blend psbbox (w,h) x0 y0 =
  if not !opened then failwith "Grdev.fill_rect: no window";
  let x = x0
  and y = !size_y - y0 in
  Drawimage.f
    file
    whitetrans
    alpha
    blend 
    psbbox ratio (w, h) (x,y);;

let draw_bkgd_img (w, h) x0 y0 =
  match bkgd_data.bgimg with
  | None -> ()
  | Some file ->
     draw_img
      file
      bkgd_data.bgratio
      bkgd_data.bgwhitetrans
      bkgd_data.bgalpha
      (try Some (blend_func bkgd_data.bgblend) with _ -> None)
      None (w, h) x0 (!size_y - y0);;

type bgoption =
   | BgColor of color
   | BgImg of string
   | BgAlpha of float
   | BgBlend of blend
   | BgRatio of Drawimage.ratiopts;;

let set_bg_option = function
  | BgColor c -> bkgd_data.bgcolor <- c
  | BgImg file -> bkgd_data.bgimg <- Some file
  | BgAlpha a -> bkgd_data.bgalpha <- a
  | BgBlend b -> bkgd_data.bgblend <- b
  | BgRatio f -> bkgd_data.bgratio <- f;;

let set_bg_options l = List.iter set_bg_option l;;

let bg_color = ref bkgd_data.bgcolor;;
let bg_colors = ref [];;

let push_bg_color c =
  bg_colors := !bg_color :: !bg_colors;
  bg_color := c;;

let pop_bg_color () =
  match !bg_colors with
  | h :: t -> bg_color := h; bg_colors := t
  | [] -> bg_color := bkgd_data.bgcolor;;

let background_colors = ref [];;
let add_background_color x y w h c =
  background_colors := (x, y, w, h, c) :: !background_colors;;

let find_bg_color x y w h =
  let rec find_color = function
    | (x0, y0, w0, h0, c) :: t ->
        if x0 <= x && y0 <= y && x + w <= x0 + w0 && y + h <= y0 + h0
        then c
        else find_color t
    | [] -> !bg_color in
  find_color !background_colors;;

(* Forward to Driver.playing. *)
let get_playing = ref (fun () -> 0);;

let get_bg_color x y w h =
  if !ignore_background then Graphics.white else
  if !get_playing () > 0 then !bg_color else
    begin
      sync dvi;
      if !psused || bkgd_data.bgimg <> None then
        let c = GY.point_color (x + 1) (y + 1) in
	let c' = GY.point_color (x + w - 1) (y + h - 1) in
        if c = c' then c else
	let rgb_of_color c =
	  let b = (c land 0x0000ff) in 
	  let g = (c land 0x00ff00) lsr 8 in
	  let r = (c land 0xff0000) lsr 16 in
	  r,g,b
	in
	let r, g, b  = rgb_of_color c  in
	let r', g', b' = rgb_of_color c' in
	Graphics.rgb ((r + r' + 1) / 2) ((g + g' + 1) / 2) ((b + b' + 1) / 2)
      else find_bg_color x y w h
    end;;

Misc.set_option
  "-bgcolor"
  (Arg.String
     (fun s ->
        set_default_color default_bgcolor s;
        bkgd_data.bgcolor <- !default_bgcolor))
  "STRING\tSet default background color (Named or RGB)";;

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
        let r = (k * r0 + i * r1) / 255
        and g = (k * g0 + i * g1) / 255
        and b = (k * b0 + i * b1) / 255 in
        table.(i) <- (r lsl 16) + (g lsl 8) + b
      done;
      Hashtbl.add htable col table;
      table;;

let get_image g col =
  match g.cache with
  | Cached (c, img) when c = col -> img
  | _ ->
      let img =
        try List.assoc col g.img_list
        with Not_found ->
          let gmap = graymap g
          and w = Glyph.width g
          and h = Glyph.height g in
          (* We enforce [h <> 0] and [w <> 0] because
             Caml graphics don't like zero-sized pixmaps. *)
          let dst = Array.make_matrix (max 1 h) (max 1 w) Graphics.transp
          and table = get_color_table col
          and p = ref 0 in
          for i = 0 to h - 1 do
            for j = 0 to w - 1 do
              dst.(i).(j) <- table.(Char.code gmap.[!p]);
              incr p
            done
          done;
          let img = Graphics.make_image dst in
          g.img_list <- (col, img) :: g.img_list;
          img in
      g.cache <- Cached(col, img);
      img;;

(*** Device manipulation ***)
type rect = { x : int; y : int; h : int; w : int };;
let nobbox =  { x = 0; y = 0; w = 10; h = 10 };;
let bbox = ref nobbox;;

let set_bbox bb =
  if not !opened then failwith "Grdev.set_bbox: no window";
  match bb with
  | None ->
      bbox := nobbox;
  | Some(x0, y0, w, h) ->
      bbox := { x = x0; y = !size_y - y0; w = w; h = -h};;

(*** Drawing ***)
let set_color col =
  if not !opened then failwith "Grdev.set_color: no window";
  (*prerr_endline "set_color";*)
  color := col;
  Graphics.set_color col;;

let draw_glyph g x0 y0 =
  if not !opened then failwith "Grdev.draw_glyph: no window";
  let w = Glyph.width g
  and h = Glyph.height g in
  let x = x0 - hoffset g
  and y = !size_y - y0 + voffset g - h in
  if x + w > !xmin && x < !xmax && y + h > !ymin && y < !ymax
  then begin
    let bg = get_bg_color x y w h in
    let img = get_image g (bg, !color) in
    Graphics.draw_image img x y;
  end;;

let fill_rect x0 y0 w h =
  if not !opened then failwith "Grdev.fill_rect: no window";
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
    let (x, y) = path.(i) in
    newpath.(i) <- (x, !size_y - y)
  done;
  newpath;;

let draw_path path ~pensize =
  if not !opened then failwith "Grdev.draw_path: no window";
  let path = adjust_path path in
  Graphics.set_line_width pensize;
  Graphics.draw_poly_line path;
  Graphics.set_line_width 1;;

let set_shade shade =
  let r = 0xFF - (!color lsr 16) land 0xFF
  and g = 0xFF - (!color lsr 8) land 0xFF
  and b = 0xFF - !color land 0xFF in
  let r = 0xFF - int_of_float (shade *. float r)
  and g = 0xFF - int_of_float (shade *. float g)
  and b = 0xFF - int_of_float (shade *. float b) in
  Graphics.set_color (Graphics.rgb r g b);;

let fill_path path ~shade =
  if not !opened then failwith "Grdev.fill_path: no window";
  let path = adjust_path path in
  set_shade shade;
  Graphics.fill_poly path;
  Graphics.set_color !color;;

let draw_arc ~x ~y ~rx ~ry ~start:s ~stop:e ~pensize =
  if not !opened then failwith "Grdev.draw_arc: no window";
  Graphics.set_line_width pensize;
  Graphics.draw_arc x (!size_y - y) rx ry (- s) (- e);
  Graphics.set_line_width 1;;

let fill_arc ~x ~y ~rx ~ry ~start:s ~stop:e ~shade =
  if not !opened then failwith "Grdev.fill_arc: no window";
  set_shade shade;
  Graphics.fill_arc x (!size_y - y) rx ry (- s) (- e);
  Graphics.set_color !color;;

let epstransparent = ref true;;
let set_epstransparent s = epstransparent := s;;

let alpha = ref 1.0;;
let set_alpha a = alpha := a;;

let draw_ps file bbox (w, h) x0 y0 =
  if not !opened then failwith "Grdev.fill_rect: no window";
  let x = x0
  and y = !size_y - y0 + h in
  try Drawimage.f file !epstransparent !alpha
      (try Some (blend_func !blend) with _ -> None)
      (Some bbox) Drawimage.ScaleAuto (w, h) (x, y - h)
  with
  | Not_found -> Misc.warning ("ps file " ^ file ^ " not found")
  | _ ->
      Misc.warning ("error happened while drawing ps file " ^ file);;

let clean_ps_cache () = Drawimage.clean_cache ();;

(* Embedded (tcl/tk) applications *)

type app_mode = Sticky | Persistent | Ephemeral;;

let app_table = Hashtbl.create 17;;

let raw_embed_app command app_mode app_name width height x y =
  let string_replace pat templ str =
    let result = Buffer.create (String.length str * 2) in
    let patlen = String.length pat in
    let find pat str at =
      let rec find_aux pos =
        if String.sub str pos patlen = pat then pos
        else find_aux (pos + 1) in
      try find_aux at with _ -> raise Not_found in
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
          Buffer.contents result in
    replace 0 in

  (* Use graphics coordinates for subwindows *)
  let gry = !size_y - y in

  let wid = GY.open_subwindow ~x ~y:gry ~width ~height in

  (*** !x commands
    !p : embedding target window id (in digit)

      If !p is not specified, the applications will be treated by WM.
      (If they are X apps, of course...)

    !g : geometry like 100x100+20+30
    !w : width  of the target window in pixel
    !h : height of the target window in pixel
    !x : x of the application against the root
    !y : y of the application against the root

    Why "!"?  '\' is for TeX. "%" is for TeX. "$" is for TeX...
  ***)

  let command0 = string_replace "!p" wid command in

  (* if there is no !p, the application geometry will be treated
     by the WM. In such cases, we try to fix the geometry
     so that it is against the root. *)

  let opt_geometry, opt_x, opt_y =
    let px, py =
      let against_root = command0 = command in
      if against_root then
        (* fix the geometry *)
        let (ww, wh, wx, wy) = GY.get_geometry () in
        x + wx, y - height + wy
      else 0, 0 in
    Printf.sprintf "%dx%d+%d+%d" width height px py,
    string_of_int px,
    string_of_int py in

  let command =
    string_replace "!g" opt_geometry
        (string_replace "!w" (string_of_int width)
            (string_replace "!h" (string_of_int height)
               (string_replace "!x" opt_x
                  (string_replace "!y" opt_y
                     command0)))) in
  (* prerr_endline command; *)
  let pid = Misc.fork_process command in
  if Hashtbl.mem app_table pid then
    raise (Failure (Printf.sprintf
                      "pid %d is already in the app_table!" pid));
  Hashtbl.add app_table pid (app_mode, app_name, wid);
;;

(* In hash table t, returns the first element that verifies p. *)
let hashtbl_find t p =
  let res = ref None in
  try
   Hashtbl.iter (fun k x -> if p x then (res := Some (k, x); raise Exit)) t;
   raise Exit
  with Exit ->
   match !res with
   | None -> raise Not_found
   | Some k_x -> k_x;;

let find_embedded_app app_name =
  hashtbl_find app_table (fun (_, name, _) -> name = app_name);;

let map_embed_app command app_mode app_name width height x y =
  let _, (app_mode, app_name, wid) = find_embedded_app app_name in
  GY.map_subwindow wid;;

let unmap_embed_app command app_mode app_name width height x y =
 let _, (app_mode, app_name, wid) = find_embedded_app app_name in
 GY.unmap_subwindow wid;;

let move_or_resize_persistent_app command app_mode app_name width height x y =
  let _, (app_mode, app_name, wid) = find_embedded_app app_name in
  GY.resize_subwindow wid width height;
  let gry = !size_y - y + height - width in
  GY.move_subwindow wid x gry;
;;

(* In hash table t, verifies that at least one element verifies p. *)
let hashtbl_exists t f =
  try Hashtbl.iter (fun _ x -> if f x then raise Exit) app_table; false
  with Exit -> true;;

(* embedded apps must be displayed when synced *)
let embed_app command app_mode app_name width height x y =
  let already_launched app_name =
    hashtbl_exists app_table (fun (ty, name, wid) -> name = app_name) in
  match app_mode with
  | Sticky ->
     if not (already_launched app_name) then
     embeds :=
      (fun () ->
         (* prerr_endline ("Launching " ^ app_name); *)
         raw_embed_app command app_mode app_name width height x y) ::
      !embeds else
     persists :=
      (fun () ->
          prerr_endline ("Moving " ^ app_name);
	  move_or_resize_persistent_app command app_mode app_name
          width height x y) :: 
      !persists
  | Persistent ->
     if not (already_launched app_name) then
     embeds :=
      (fun () ->
        raw_embed_app command app_mode app_name width height x y) ::
      !embeds;
     persists :=
      (fun () ->
        map_embed_app command app_mode app_name width height x y) ::
      !persists;
     unmap_embeds :=
      (fun () ->
        unmap_embed_app command app_mode app_name width height x y) ::
      !unmap_embeds;
  | Ephemeral ->
     embeds :=
      (fun () ->
         (* prerr_endline ("Launching " ^ app_name); *)
         raw_embed_app command app_mode app_name width height x y) ::
      !embeds;;

let kill_app pid wid =
  (*prerr_endline (Printf.sprintf "kill_app (pid=%d,window=%s)" pid wid);*)
  begin try Hashtbl.remove app_table pid with _ -> 
    prerr_endline "kill_app failed to remove application..."
  end;
  begin try Unix.kill pid 9 with _ -> 
    (* prerr_endline (Printf.sprintf "kill_app (pid=%d,window=%s): process already dead" pid wid); *)
    ()
  end;
  while
    try
      let pid', _ = Unix.waitpid [Unix.WNOHANG] 0 in
      pid' <> 0
    with
      Unix.Unix_error(Unix.ECHILD, _, _) -> false
  do () done;
  prerr_endline (Printf.sprintf "kill_app (pid=%d,window=%s)" pid wid);
  (* if this is the forked process, do not close the window!!! *)
  if Unix.getpid () = Misc.advi_process then GY.close_subwindow wid
;;

let kill_apps app_mode =
  (* begin match app_mode with
  | Persistent -> prerr_endline "Killing persistent apps"
  | Sticky -> prerr_endline "Killing sticky apps"
  | Ephemeral -> prerr_endline "Killing ephemeral apps"
  end; *)
  let to_be_killed =
    Hashtbl.fold (fun pid (apt, app_name, wid) acc ->
      if apt = app_mode then (pid,wid) :: acc else acc) app_table []
  in
  List.iter (fun (pid,wid) -> kill_app pid wid) to_be_killed;;

let signal_app sig_val pid wid =
  prerr_endline
    (Printf.sprintf
      "signal_app (pid=%d,window=%s) signal=%i killing=%b kill is %i"
      pid wid sig_val (sig_val = Sys.sigquit) Sys.sigquit);
  if sig_val = Sys.sigquit then kill_app pid wid else
  try Unix.kill pid sig_val with _ ->
    (* prerr_endline
        (Printf.sprintf
          "signal_app (pid=%d,window=%s) signal=%i: cannot signal process"
          pid wid sig_val); *)
    ();;

let kill_embedded_app sig_val app_name =
  (* prerr_endline
   (Printf.sprintf
     "kill_embedded_app (signal=%i app_name=%s)"
     sig_val app_name); *)
  try
    let pid, (app_mode, app_name, wid) = find_embedded_app app_name in
    signal_app sig_val pid wid
  with
  | Not_found ->
      Misc.warning (Printf.sprintf "application %s is not running" app_name);;

let unmap_persistent_apps () =
  List.iter (fun f -> f ()) (List.rev !unmap_embeds);
  unmap_embeds := [];;

let kill_ephemeral_apps () =
  kill_apps Ephemeral;;

let kill_persistent_apps () =
  kill_apps Sticky;
  unmap_persistent_apps ();
  kill_apps Persistent;;

let kill_all_embedded_apps () =
  kill_ephemeral_apps ();
  kill_persistent_apps ();;


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

  end;;

module H =
  struct
    type mode = Over | Click_down  
      type link = 
          { link : string;
            action : (unit -> unit);
            mode : mode;
            color : color option;
            area : (int * int * int) option;
          } 
    type tag =
      | Name of string
      | Href of string
      | Advi of link

    type anchor = {
        tag : tag;
        draw : (int * int * glyph) list
      }

    let anchors = ref A.empty
    let clear () = anchors := A.empty

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
      let make_anchor  draw (x, y as orig) w h voff =
        let anchor = {tag = tag; draw = List.rev draw } in
        let e =
          match tag with
          | Href _ -> 0
          | Advi _ -> 0
          | Name _ -> 0 in
        let y' = y - voff - 1 in
        let h' = h + 2 in
        let a =
          { A.x = x - e;
            A.y =  (!size_y - y' - h') - e;
            A.w = w + e + e;
            A.h = h' + e + e;
            A.action = anchor;
          } in
        anchors := A.add a !anchors;
        match tag with
        | Href _ -> draw_anchor href_frame 1 a
        | Advi _ -> draw_anchor advi_frame 1 a
        | _ -> ()
      in
      let rec split draw (x, y as orig) w h voff = function
        | [] -> make_anchor  draw orig w h voff

        | (x1, y1, g1 as d) :: rest ->
            if x1 + width g1 > x then
              split (d :: draw) orig
                (max w ((x1 - x) + width g1))
                (max h (height g1))
                (max voff (voffset g1)) rest
            else
              begin
                make_anchor  (d :: draw) orig w h voff;
                start rest
              end
      and start = function
        | [] -> ()
        | (x, y, g as d) :: rest ->
            split [d] (x, y) (width g) (height g) (voffset g) rest in
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
      match emph with
      | Rect (ima, act, l) ->
          GY.display_mode now;
          List.iter
            (function ima, act -> Graphics.draw_image ima act.A.x act.A.y) l;
          Graphics.draw_image ima act.A.x act.A.y;
          GY.set_cursor !free_cursor;
          GY.display_mode false
      | Screen (ima, act, all_anchors) ->
          GY.display_mode true;
          anchors := all_anchors;
          Gs.flush ();
          (* long delay to be safe *)
          ignore (sleep_watch false false 0.1);
          Graphics.draw_image ima 0 0;
          GY.set_cursor !free_cursor;
          GY.flush ();
          GY.display_mode false
      | Nil -> ()

    let emphasize c act =
      let ima = Graphics.get_image act.A.x act.A.y act.A.w act.A.h in
      Graphics.set_color c;
      GY.display_mode true;
      Graphics.fill_rect act.A.x act.A.y act.A.w act.A.h;
      Graphics.set_color !color;
      push_bg_color c;
      List.iter (function x, y, g -> draw_glyph g x y) act.A.action.draw;
      pop_bg_color ();
      GY.set_cursor GY.Cursor_hand2;
      GY.display_mode false;
      Rect (ima, act, [])

    let save_screen_exec act a =
      Gs.flush ();
      (* get image take the image from the backing store *)
      let ima = Graphics.get_image 0 0 !size_x !size_y in
      GY.sync ();
      (* wait until all events have been processed, flush should suffice *)
(*
   Graphics.set_color (Graphics.point_color 0 0);
   (* it seems that the image is saved ``lazily'' and further instruction
      could be capture in the image *)
   sleep_watch false false 0.05;
 *)
      let all_anchors = !anchors in
      a ();
      flush_last ();
      GY.synchronize ();
      launch_embedded_apps ();
      Screen (ima, act, all_anchors)

    let light t =
      try
        match t with
        | Name n ->
            let t = Name (Misc.get_suffix "#" n) in
            emphasize name_emphasize (find_tag t)
        | _ -> Nil
      with
      | Not_found | Misc.Match -> Nil

    let flashlight t =
      deemphasize false (light t)

    let emphasize_and_flash color act =
      let emph = emphasize color act in
      let m =
        match act.A.action.tag with
        | Href n -> light (Name n)
        | _ -> Nil in
      match emph, m with
      | Rect (ima, a, l), Rect (ima', a', l') ->
          Rect (ima, a, (ima', a') :: (l' @ l))
      | x, _ -> x

    let reemphasize emph act =
      deemphasize true emph;
      emphasize_and_flash href_emphasize act
  end;;

(*** Clearing device ***)

module Symbol = Symbol.Make (Glyph);;

let cut s =
  (*  print_string s; print_newline (); *)
  (* cut does not work yet *)
  GY.cut s;;

let open_dev geom =
  if !opened then Graphics.close_graph ();
  Graphics.open_graph geom;

  GY.init (); (* we disable Graphics's event retrieving *)
  Timeout.init ();
  (* Fill the event queue *)
  let rec f () =
    Timeout.add 0.25 (fun () -> 
      GY.retrieve_events (); ignore (f ()))
  in
  ignore (f ());

  size_x := Graphics.size_x ();
  size_y := Graphics.size_y ();
  xmin := 0; xmax := !size_x;
  ymin := 0; ymax := !size_y;
  Graphics.remember_mode true;
  GY.display_mode !Misc.global_display_mode;
  Graphics.set_window_title !title;
  color := !default_fgcolor;
  opened := true;;

let close_dev () =
  if !opened then begin
    kill_ephemeral_apps ();
    kill_persistent_apps ();
    Graphics.close_graph ();
  end;
  opened := false;;

let clear_dev () =
  if not !opened then failwith "Grdev.clear_dev: no window";
  kill_ephemeral_apps ();
  unmap_persistent_apps ();
  GY.display_mode !Misc.global_display_mode;
  Graphics.clear_graph ();
  H.clear ();
  bg_color := bkgd_data.bgcolor; (* modifiable via \setbgcolor . RDC *)
  bg_colors := [];
  background_colors := [];
  Symbol.clear ();
  size_x := Graphics.size_x ();
  size_y := Graphics.size_y ();
  xmin := 0; xmax := !size_x;
  ymin := 0; ymax := !size_y;
  (* here we add the background setting. RDC *)
  Graphics.set_color !bg_color;
  if (!bg_color <> Graphics.white) then Graphics.fill_rect !xmin !ymin !xmax !ymax;
  (* now try to handle background images *)
  draw_bkgd_img (!xmax, !ymax) 0 0;;

(*** Events ***)

type status = GY.status = {
    mouse_x : int;
    mouse_y : int;
    button : bool;
    keypressed : bool;
    key : char;
    modifiers : int;
  };;

type area =
   | Bottom_right | Bottom_left | Top_right | Top_left | Middle;;
type button =
   | Button1 | Button2 | Button3;;
type event =
   | Resized of int * int
   | Refreshed
   | Key of char
   | Move of int * int
   | Region of int * int * int * int
   | Selection of string
   | Position of int * int
   | Href of string
   | Advi of string * (unit -> unit)
   | Click of area * button * int * int
   | Nil;;

type option_event =
   | Final of event
   | Raw of status;;

let all_events = [
  GY.Button_down;
  GY.Button_up;
  GY.Mouse_motion; 
  GY.Key_pressed;
];;

let button_up_motion = [
  GY.Button_up;
  GY.Mouse_motion; 
];;

let button_up = [
  GY.Button_up;
];;

let event = ref [];;
let push_back_event ev =
  if List.length !event > 1 then Misc.warning "STACK";
  event :=  ev :: !event;;
let event_waiting () =
  match !event with [] -> false | _ -> true;;
let rec pop_event () =
  match !event with
  | [] -> assert false
  | h :: t -> event := t; h;;

let mouse_x = ref 0;;
let mouse_y = ref 0;;
let button = ref false;;

let reposition ~x ~y ~w ~h =
  GY.reposition x y w h;
  let x = Graphics.size_x () and y = Graphics.size_y() in
  size_x := x;
  size_y := y;
  Gs.kill ();
  x, y;;

let resized () =
  let x = Graphics.size_x () and y = Graphics.size_y () in
  let b = x <> !size_x || y <> !size_y in
  if b then
    begin
      size_x := x;
      size_y := y;
      Gs.kill ();
      Some (x, y)
    end
  else None;;

(* pour sauver une image_rectiligne *)
type rectangular_image =
    { north : Graphics.image;
      south : Graphics.image;
      east : Graphics.image;
      west : Graphics.image; };;

let rec save_rectangle x y dx dy =
  let x = min x (x + dx) in
  let y = min y (y + dy) in
  let dx = max 1 (abs dx) in
  let dy = max 1 (abs dy) in
  { south = Graphics.get_image x y dx 1;
    west = Graphics.get_image x y 1 dy;
    east = Graphics.get_image (x + dx) y 1 dy;
    north =  Graphics.get_image x (y + dy) (succ dx) 1;
  };;

let rec restore_rectangle r x y dx dy =
  let x = min x (x + dx) in
  let y = min y (y + dy) in
  let dx = max 1 (abs dx) in
  let dy = max 1 (abs dy) in
  begin
    Graphics.draw_image r.south x y;
    Graphics.draw_image r.west x y;
    Graphics.draw_image r.east (x + dx) y;
    Graphics.draw_image r.north x (y + dy);
  end;;

let rec draw_rectangle x y dx dy =
  Graphics.moveto x y;
  Graphics.lineto (x + dx) y;
  Graphics.lineto (x + dx) (y + dy);
  Graphics.lineto (x) (y + dy);
  Graphics.lineto (x) (y);;

let wait_button_down () = 
  if GY.button_down () then
    ignore (GY.wait_next_event [ GY.Button_up ]);;


let rec wait_signal_event events =
    match resized (), !usr1_status, event_waiting () with
    | Some (x, y), _, _ -> Final (Resized (x, y))
    | _, true, _ -> clear_usr1 (); Final (Refreshed)
    | _, _, true -> Raw (pop_event ())
    | _, _, _ ->
        try
          waiting := true;
          let ev = GY.wait_next_event events in
          waiting := false;
          match resized () with
          | Some (x, y) ->
              push_back_event ev;
              Final (Resized (x, y))
          | None ->
              Raw ev
        with
        | Usr1 ->
            waiting := false;
            Final Refreshed
        | exn ->
            waiting := false;
            raise exn;;

let wait_select_rectangle x y =
  let rec select dx dy =
    let buf = save_rectangle x y dx dy in
    draw_rectangle x y dx dy;
    let ev = wait_signal_event button_up_motion in
    restore_rectangle buf x y dx dy;
    match ev with
    | Raw e ->
        let dx' = e.GY.mouse_x - x in 
        let dy' = e.GY.mouse_y - y in 
        if e.GY.button then select dx' dy'
        else Final (Region (x, y, dx', 0 - dy'))
    | x -> x
  in
  set_color !default_fgcolor;
  GY.display_mode true;
  GY.set_cursor select_cursor;
  let restore () =
    GY.display_mode false;
    set_color !color;
    GY.set_cursor !free_cursor
  in
  try let e = select 0 0 in restore(); e
  with exn -> restore(); raise exn;;

let wait_select_button_up m x y =
  let draw_color b =
    let draw s c x y =
      set_color (if b then c else cut_emphasize);
      draw_glyph s x y in
    Symbol.apply draw in
  let rec select r =
    let ev = wait_signal_event button_up_motion in
    match ev with
    | Raw e ->
        let x' = e.GY.mouse_x in 
        let y' = e.GY.mouse_y in
        let r' = Symbol.new_region r x' (!size_y - y') in
        Symbol.iter_regions (draw_color true) (draw_color false) r r';
        if e.GY.button then select r'
        else
          let m = GY.get_modifiers () in
          if m land GY.shift = 0 then
            begin
              Symbol.iter_region (draw_color true) r';
              Final Nil
            end
          else
            Final (Selection (Symbol.region_to_ascii r'))
    | x -> x in
  let color = !color in
  GY.synchronize ();
  GY.display_mode true;
  Graphics.remember_mode false;
  GY.set_cursor select_cursor;
  let restore () =
    GY.display_mode false;
    Graphics.remember_mode true;
    set_color color;
    GY.set_cursor !free_cursor in
  try
    let e =
      if m land GY.button2 = 0 then
        let r = Symbol.position x (!size_y - y) in
        select r
      else
        match Symbol.word x (!size_y - y) with
        | Some (r, w) ->
            Symbol.iter_region (draw_color false) r;
            Final (Selection w)
        | None -> Final Nil in
    restore ();
    e
  with
  | exn ->
      restore ();
      match exn with
      | Not_found -> Final Nil
      | _ -> raise exn;;

let wait_move_button_up x y =
  let bbox = !bbox in
  let w = bbox.w and h = bbox.h in
  let rec move dx dy =
    let x' = bbox.x + dx and y' = bbox.y + dy in
    let buf = save_rectangle x' y' w h in
    draw_rectangle x' y' w h;
    let ev = wait_signal_event button_up_motion in
    restore_rectangle buf x' y' w h;
    match ev with
    | Raw e ->
        let dx' = e.GY.mouse_x - x in 
        let dy' = e.GY.mouse_y - y in 
        if e.GY.button then move dx' dy'
        else Final (Move (dx', 0 - dy'))
    | x -> x in
  let color = !color in
  set_color !default_fgcolor;
  GY.set_cursor move_cursor;
  GY.display_mode true;
  let restore () =
    GY.display_mode false;
    set_color color;
    GY.set_cursor !free_cursor in
  try let e = move 0 0 in restore (); e
  with exn -> restore (); raise exn;;

let near x x' = abs (x - x') < !size_x / 4;;

let click_area x y =
  if near x 0 then
    if near y 0 then Bottom_left else
    if near y !size_y then Top_left
    else Middle else
  if near x !size_x then
    if near y 0 then Bottom_right else
    if near y !size_y then Top_right
    else Middle
  else Middle;;

let button m =
  if m land GY.button1 <> 0 then Button1 else
  if m land GY.button2 <> 0 then Button2 else
  if m land GY.button3 <> 0 then Button3
  else Button2;;

let wait_button_up m x y =
  if m land GY.control <> 0 then wait_move_button_up x y else
  if m land GY.shift <> 0 && m land GY.button1 = 0 then
    wait_select_button_up m x y
  else
    begin
      match wait_signal_event button_up with
      | Raw e ->
          if m land GY.shift <> 0
          then Final (Position (x, !size_y - y))
          else Final (Click (click_area x y, button m, x, !size_y - y))
      | x -> x
    end;;

let wait_event () =
  (* We reached to a pause. Now we can reset the sleep break *)
  clear_sleep ();
  let temp_cursor = ref false in
  reset_cursor ();
  let rec event emph b =
    let send ev = H.deemphasize true emph; ev in
    let rescan () = H.deemphasize true emph; event H.Nil false in
    match wait_signal_event all_events with
    | Raw ev ->
        begin
          if ev.GY.keypressed then begin
            send (Key ev.GY.key)
          end else
            let ev' = 
              { mouse_x = ev.GY.mouse_x;
                mouse_y = !size_y - ev.GY.mouse_y;
                button = ev.GY.button;
                keypressed = ev.GY.keypressed;
                key = ev.GY.key; 
                modifiers = ev.GY.modifiers; } in
            begin try
              begin match H.find ev.mouse_x ev.mouse_y with
              | {A.action = {H.tag = H.Href h; H.draw = d }} as act ->
                  if ev.button then
                    begin
                      let ev' = GY.wait_next_event button_up in
                      send (Href h)
                    end
                  else if H.up_to_date act emph then
                    event emph b
                  else
                    begin
                      H.deemphasize true emph;
                      event (H.emphasize_and_flash href_emphasize act) b
                    end
              | {A.action =
                 {H.tag = H.Advi {H.link=s; H.action=a; H.mode=H.Over};
                  H.draw = d }} as act ->
                  if H.up_to_date act emph then event emph b
                  else
                    begin
                      H.deemphasize true emph;
                      event (H.save_screen_exec act a) b
                    end
              | {A.action =
                 {H.tag = H.Advi {H.link=s; H.action=a; H.mode=H.Click_down};
                  H.draw = d }} as act ->
                  if ev.button && not b then
                    begin
                      H.deemphasize true emph;
                      event (H.save_screen_exec act a) true;
                    end
                  else if ev.button then event emph b
                  else if H.up_to_date act emph then
                    event emph b
                  else 
                    begin
                      H.deemphasize true emph;
                      event (H.emphasize_and_flash href_emphasize act) b
                    end
              | _ ->
                  rescan ()
              end
            with
            | Not_found ->
                if ev'.button then begin
                  let m = GY.get_modifiers () in
                  match wait_button_up m ev.mouse_x ev.mouse_y with
                  | Final (Region (x, y, dx, dy) as e) -> send e
                  | Final (Selection s as e) -> send e
                  | Final (Position (x, y) as e) -> send e
                  | Final (Move (dx, dy) as e) -> send e
                  | Final (Click (_, _, _, _) as e) -> send e
                  | Final Nil -> send Nil
                  | Final e ->
                      push_back_event ev;
                      send e
                  | Raw _ -> rescan ()
                end else begin
                  let m = GY.get_modifiers () in
                  if m land GY.shift <> 0 then
                     (if not !temp_cursor then
                       (temp_cursor:= true; GY.set_cursor select_cursor))
                  else if !temp_cursor then
                    (temp_cursor:= false; reset_cursor ());
                  rescan ()
		end
            end
        end
    | Final e -> send e in
  event H.Nil false;;

(* To be changed *)
exception GS = Gs.Terminated;;

let resized () =
  Graphics.size_x () <> !size_x || Graphics.size_y () <> !size_y;;

let continue () =
  if resized() || GY.key_pressed() (*  || !usr1_status *) then
    begin
      Gs.flush ();
      raise Stop
    end;;

(* Calling GS *)

let current_pos () =
  if not !last_is_dvi then flush_ps ();
  let x = !Gs.current_x and y = !Gs.current_y in
  x, y;;

let newpage x y z t w =
  Gs.newpage x y z t w;
  Gs.flush ();
  last_is_dvi := false;
  psused := false;;

let add_headers l =
  Gs.add_headers l;;

let exec_ps s x0 y0 =
  sync ps;
  if not !opened then failwith "Grdev.exec_ps: no window";
  Gs.draw s x0 y0;;
