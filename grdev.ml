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

open Misc;;

let ignore_background =
    Options.flag false
    "--ignore_background"
    "\tIgnore background for antialiasing";;

type color = Graphics.color;;
let href_frame = 0x00ff00;;
let advi_frame = 0xaaaaff;;
let rect_emphasize = Graphics.blue;;
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

let flush_dvi () = GraphicsY11.flush ();;
let flush_last () = if !last_is_dvi then flush_dvi () else flush_ps ();;

let sync b =
  if !last_is_dvi <> b then begin flush_last (); last_is_dvi := b end;;

let title = ref "Advi";;
let set_title s = title := s;;

let synchronize () =
Misc.debug_stop "Dev.synchronize";
  Gs.flush ();
  Transimpl.synchronize_transition ();
  GraphicsY11.synchronize ();
  Launch.launch_embedded_apps ();;

(* For refreshed signal on usr1 *)
exception Usr1;;

let waiting = ref false;;
let usr1 = Sys.sigusr1;;
let usr1_status = ref false;;

let clear_usr1 () = usr1_status := false;;

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
    if breakable &&
       (!usr1_status || !sleep_broken || GraphicsY11.key_pressed ())
    then begin
      if GraphicsY11.key_pressed () then ignore (GraphicsY11.read_key ());
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

(* if -nopauses is specified, Transimpl.sleep function is overridden by
   (fun _ -> true) (look at dviview.ml) *)
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

let blend = ref Drawimage.Normal;;
let set_blend b = blend := b;;

(* Viewport type definition *)

type viewport = int*int*int*int;; 

(* Background implementation *)

(* The Background preferences                    *)
(* to be extended, should contain gradients etc. *)

type bkgd_prefs = {
  mutable bgcolor : color;
  mutable bgimg : string option;
  mutable bgratio : Drawimage.ratiopts;
  mutable bgwhitetrans : bool;
  mutable bgalpha : Drawimage.alpha;
  mutable bgblend : Drawimage.blend;
  mutable bgviewport: viewport option; 
  (* hook for sophisticated programmed graphics backgrounds *)
  mutable bgfunction: (viewport -> unit) option; 
};;


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

Options.add
  "-fgcolor"
  (Arg.String (set_default_color default_fgcolor))
  "STRING\tSet default foreground color (Named or RGB)";;

let default_bkgd_data () =
  { bgcolor = !default_bgcolor;
    bgimg = None;
    bgratio = Drawimage.ScaleAuto;
    bgwhitetrans = false;
    bgalpha = 1.0;
    bgblend = Drawimage.Normal;
    bgviewport = None;
    bgfunction = None};;

let blit_bkgd_data s d =
  d.bgcolor <- s.bgcolor;
  d.bgimg   <- s.bgimg;
  d.bgratio <- s.bgratio;
  d.bgwhitetrans <- s.bgwhitetrans;
  d.bgalpha <- s.bgalpha;
  d.bgblend <- s.bgblend;
  d.bgviewport <- s.bgviewport;
  d.bgfunction <- s.bgfunction;;

let bkgd_data = default_bkgd_data ();;

let copy_of_bkgd_data () =
  let c = default_bkgd_data () in
  blit_bkgd_data bkgd_data c;
  c;;

let bg_color = ref bkgd_data.bgcolor;;
let bg_colors = ref [];;

let draw_img file ratio whitetrans alpha blend psbbox (w, h) x0 y0 =
  if not !opened then failwith "Grdev.draw_img: no window";
  let x = x0
  and y = !size_y - y0 in
  Drawimage.f
    file
    whitetrans
    alpha
    blend
    psbbox ratio (w, h) (x, y);;

let draw_bkgd () = 
  (* find the viewport *)
  let (w,h,xoff,yoff) as viewport =
    match bkgd_data.bgviewport with
      None -> (!size_x,!size_y,!xmin,!ymin)
    | Some v -> v
  in 	  
  (* Background: color. *)
  bg_color := bkgd_data.bgcolor;
  Graphics.set_color !bg_color;
  if !bg_color <> Graphics.white
  then Graphics.fill_rect xoff yoff w h;
  (* Background: image. *)
  lift (fun file ->
     draw_img
      file
      bkgd_data.bgratio
      bkgd_data.bgwhitetrans
      bkgd_data.bgalpha
      bkgd_data.bgblend
      None (w, h) xoff (!size_y - yoff)
  ) bkgd_data.bgimg;
  (* Background: function. *)
  lift (fun draw -> draw  viewport) bkgd_data.bgfunction;
  synchronize()
;;

type bgoption =
   | BgColor of color
   | BgImg of Misc.file_name
   | BgAlpha of Drawimage.alpha
   | BgBlend of Drawimage.blend
   | BgRatio of Drawimage.ratiopts
   | BgViewport of viewport option
   | BgFun of (viewport -> unit) option
;;

let set_bg_option = function
  | BgColor c -> bkgd_data.bgcolor <- c
  | BgImg file -> bkgd_data.bgimg <- Some file
  | BgAlpha a -> bkgd_data.bgalpha <- a
  | BgBlend b -> bkgd_data.bgblend <- b
  | BgRatio f -> bkgd_data.bgratio <- f
  | BgViewport v -> bkgd_data.bgviewport <- v
  | BgFun f -> bkgd_data.bgfunction <- f
;;

let set_bg_options l = List.iter set_bg_option l;;

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

let mean_color c c' =
  let rgb_of_color c =
    let b = (c land 0x0000ff) in
    let g = (c land 0x00ff00) lsr 8 in
    let r = (c land 0xff0000) lsr 16 in
    r, g, b in
  let r, g, b  = rgb_of_color c  in
  let r', g', b' = rgb_of_color c' in
  Graphics.rgb ((r + r' + 1) / 2) ((g + g' + 1) / 2) ((b + b' + 1) / 2);;

let get_bg_color x y w h =
  if !ignore_background then Graphics.white else begin
    sync dvi;
    if !psused || bkgd_data.bgimg <> None || bkgd_data.bgfunction <> None || bkgd_data.bgviewport <> None then
      let point_color x y =
        let x' = min (!size_x-1) x and y' = min (!size_y-1) y in
        GraphicsY11.point_color x' y' in
      let c = point_color (x + 1) (y + 1) in
      let c' = point_color (x + w - 1) (y + h - 1) in
      if c = c' then c else
      if !get_playing () > 0 then find_bg_color x y w h else
      mean_color c c'
    else find_bg_color x y w h
  end;;

Options.add
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

let get_glyph_image g col =
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
type 'a rect = { x : 'a; y : 'a; h : 'a; w : 'a };;
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
  if x + w > !xmin && x < !xmax && y + h > !ymin && y < !ymax then begin
    let bg = get_bg_color x y w h in
    let img = get_glyph_image g (bg, !color) in
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
  if w > 0 && h > 0 then begin
    Graphics.fill_rect x y w h;
    add_background_color x y w h !color;
  end;;

(* TODO: implement clipping in the following primitives ? *)
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
  try Drawimage.f file !epstransparent !alpha !blend
        (Some bbox) Drawimage.ScaleAuto (w, h) (x, y - h)
  with
  | Not_found -> Misc.warning ("ps file " ^ file ^ " not found")
  | _ -> Misc.warning ("error happened while drawing ps file " ^ file)
;;

let clean_ps_cache () = Drawimage.clean_cache ();;

(*** HTML interaction ***)

(* pour sauver une image_rectiligne *)
type rectangular_image = {
    north : Graphics.image;
    south : Graphics.image;
    east : Graphics.image;
    west : Graphics.image;
  };;

let rec save_rectangle x y dx dy =
  let x = min x (x + dx) in
  let y = min y (y + dy) in
  let dx = max 1 (abs dx) in
  let dy = max 1 (abs dy) in
  { south = Graphics.get_image x y dx 1;
    west = Graphics.get_image x y 1 dy;
    east = Graphics.get_image (x + dx) y 1 dy;
    north = Graphics.get_image x (y + dy) (succ dx) 1;
  };;

let rec restore_rectangle r x y dx dy =
  let x = min x (x + dx) in
  let y = min y (y + dy) in
  let dx = max 1 (abs dx) in
  let dy = max 1 (abs dy) in
  Graphics.draw_image r.south x y;
  Graphics.draw_image r.west x y;
  Graphics.draw_image r.east (x + dx) y;
  Graphics.draw_image r.north x (y + dy);;

let draw_rectangle x y dx dy =
  Graphics.moveto x y;
  Graphics.lineto (x + dx) y;
  Graphics.lineto (x + dx) (y + dy);
  Graphics.lineto x (y + dy);
  Graphics.lineto x y;;

let draw_line x y dx dy =
  Graphics.moveto x y;
  Graphics.lineto (x + dx) (y + dy);;

let draw_point x y =
  Graphics.draw_circle x y 3;;
  

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

(* *)
let editing =
  Options.flag false "-edit" "Start in edit mode";;

module H =
  struct
    type mode = Over | Click_down
    type link = {
      link : string;
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
	let draw_rect_with_line_width e x y w h =
          Graphics.fill_rect x y e h;
          Graphics.fill_rect x y w e;
          Graphics.fill_rect (x + w - e) y e h;
          Graphics.fill_rect x (y + h - e) w e
	in
	draw_rect_with_line_width e x y w h
      else Graphics.draw_rect x y w h

    let draw_anchor c e a =
      Graphics.set_color c;
      frame_rect e a.A.x a.A.y a.A.w a.A.h;
      Graphics.set_color !color

    let make_anchors tag all_draw =
      let make_anchor draw (x, y as orig) w h voff =
        let anchor = {tag = tag; draw = List.rev draw } in
        let e =
          match tag with
          | Href _ -> 0
          | Advi _ -> 0
          | Name _ -> 0 in
        let y' = y - voff - 1 in
        let h' = h + 2 in
        let a =
          let e = e + 1 in
          { A.x = x - e;
            A.y = (!size_y - y' - h') - e;
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
        | [] -> make_anchor draw orig w h voff
        | (x1, y1, g1 as d) :: rest ->
            if x1 + width g1 > x then
              split (d :: draw) orig
                (max w ((x1 - x) + width g1))
                (max h (height g1))
                (max voff (voffset g1)) rest
            else
              begin
                make_anchor (d :: draw) orig w h voff;
                start rest
              end
      and start = function
        | [] -> ()
        | (x, y, g as d) :: rest ->
            split [d] (x, y) (width g) (height g) (voffset g) rest in
      start all_draw

    let add anchor = make_anchors anchor.tag anchor.draw

    let area tag x y w h =
      let anchor = { tag = tag; draw = [] } in
      let a = { A.x = x; A.y = y; A.w = w; A.h = h; A.action = anchor} in
      anchors := A.add a !anchors

    let find x y = A.find x y !anchors

    let find_tag t = A.find_action (fun x -> x.tag = t) !anchors


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
          GraphicsY11.display_mode now;
          List.iter
            (function ima, act -> Graphics.draw_image ima act.A.x act.A.y) l;
          Graphics.draw_image ima act.A.x act.A.y;
          Busy.set
            (if !editing then Busy.Selection else Busy.Free);
          GraphicsY11.display_mode false
      | Screen (ima, act, all_anchors) ->
          GraphicsY11.display_mode true;
          anchors := all_anchors;
          Gs.flush ();
          (* long delay to be safe *)
          ignore (sleep_watch false false 0.1);
          Graphics.draw_image ima 0 0;
          Busy.set
            (if !editing then Busy.Selection else Busy.Free);
          GraphicsY11.flush ();
          GraphicsY11.display_mode false
      | Nil -> ()

    let emphasize fill c act =
      let ima = Graphics.get_image act.A.x act.A.y act.A.w act.A.h in
      Graphics.set_color c;
      GraphicsY11.display_mode true;
      if fill then Graphics.fill_rect act.A.x act.A.y act.A.w act.A.h
      else Graphics.draw_rect (act.A.x+1) (act.A.y+1) (act.A.w-2) (act.A.h-2);
      Graphics.set_color !color;
      push_bg_color c;
      List.iter (function x, y, g -> draw_glyph g x y) act.A.action.draw;
      pop_bg_color ();
      GraphicsY11.set_cursor GraphicsY11.Cursor_hand2;
      GraphicsY11.display_mode false;
      Rect (ima, act, [])

    let save_screen_exec act a =
      Gs.flush ();
      (* get image take the image from the backing store *)
      let ima = Graphics.get_image 0 0 !size_x !size_y in
      GraphicsY11.sync ();
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
      GraphicsY11.synchronize ();
      Launch.launch_embedded_apps ();
      Screen (ima, act, all_anchors)

    let light t =
      try
        match t with
        | Name n ->
            let t = Name (Misc.get_suffix "#" n) in
            emphasize true name_emphasize (find_tag t)
        | _ -> Nil
      with
      | Not_found | Misc.Match -> Nil

    let flashlight t =
      deemphasize false (light t)

    let emphasize_and_flash color act =
      let fill, color = 
        match act.A.action.tag with
        | Href s when  has_prefix "#/page." s -> false, rect_emphasize
        | _ -> true, color in
      let emph = emphasize fill color act in
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

module E =
  struct
    type direction = X | Y | XY | Z
    type info = { comm : string; name : string;
                  line : string; file : string; 
                  origin : float rect; unit : float;
                  move : direction; resize : direction; }
    type figure = { rect : int rect; info : info; }
    type action = Move of int * int | Resize of int * int

    let figures : figure list ref = ref []
    let screen = ref None
    let switch_edit_mode () =
      editing := not !editing;
      Busy.set
       (if !editing then Busy.Selection else Busy.Free)

    let clear () = figures := []; screen := None
        (*
           let save_screen cont =
           screen := Graphics.get_image 0 0 !size_x !size_y
           let restore_screen () = ()
         *)
        

    let add rect info =
      let r = { rect with y = !size_y - rect.y; } in
      figures := { rect = r; info = info} :: !figures;
      if !editing then
        begin
          Graphics.set_color Graphics.blue;
          draw_rectangle r.x r.y r.w r.h;
          let cv z = truncate (z *. info.unit) in
          let ox = cv info.origin.x in
          let oy = cv info.origin.y in
          let x0 = r.x - ox in
          let y0 = r.y - oy in
          Graphics.set_color Graphics.green;
          draw_line x0 y0 ox oy;
          draw_point x0 y0;
          Graphics.set_color !color;
        end


    let inside x y p  =
      let a = p.rect in
      (if a.w > 0 then a.x <= x && x <= a.x + a.w
      else  a.x + a.w <= x && x <= a.x) &&
      (if a.h > 0 then a.y <= y && y <= a.y + a.h
      else a.y + a.h <= y && y <= a.y)
    let find x y = List.find (inside x y) !figures

    let tostring p a =
      (* should memorize the origin *)
      let delta z dz =
        if dz = 0 then "*"
        else Printf.sprintf "%.4f" (z +. (float dz  /. p.info.unit)) in
      let origin = p.info.origin in
      let action, dx, dy =
        match a with
        | Move (dx, dy) ->
            "moveto", delta origin.x dx, delta origin.y (0-dy)
        | Resize (dx, dy) ->
            "resizeto", delta origin.w dx, delta origin.h (0-dy) in
      Printf.sprintf "<edit %s %s #%s @%s %s %s,%s>"
        p.info.comm p.info.name p.info.line p.info.file action dx dy

    let editing() = !editing

  end;;

(*** Clearing device ***)

module Symbol = Symbol.Make (Glyph);;

let cut s =
  (* print_string s; print_newline (); *)
  (* cut does not work yet *)
  GraphicsY11.cut s;
;;

let open_dev geom =
  if !opened then Graphics.close_graph ();
  Graphics.open_graph geom;

  (* We disable Graphics's event retrieving *)
  GraphicsY11.init ();
  Timeout.init ();
  (* Fill the event queue *)
  let rec f () =
    Timeout.add 0.25 (fun () ->
      GraphicsY11.retrieve_events (); ignore (f ()))
  in
  ignore (f ());

  size_x := Graphics.size_x ();
  size_y := Graphics.size_y ();
  xmin := 0; xmax := !size_x;
  ymin := 0; ymax := !size_y;
  Graphics.remember_mode true;
  GraphicsY11.display_mode !Options.global_display_mode;
  Graphics.set_window_title !title;
  color := !default_fgcolor;
  opened := true;
  !size_x, !size_y;;

let close_dev () =
  if !opened then begin
    Embed.kill_all_embedded_apps ();
    Graphics.close_graph ();
  end;
  opened := false;;

let clear_dev () =
  if not !opened then failwith "Grdev.clear_dev: no window";
  Embed.kill_ephemeral_apps ();
  Launch.unmap_persistent_apps ();
  Misc.debug_stop "subwindows of persistent apps unmapped";
  GraphicsY11.display_mode !Options.global_display_mode;
  Graphics.clear_graph ();
  Misc.debug_stop "graphics cleared";
  H.clear ();
  E.clear ();
  bg_colors := [];
  background_colors := [];
  Symbol.clear ();
  (* update graphics size information *)
  size_x := Graphics.size_x ();
  size_y := Graphics.size_y ();
  xmin := 0; xmax := !size_x;
  ymin := 0; ymax := !size_y;
  (* draw background *)
  draw_bkgd ();;

(*** Events ***)

type status = GraphicsY11.status = {
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
   | Edit of E.figure * E.action
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
  GraphicsY11.Button_down;
  GraphicsY11.Button_up;
  GraphicsY11.Mouse_motion;
  GraphicsY11.Key_pressed;
];;

let button_up_motion = [
  GraphicsY11.Button_up;
  GraphicsY11.Mouse_motion;
];;

let button_up = [
  GraphicsY11.Button_up;
];;

let events = ref [];;
let push_back_event ev =
  if List.length !events > 1 then Misc.warning "STACK";
  events := ev :: !events;;
let event_waiting () = !events <> [];;
let rec pop_event () =
  match !events with
  | [] -> assert false
  | h :: t -> events := t; h;;

let push_back_key_event c m =
    push_back_event {
      modifiers = m;
      keypressed = true; key = c;
      mouse_x = 0; mouse_y = 0;
      button = false;
    };;

Misc.forward_push_back_key_event := push_back_key_event;;

let mouse_x = ref 0;;
let mouse_y = ref 0;;
let button = ref false;;

let reposition ~x ~y ~w ~h =
  Gs.flush ();
  Gs.kill ();
  GraphicsY11.reposition x y w h;
  let x = Graphics.size_x ()
  and y = Graphics.size_y () in
  size_x := x;
  size_y := y;
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


let rec wait_signal_event events =
    match resized (), !usr1_status, event_waiting () with
    | Some (x, y), _, _ -> Final (Resized (x, y))
    | _, true, _ -> clear_usr1 (); Final (Refreshed)
    | _, _, true -> Raw (pop_event ())
    | _, _, _ ->
       try
         waiting := true;
         let ev = GraphicsY11.wait_next_event events in
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
        let dx' = e.GraphicsY11.mouse_x - x in
        let dy' = e.GraphicsY11.mouse_y - y in
        if e.GraphicsY11.button then select dx' dy'
        else Final (Region (x, y, dx', 0 - dy'))
    | x -> x in
  set_color !default_fgcolor;
  GraphicsY11.display_mode true;
  Busy.set Busy.Selection;
  let restore () =
    GraphicsY11.display_mode false;
    set_color !color;
    Busy.set
      (if !editing then Busy.Selection else Busy.Free) in
  try
    let e = select 0 0 in
    restore ();
    e
  with exn -> restore (); raise exn;;

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
        let x' = e.GraphicsY11.mouse_x in
        let y' = e.GraphicsY11.mouse_y in
        let r' = Symbol.new_region r x' (!size_y - y') in
        Symbol.iter_regions (draw_color true) (draw_color false) r r';
        if e.GraphicsY11.button then select r'
        else
          let m = GraphicsY11.get_modifiers () in
          if m land GraphicsY11.shift = 0 then
            begin
              Symbol.iter_region (draw_color true) r';
              Final Nil
            end
          else
            begin
            Final (Selection (Symbol.region_to_ascii r'))
                end;
    | x -> x in
  let color = !color in
  GraphicsY11.synchronize ();
  Graphics.remember_mode false;
  GraphicsY11.display_mode true;
  Busy.temp_set Busy.Selection;
  let restore () =
    Graphics.remember_mode true;
    GraphicsY11.display_mode false;
    set_color color;
    Busy.restore_cursor() in
  try
    let e =
      if m land GraphicsY11.button2 = 0 then
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

type trans =
    Move_xy
  | Move_x
  | Move_y
  | Resize_xy
  | Resize_x
  | Resize_y

let move rect dx dy = { rect with x = rect.x + dx; y = rect.y + dy };;
let move_x rect dx dy = { rect with x = rect.x + dx };;
let move_y rect dx dy = { rect with y = rect.y + dy };;
let resize rect dx dy = { rect with w = rect.w + dx; h = rect.h + dy };;
let resize_x rect dx dy = { rect with w = rect.w + dx };;
let resize_y rect dx dy = { rect with h = rect.h + dy };;

let trans  = function
  | Move_xy -> move
  | Move_x -> move_x
  | Move_y -> move_y
  | Resize_xy -> resize
  | Resize_x -> resize_x
  | Resize_y -> resize_y
;;

let trans_cursor = function
  | Move_xy -> Busy.Move
  | Move_x -> Busy.Move
  | Move_y -> Busy.Move
  | Resize_xy -> Busy.Resize
  | Resize_x -> Busy.Resize_x
  | Resize_y -> Busy.Resize_y
;;


let filter trans event dx dy =
  let r = trans { x = 0; y = 0; w = 0; h = 0; } dx dy in
  event (r.x + r.w) (r.y + r.h);;

let wait_move_button_up rect trans_type event x y =
  let trans = trans trans_type in
  let w = rect.w and h = rect.h in
  let rec move dx dy =
    let r = trans rect dx dy in 
    let buf = save_rectangle r.x r.y r.w r.h in
    draw_rectangle r.x r.y r.w r.h;
    let ev = wait_signal_event button_up_motion in
    restore_rectangle buf r.x r.y r.w r.h;
    match ev with
    | Raw e ->
        let dx' = e.GraphicsY11.mouse_x - x in
        let dy' = e.GraphicsY11.mouse_y - y in
        if e.GraphicsY11.button then move dx' dy'
        else Final (filter trans event dx' (0 - dy'))
    | z -> z in
  let color = !color in
  set_color !default_fgcolor;
  Busy.temp_set (trans_cursor trans_type);
  GraphicsY11.display_mode true;
  let restore () =
    GraphicsY11.display_mode false;
    set_color color;
    Busy.restore_cursor () in
  try let e = move 0 0 in restore (); e
  with exn -> restore (); raise exn;;

let near x x' = abs (x - x') < !size_x / 4;;
let close x x' = abs (x - x') < !size_x / 10;;

let click_area near x y =
  if near x 0 then
    if near y 0 then Bottom_left else
    if near y !size_y then Top_left
    else Middle else
  if near x !size_x then
    if near y 0 then Bottom_right else
    if near y !size_y then Top_right
    else Middle
  else Middle;;

let pressed m b = m land b <> 0;;
let released m b = m land b = 0;;

module G = GraphicsY11
let button m =
  if pressed m G.button1 then Button1
  else if pressed m G.button3 then Button3
  else Button2;;

let wait_button_up m x y =
    let wait_position () = 
    match wait_signal_event button_up with
    | Raw e ->
        if !editing || pressed m G.shift
        then
          begin match click_area close x y with
            Middle -> Final (Position (x, !size_y - y))
          | c -> Final (Click (c, button m, x, !size_y - y))
          end
        else Final (Click (click_area near x y, button m, x, !size_y - y))
    | x -> x
    in
  if !editing && pressed m G.button1 then
    wait_position()
  else if !editing || pressed m G.control then
    begin
      try 
        let p = E.find x y in
        let rect = p.E.rect in
        let info = p.E.info in
        if pressed m G.button2 && info.E.move <> E.Z then
          let event dx dy = Edit (p, E.Move (dx, dy)) in
          let action = match info.E.move with
            E.X -> Move_x | E.Y -> Move_y | _ -> Move_xy in
          wait_move_button_up rect action event x y 
        else if pressed m G.button3 && info.E.resize <> E.Z then
          let event dx dy = Edit (p, E.Resize (dx, dy)) in
          let action = match info.E.resize with
            E.X -> Resize_x | E.Y -> Resize_y | _ -> Resize_xy in
          wait_move_button_up rect action event x y 
        else Final Nil
      with
        Not_found ->
          if pressed m G.control then
            let event dx dy = Move (dx, dy) in
            wait_move_button_up !bbox Move_xy event x y 
          else
            wait_position()
    end
  else if pressed m G.shift && released m G.button1 then
    wait_select_button_up m x y
  else
    wait_position ()
;;

let wait_event () =
  (* We reached a pause. Now we can reset the sleep break *)
  clear_sleep ();
  let rec event emph b =
    let send ev = H.deemphasize true emph; ev in
    let rescan () = H.deemphasize true emph; event H.Nil false in
    match wait_signal_event all_events with
    | Final e -> send e
    | Raw ev ->
        if ev.GraphicsY11.keypressed then send (Key ev.GraphicsY11.key) else
        try match H.find ev.mouse_x ev.mouse_y with
        | {A.action = {H.tag = H.Href h; H.draw = d}} as act ->
            if ev.button then
              let ev' = GraphicsY11.wait_next_event button_up in
              send (Href h) else
              if H.up_to_date act emph then event emph b else begin
                H.deemphasize true emph;
                event (H.emphasize_and_flash href_emphasize act) b end
        | {A.action =
           {H.tag = H.Advi {H.link = s; H.action = a; H.mode = H.Over};
            H.draw = d}} as act ->
              if H.up_to_date act emph then event emph b else begin
                H.deemphasize true emph;
                event (H.save_screen_exec act a) b end
        | {A.action =
           {H.tag = H.Advi {H.link = s; H.action = a; H.mode = H.Click_down};
            H.draw = d}} as act ->
              if ev.button && not b then begin
                H.deemphasize true emph;
                event (H.save_screen_exec act a) true end else
                if ev.button then event emph b else
                if H.up_to_date act emph then event emph b else begin
                  H.deemphasize true emph;
                  event (H.emphasize_and_flash href_emphasize act) b end
        | _ -> rescan ()
        with Not_found ->
          if ev.button then
            let m = GraphicsY11.get_modifiers () in
            match wait_button_up m ev.mouse_x ev.mouse_y with
            | Final (Region (x, y, dx, dy) as e) -> send e
            | Final (Selection s as e) -> send e
            | Final (Position (x, y) as e) -> send e
            | Final (Move (dx, dy) as e) -> send e
            | Final (Click (_, _, _, _) as e) -> send e
            | Final (Edit (_, _) as e) -> send e
            | Final Nil -> send Nil
            | Final
                (Resized (_, _) | Refreshed |
                 Key _ | Href _ | Advi (_, _) as e) ->
                push_back_event ev;
                send e
            | Raw _ -> rescan ()
          else
            let m = GraphicsY11.get_modifiers () in
            if m land GraphicsY11.shift <> 0
            then Busy.temp_set Busy.Selection
            else Busy.restore_cursor ();
            rescan () in
  event H.Nil false;;

(* To be changed *)
exception GS = Gs.Terminated;;

let resized () =
  Graphics.size_x () <> !size_x || Graphics.size_y () <> !size_y;;

let continue () =
  if resized () || GraphicsY11.key_pressed () (*  || !usr1_status *)
  then begin Gs.flush (); raise Stop end;;

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

let embed_app command app_mode app_name width_pixel height_pixel x y =
  Embed.embed_app
   command app_mode app_name width_pixel height_pixel x (!size_y - y);;

let wait_button_up () =
  if GraphicsY11.button_down () then
    ignore (GraphicsY11.wait_next_event [GraphicsY11.Button_up]);;


