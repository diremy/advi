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


(* Private glyphs *)

type glyph;;

val make_glyph : Glyph.t -> glyph;;
val get_glyph  : glyph -> Glyph.t;;
val draw_glyph : glyph -> int -> int -> unit;;

module Symbol :
    sig
      type fontname = string
      type fontratio = float
      type g =
          { fontname : string;
            fontratio : float;
            glyph : glyph;
          } 
      type symbol =
          Glyph of g
        | Space of int * int
        | Rule of int * int
        | Line of int * string option
      type element =
          { color   : int; 
            locx    : int; 
            locy    : int;
            code    : int;
            symbol : symbol }
      type set
      val voffset : element -> int
      val hoffset : element -> int
      val height : element -> int
      val width : element -> int
      val clear : unit -> unit
      val add : int -> int -> int -> int -> symbol -> unit
      val to_ascii   : set -> string
      val to_escaped : set -> string
      val inzone : int -> int -> int -> int -> set
      val intime : int -> int -> int -> int -> set
      val iter : (element -> unit) -> set -> unit
      val lines : int -> int -> (element * int * int * string * string) option
    end;;

(* Device configuration *)

val open_dev : string -> unit;;
val close_dev : unit -> unit;;
val clear_dev : unit -> unit;;
val set_bbox : (int * int * int * int) option -> unit;;

(* Drawing *)

type color = int;;
val fgcolor : unit -> color;;

val set_color : int -> unit;;
val push_bg_color : int -> unit;;
val pop_bg_color : unit -> unit;;
val fill_rect : int -> int -> int -> int -> unit;;

val draw_path : (int * int) array -> pensize:int -> unit;;
val fill_path : (int * int) array -> shade:float -> unit;;
val draw_arc :
  x:int -> y:int -> rx:int -> ry:int ->
  start:int -> stop:int -> pensize:int -> unit;;
val fill_arc :
  x:int -> y:int -> rx:int -> ry:int -> 
  start:int -> stop:int -> shade:float -> unit;;

(* Alpha blending *)
val set_alpha : float -> unit;; 
val set_epstransparent : bool -> unit;; 

type blend =
  | Normal | Multiply | Screen | Overlay (* | SoftLight | HardLight *)
  | ColorDodge | ColorBurn | Darken | Lighten | Difference 
  | Exclusion (* | Luminosity | Color | Saturation | Hue *)
;;

val set_blend : blend -> unit;;
val draw_ps :
  string -> (int * int * int * int) -> (int * int) -> int -> int -> unit;;
val clean_ps_cache : unit -> unit;;
val sleep : float -> bool;; (* true= interrupted, false= fully performed *)

(* Background information *)

type bkgd_prefs = {
  mutable bgcolor : int;
  mutable bgimg : string option;
  mutable bgratio : Drawimage.ratiopts;
  mutable bgwhitetrans : bool;
  mutable bgalpha : float;
  mutable bgblend : blend;
};;

val blit_bkgd_data : bkgd_prefs -> bkgd_prefs -> unit;;
val copy_of_bkgd_data : unit -> bkgd_prefs;;
val default_bkgd_data : unit -> bkgd_prefs;;
val bkgd_data : bkgd_prefs;;

type bgoption =
   | BgColor of color
   | BgImg of string
   | BgAlpha of float
   | BgBlend of blend;;

val set_bg_options : bgoption list -> unit;;

(* Embedded applications *)
type app_mode =
   | Sticky
      (* A [Sticky] application is launched once and only once.
         They are never killed when a new slide is visualized.
         As soon as launched, a [Sticky] application remains visible
         throughout the show.
         To do: add a tag name for each [Sticky] application, in order
         to kill it on demand. *)
   | Persistent
      (* A [Sticky] application that is not visible out of the slide
         that launched it. A [Persistent] application is launched once
         and only once and keeps running throughout the show.
         A [Persistent] application must be embeddable, since its
         window must be mapped and unmapped. *)
   | Ephemeral
      (* An [Ephemeral] is an application that is launched when the
         page it appears in is visualised. It is killed when going
         to another slide. If the page is visible again, the
         application will be launched again as well. *)
;;

val embed_app :
  string -> app_mode -> string -> int -> int -> int -> int -> unit;;
val kill_embedded_app : string -> unit;;
val kill_all_embedded_apps : unit -> unit;;

(* Events *)

type status = {
    mouse_x : int;
    mouse_y : int;
    button : bool;
    keypressed : bool;
    key : char;
    modifiers : int;
  };;

type area = Bottom_right | Bottom_left | Top_right | Top_left | Middle;;
type button = Button1 | Button2 | Button3;;
type event =
    Resized of int * int
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

val wait_event : unit -> event;;

module H :
    sig
      type tag =
        | Name of string 
        | Href of string
        | Advi of string * (unit -> unit)

      type anchor = {
          tag : tag;
          draw : (int * int * glyph) list
        } 

      val add : anchor -> unit
      val flashlight : tag -> unit

    end;;

exception Stop;;
exception GS;;
val continue : unit -> unit;;
val reposition : x:int -> y:int -> w:int -> h:int -> int * int;;
val exec_ps : string -> int -> int -> unit;;
val newpage : string list -> int -> float -> int -> int -> unit;;
val add_headers : string list -> unit;;
val current_pos : unit -> int * int;;
val synchronize : unit -> unit;; 

type busy = Free | Busy | Pause | Disk;;

val set_busy : busy -> unit;;

val set_transition : Transitions.t -> unit;;

val transbox_save : int -> int -> int -> int -> unit;;
val transbox_go : Transitions.t -> unit;;

val set_title : string -> unit;;
val cut : string -> unit;;

val wait_button_down : unit -> unit;;
