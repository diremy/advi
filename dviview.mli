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
  val get_glyph  : glyph -> Glyph.t

  val open_dev : string -> unit
  val close_dev : unit -> unit
  val clear_dev : unit -> unit
  val set_bbox : (int * int * int * int) option -> unit

  type color = int

  val set_color : int -> unit
  val draw_glyph : glyph -> int -> int -> unit
  val fill_rect : int -> int -> int -> int -> unit

  val draw_path: (int * int) array -> pensize:int -> unit
  val fill_path: (int * int) array -> shade:float -> unit
  val draw_arc: x:int -> y:int -> rx:int -> ry:int -> 
                start:int -> stop:int -> pensize:int -> unit
  val fill_arc: x:int -> y:int -> rx:int -> ry:int -> 
                start:int -> stop:int -> shade:float -> unit

  val set_epstransparent : bool -> unit
  val set_alpha : float -> unit
  type blend =
    | Normal | Multiply | Screen | Overlay (* | SoftLight | HardLight *)
    | ColorDodge | ColorBurn | Darken | Lighten | Difference 
    | Exclusion (* | Luminosity | Color | Saturation | Hue *)
  val set_blend : blend -> unit
  val draw_ps : string -> (int * int * int * int) -> (int * int) -> int -> int -> unit
  val clean_ps_cache : unit -> unit
  val sleep : float -> unit

  val set_transition : Transitions.t -> unit

  type app_type = Sticky | Persistent | Embedded
  val embed_app : string -> app_type -> string -> int -> int -> int -> int -> unit
  val kill_embedded_apps : unit -> unit 
  val kill_embedded_app : string -> unit 

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
    | Click of area * button * int * int
    | Nil

  val wait_event : unit -> event

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
      end

  exception Stop
  exception GS
  val continue : unit -> unit
  val current_pos : unit -> int * int
  val newpage : string list -> int -> float -> int -> int -> unit
  val exec_ps : string -> int -> int -> unit
  val add_headers : string list -> unit
  val synchronize : unit -> unit
  type busy = Free | Busy | Pause | Disk
  val set_busy : busy -> unit;;
  val set_title : string -> unit
end ;;

module type DVIVIEW = sig
  open Dimension
  val set_autoresize : bool -> unit
  val set_geometry : string -> unit
  val set_crop : bool -> unit
  val set_hmargin : dimen -> unit
  val set_vmargin : dimen -> unit
  exception Error of string

  val main_loop : string -> unit
end ;;

module Make(Dev : DEVICE) : DVIVIEW ;;
