(***********************************************************************)
(*                                                                     *)
(*                           Objective Caml                            *)
(*                                                                     *)
(*                 projet Cristal, INRIA Rocquencourt                  *)
(*                                                                     *)
(*  Copyright 2001 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the GNU Library General Public License.         *)
(*                                                                     *)
(***********************************************************************)

(* $Id$ *)

(* Module [GraphicsX11]: additional graphics primitives for the X Windows system *)

type window_id = string

val map_subwindow : window_id -> unit
val unmap_subwindow : window_id -> unit

external bstore_id : unit -> window_id = "gr_bstore"
        (* return the X pixmap of the bstore window as an integer *)



external flush : unit -> unit = "gr_flush"
        (* flush the content of the backing store *)

external bsize_x : unit -> int = "gr_bsize_x"
external bsize_y : unit -> int = "gr_bsize_y"
        (* Idem, but return the size of the backing store. *)
external screen_x : unit -> int = "gr_screen_x"
external screen_y : unit -> int = "gr_screen_y"
        (* Return the size of the screen. *)

external set_named_atom_property : string -> string -> unit
    = "gr_set_named_atom_property"
        (* make_atom_property ATOM STRING define an X atom ATOM with
           property STRING *)

(* setting cursor *)
(* check by fd -fn cursor *)
type cursor = 
  | Cursor_id of int
  | Cursor_X_cursor
  | Cursor_arrow
  | Cursor_based_arrow_down
  | Cursor_based_arrow_up
  | Cursor_boat
  | Cursor_bogosity
  | Cursor_bottom_left_corner
  | Cursor_bottom_right_corner
  | Cursor_bottom_side
  | Cursor_bottom_tee
  | Cursor_box_spiral
  | Cursor_center_ptr
  | Cursor_circle
  | Cursor_clock
  | Cursor_coffee_mug
  | Cursor_cross
  | Cursor_cross_reverse
  | Cursor_crosshair
  | Cursor_diamond_cross
  | Cursor_dot
  | Cursor_dotbox
  | Cursor_double_arrow
  | Cursor_draft_large
  | Cursor_draft_small
  | Cursor_draped_box
  | Cursor_exchange
  | Cursor_fleur
  | Cursor_gobbler
  | Cursor_gumby
  | Cursor_hand1
  | Cursor_hand2
  | Cursor_heart
  | Cursor_icon
  | Cursor_iron_cross
  | Cursor_left_ptr
  | Cursor_left_side
  | Cursor_left_tee
  | Cursor_leftbutton
  | Cursor_ll_angle
  | Cursor_lr_angle
  | Cursor_man
  | Cursor_middlebutton
  | Cursor_mouse
  | Cursor_pencil
  | Cursor_pirate
  | Cursor_plus
  | Cursor_question_arrow
  | Cursor_right_ptr
  | Cursor_right_side
  | Cursor_right_tee
  | Cursor_rightbutton
  | Cursor_rtl_logo
  | Cursor_sailboat
  | Cursor_sb_down_arrow
  | Cursor_sb_h_double_arrow
  | Cursor_sb_left_arrow
  | Cursor_sb_right_arrow
  | Cursor_sb_up_arrow
  | Cursor_sb_v_double_arrow
  | Cursor_shuttle
  | Cursor_sizing
  | Cursor_spider
  | Cursor_spraycan
  | Cursor_star
  | Cursor_target
  | Cursor_tcross
  | Cursor_top_left_arrow
  | Cursor_top_left_corner
  | Cursor_top_right_corner
  | Cursor_top_side
  | Cursor_top_tee
  | Cursor_trek
  | Cursor_ul_angle
  | Cursor_umbrella
  | Cursor_ur_angle
  | Cursor_watch
  | Cursor_xterm

val set_cursor : cursor -> unit 
        (* set the cursor for the graphics window *)

val unset_cursor : unit -> unit 
        (* unset the cursor (use the parent's cursor) *)

external get_geometry : unit -> int * int * int * int = "gr_get_geometry"
        (* returns width, height, x, y of the graphics window *)

external get_modifiers : unit -> int = "gr_get_modifiers"
        (* returns the list of modifiers as an integer *)

val button1 : int
val button2 : int
val button3 : int
val shift : int
val control : int
val mod1 : int
        (* mask for modifiers *)
