(***********************************************************************)
(*                                                                     *)
(*                           Objective Caml                            *)
(*                                                                     *)
(*    Pierre Weis and Jun Furuse, projet Cristal, INRIA Rocquencourt   *)
(*                                                                     *)
(*  Copyright 2001 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the GNU Library General Public License.         *)
(*                                                                     *)
(***********************************************************************)

(* $Id$ *)


(* Re-imported from graphicsX11 *)

(* Module [GraphicsX11]: additional graphics primitives for the X Windows system *)

type window_id = GraphicsX11.window_id

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

external bstore_id : unit -> window_id = "gr_bstore"
        (* return the X pixmap of the bstore window as an integer *)

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

let glyph_of_cursor = function
  | Cursor_id x -> x / 2 * 2 (* must be even *) 
  | Cursor_X_cursor -> 0
  | Cursor_arrow -> 2
  | Cursor_based_arrow_down -> 4
  | Cursor_based_arrow_up -> 6
  | Cursor_boat -> 8
  | Cursor_bogosity -> 10
  | Cursor_bottom_left_corner -> 12
  | Cursor_bottom_right_corner -> 14
  | Cursor_bottom_side -> 16
  | Cursor_bottom_tee -> 18
  | Cursor_box_spiral -> 20
  | Cursor_center_ptr -> 22
  | Cursor_circle -> 24
  | Cursor_clock -> 26
  | Cursor_coffee_mug -> 28
  | Cursor_cross -> 30
  | Cursor_cross_reverse -> 32
  | Cursor_crosshair -> 34
  | Cursor_diamond_cross -> 36
  | Cursor_dot -> 38
  | Cursor_dotbox -> 40
  | Cursor_double_arrow -> 42
  | Cursor_draft_large -> 44
  | Cursor_draft_small -> 46
  | Cursor_draped_box -> 48
  | Cursor_exchange -> 50
  | Cursor_fleur -> 52
  | Cursor_gobbler -> 54
  | Cursor_gumby -> 56
  | Cursor_hand1 -> 58
  | Cursor_hand2 -> 60
  | Cursor_heart -> 62
  | Cursor_icon -> 64
  | Cursor_iron_cross -> 66
  | Cursor_left_ptr -> 68
  | Cursor_left_side -> 70
  | Cursor_left_tee -> 72
  | Cursor_leftbutton -> 74
  | Cursor_ll_angle -> 76
  | Cursor_lr_angle -> 78
  | Cursor_man -> 80
  | Cursor_middlebutton -> 82
  | Cursor_mouse -> 84
  | Cursor_pencil -> 86
  | Cursor_pirate -> 88
  | Cursor_plus -> 90
  | Cursor_question_arrow -> 92
  | Cursor_right_ptr -> 94
  | Cursor_right_side -> 96
  | Cursor_right_tee -> 98
  | Cursor_rightbutton -> 100
  | Cursor_rtl_logo -> 102
  | Cursor_sailboat -> 104
  | Cursor_sb_down_arrow -> 106
  | Cursor_sb_h_double_arrow -> 108
  | Cursor_sb_left_arrow -> 110
  | Cursor_sb_right_arrow -> 112
  | Cursor_sb_up_arrow -> 114
  | Cursor_sb_v_double_arrow -> 116
  | Cursor_shuttle -> 118
  | Cursor_sizing -> 120
  | Cursor_spider -> 122
  | Cursor_spraycan -> 124
  | Cursor_star -> 126
  | Cursor_target -> 128
  | Cursor_tcross -> 130
  | Cursor_top_left_arrow -> 132
  | Cursor_top_left_corner -> 134
  | Cursor_top_right_corner -> 136
  | Cursor_top_side -> 138
  | Cursor_top_tee -> 140
  | Cursor_trek -> 142
  | Cursor_ul_angle -> 144
  | Cursor_umbrella -> 146
  | Cursor_ur_angle -> 148
  | Cursor_watch -> 150
  | Cursor_xterm -> 152

external set_cursor : int -> unit = "gr_set_cursor"
external unset_cursor : unit -> unit = "gr_unset_cursor"

let set_cursor c =
  set_cursor (glyph_of_cursor c)
;;
