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
(*  Jun Furuse, Didier R�my and Pierre Weis.                           *)
(*  Contributions by Roberto Di Cosmo, Didier Le Botlan,               *)
(*  Xavier Leroy, and Alan Schmitt.                                    *)
(*                                                                     *)
(*  Based on Mldvi by Alexandre Miquel.                                *)
(***********************************************************************)

(* The ``scratching'' facility for Active-DVI: you can interactively
   annotate your slides by ``scratching'' on them. *)

module G = Graphics;;

module Graphics = GraphicsY11;;

open Graphics;;

let get_color s =
    try int_of_string s with
    | Failure "int_of_string" ->
        Misc.warning (Printf.sprintf "Cannot understand %s as a color" s);
        G.black;;

let scratch_line_color = ref G.red;;
let set_scratch_line_color s = scratch_line_color := get_color s;;
Options.add "-scratch-line-color"
 (Arg.String set_scratch_line_color)
 "INT\tSet the color of the pen used when scratching slides (default red)"
;;

let scratch_line_width = ref 2;;
let set_scratch_line_width i = scratch_line_width := i;;
Options.add "-scratch-line-width"
 (Arg.Int set_scratch_line_width)
 (Printf.sprintf 
   "INT\tSet the width of the pen used when scratching slides (default %i)"
   !scratch_line_width)
;;

let scratch_font_color = ref G.red;;
let set_scratch_font_color s = scratch_font_color := get_color s;;
Options.add "-scratch-font-color"
 (Arg.String set_scratch_font_color)
 "STRING\tSet the color of the font used when scratching slides (default red)"
;;

let scratch_write_font =
  ref "-adobe-times-bold-r-normal--18-180-75-75-p-99-iso8859-1";;
let set_scratch_font s = scratch_write_font := s;;
Options.add "-scratch-font"
 (Arg.String set_scratch_font)
 "STRING\tSet the font used when scratching slides (default times bold)"
;;

let rec scratch_write_char c x y =
   G.moveto x y;
   let tx, ty = G.text_size (String.make 1 c) in
   G.draw_char c;
   scratch_write (x + tx) y

and scratch_write x y =
   match Graphics.wait_next_event [Button_down; Key_pressed] with
   | {mouse_x = nx; mouse_y = ny; button = btn;
      keypressed = kp; key = c} ->
       if btn then () else
       if kp then
        if c <> '' then scratch_write_char c x y
;;

let do_write () =
 G.set_color !scratch_font_color;
 G.set_font !scratch_write_font;
 let cursor = get_cursor () in
 set_cursor Cursor_pencil;
 try
   match Graphics.wait_next_event [Button_down; Key_pressed] with
   | {mouse_x = x; mouse_y = y; button = btn; key = c} ->
       if btn
       then scratch_write x y
       else scratch_write_char c x y;
       set_cursor cursor
 with x -> set_cursor cursor; raise x
;;

let write () = only_on_screen do_write ();;

let rec scratch_lineto x y =
  if y > 0 then G.lineto x y;
  scratch_draw x y

and scratch_draw x y =
   match Graphics.wait_next_event [Mouse_motion; Button_up] with
   | {mouse_x = nx; mouse_y = ny; button = btn;
      keypressed = kp; key = c} ->
       if nx <> x || ny <> y then scratch_lineto nx ny else
       if btn then scratch_draw nx ny
;;

let do_draw () =
 G.set_color !scratch_line_color;
 G.set_line_width !scratch_line_width;
 let cursor = get_cursor () in
 set_cursor Cursor_pencil;
 try
   match Graphics.wait_next_event [Button_down] with
   | {mouse_x = x; mouse_y = y} ->
       G.moveto x y;
       scratch_draw x y;
       set_cursor cursor
 with x -> set_cursor cursor; raise x
;;

let draw () = only_on_screen do_draw ();;

