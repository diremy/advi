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

(* $Id$ *)

(* The ``scratching'' facility for Active-DVI: you can interactively
   annotate your slides by ``scratching'' on them. *)

module G = Graphics;;

module Graphics = GraphicsY11;;

open Graphics;;

let scratch_line_color = ref G.red;;
let set_scratch_line_color s = scratch_line_color := Dvicolor.parse_color s;;
Options.add "-scratch-line-color"
 (Arg.String set_scratch_line_color)
 "INT\tSet the color of the pen used when scratching slides (default red)"
;;
let incr_r_scratch_line_color () =
 scratch_line_color := !scratch_line_color + (10 lsl 16);;

let incr_g_scratch_line_color () =
 scratch_line_color := !scratch_line_color + (10 lsl 8);;

let incr_b_scratch_line_color () =
 scratch_line_color := !scratch_line_color + 10;;

let scratch_line_width = ref 2;;
let set_scratch_line_width i = scratch_line_width := i;;
let incr_scratch_line_width () =
  incr scratch_line_width;;
let decr_scratch_line_width () =
  if !scratch_line_width > 1 then decr scratch_line_width;;
Options.add "-scratch-line-width"
 (Arg.Int set_scratch_line_width)
 (Printf.sprintf 
   "INT\tSet the width of the pen used when scratching slides (default %i)"
   !scratch_line_width)
;;

let scratch_font_color = ref G.red;;
let set_scratch_font_color s = scratch_font_color := Dvicolor.parse_color s;;
Options.add "-scratch-font-color"
 (Arg.String set_scratch_font_color)
 "STRING\tSet the color of the font used when scratching slides (default red)"
;;

let scratch_font =
  ref "-adobe-times-bold-r-normal--18-180-75-75-p-99-iso8859-1";;
let set_scratch_font s = scratch_font := s;;
Options.add "-scratch-font"
 (Arg.String set_scratch_font)
 "STRING\tSet the font used when scratching slides (default times bold)"
;;

let end_write () =
  raise Exit;;

let rec scratch_write_char =
  let prev_xs = ref [] in
  let prev_size_y = ref 0 in
  fun c x y ->
   G.moveto x y;
   if c = '' then begin
     (* prerr_endline "backspace"; *)
     let px =
      match !prev_xs with
      | [] -> 0
      | px :: xs -> prev_xs := xs; px in
     let py = y in
     let ty = !prev_size_y in
     G.set_color G.background;
     G.fill_rect (px - 1) py (x - px + 2) ty;
     G.set_color !scratch_font_color;
     scratch_write px py
   end else begin
     let tx, ty = G.text_size (String.make 1 c) in
     prev_xs := x :: !prev_xs;
     prev_size_y := max ty !prev_size_y;
     G.draw_char c;
     scratch_write (x + tx) y end

and scratch_write x y =
   match Graphics.wait_next_event [Button_down; Key_pressed] with
   | {mouse_x = nx; mouse_y = ny; button = btn;
      keypressed = kp; key = c} ->
       if kp then
        if c = '' then end_write ()
        else scratch_write_char c x y else
       if btn then scratch_write nx ny else
       scratch_write nx ny
;;

let save_excursion curs col f =

  let cautious_set_font fnt =
    try Graphics.set_font fnt with
    | G.Graphic_failure s -> Misc.warning s in

  let color = Graphics.get_color () in
  let line_width = Graphics.get_line_width () in
  let cursor = Graphics.get_cursor () in
  let font = Graphics.get_font () in

  let restore () =
    G.set_color color;
    Graphics.set_line_width line_width;
    Graphics.set_font font;
    set_cursor cursor in

  G.set_color col;
  Graphics.set_line_width !scratch_line_width;
  cautious_set_font !scratch_font;
  set_cursor curs;

  try f (); restore ()
  with x -> restore (); if x <> Exit then raise x
;;

let rec wait_button_pressed () =
  match Graphics.wait_next_event [Button_down; Key_pressed] with
  | {mouse_x = x; mouse_y = y; button = btn; keypressed = kp; key = c} ->
      if kp then
        begin match c with
        | '' -> end_write ()
        | _ -> wait_button_pressed ()
        end else
      if not btn then wait_button_pressed ();;

let enter_write () =
  wait_button_pressed ();
  let x, y = G.mouse_pos () in
  G.moveto x y;
  scratch_write x y;;

let do_write () = save_excursion Cursor_pencil !scratch_font_color enter_write;;

let write () = only_on_screen do_write ();;

let end_draw () =
  raise Exit;;

let rec scratch_lineto btn x y =
  if y > 0 then G.lineto x y

and scratch_draw btn x y =
prerr_endline (Printf.sprintf "btn = %B" btn);
   match Graphics.wait_next_event
     [Mouse_motion; Button_down; Key_pressed] with
   | {mouse_x = nx; mouse_y = ny; button = nbtn;
      keypressed = kp; key = c} ->
prerr_endline (if nbtn then "nbtn=down" else "nbtn=up"); Pervasives.flush stderr;
      if kp then begin
        (match c with
         | '' -> end_draw ()
         | '>' -> incr_scratch_line_width ()
         | '<' -> decr_scratch_line_width ()
         | 'R' -> incr_r_scratch_line_color ()
         | 'G' -> incr_g_scratch_line_color ()
         | 'B' -> incr_b_scratch_line_color ()
         | 'b' -> set_scratch_line_color "blue"
         | 'g' -> set_scratch_line_color "green"
         | 'w' -> set_scratch_line_color "white"
         | 'c' -> set_scratch_line_color "cyan"
         | 'm' -> set_scratch_line_color "magenta"
         | 'r' -> set_scratch_line_color "red"
         | 'y' -> set_scratch_line_color "yellow"
         | 'k' -> set_scratch_line_color "black"
         | _ -> ());
        Graphics.set_line_width !scratch_line_width;
        G.set_color !scratch_line_color;
        scratch_draw btn x y
       end else
      if nx <> x || ny <> y then scratch_lineto btn nx ny;
      (match nbtn with
      | true -> (* down *)
   prerr_endline "down"; Pervasives.flush stderr;
         if btn then scratch_draw btn nx ny else enter_draw ()
      | false ->
   prerr_endline "up"; Pervasives.flush stderr;
 (* up *) scratch_draw false nx ny)

and enter_draw () =
   prerr_endline "up"; Pervasives.flush stderr;
   wait_button_pressed ();
   Graphics.set_line_width !scratch_line_width;
   G.set_color !scratch_line_color;
   prerr_endline "pressed"; Pervasives.flush stderr;
   let x, y = G.mouse_pos () in
   G.moveto x y;
   scratch_draw true x y;;

let do_draw () = save_excursion Cursor_spraycan !scratch_line_color enter_draw;;

let draw () = only_on_screen do_draw ();;

