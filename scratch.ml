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

let color_incr = ref 10;;

let incr_scratch_line_r_color () =
 scratch_line_color := !scratch_line_color + (!color_incr lsl 16);;

let incr_scratch_line_g_color () =
 scratch_line_color := !scratch_line_color + (!color_incr lsl 8);;

let incr_scratch_line_b_color () =
 scratch_line_color := !scratch_line_color + !color_incr;;

let default_scratch_line_width = 2;;
let scratch_line_width = ref default_scratch_line_width;;
let set_scratch_line_width i = if i > 0 then scratch_line_width := i;;
Options.add "-scratch-line-width"
 (Arg.Int set_scratch_line_width)
 (Printf.sprintf
   "INT\tSet the width of the pen used when scratching slides (default %i)"
   default_scratch_line_width)
;;

let incr_scratch_line_width () =
  incr scratch_line_width;;
let decr_scratch_line_width () =
  if !scratch_line_width > 1 then decr scratch_line_width;;

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

let end_write () = raise Exit;;

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
        begin match c with
        | '' -> end_write ()
        | c -> scratch_write_char c x y
        end else
       if btn then scratch_write nx ny else
       scratch_write x y
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

let rec wait_button_pressed f =
  match Graphics.wait_next_event [Button_down; Key_pressed] with
  | {mouse_x = x; mouse_y = y; button = btn; keypressed = kp; key = c} ->
      if kp then
        begin match c with
        | '' -> end_write ()
        | c -> f c; wait_button_pressed f
        end else
      if not btn then wait_button_pressed f;;

let write_handle_char c =
  (match c with
   | '' -> end_write ()
   | _ -> ());;

let enter_write () =
  wait_button_pressed write_handle_char;
  let x, y = G.mouse_pos () in
  G.moveto x y;
  scratch_write x y;;

let do_write () = save_excursion Cursor_pencil !scratch_font_color enter_write;;

let write () = only_on_screen do_write ();;

let end_draw () = raise Exit;;

type scratch_figure =
   | Point | Hline | Vline | Segment | Circle | Polygone | Finish | No_figure;;
let scratch_figure = ref No_figure;;
let set_scratch_figure f = scratch_figure := f;;
let clear_scratch_figure () = set_scratch_figure No_figure;;

let draw_handle_char c =
  (match c with
   | '' -> end_draw ()
   | '>' -> incr_scratch_line_width ()
   | '<' -> decr_scratch_line_width ()
   | 'R' -> incr_scratch_line_r_color ()
   | 'G' -> incr_scratch_line_g_color ()
   | 'B' -> incr_scratch_line_b_color ()
   | 'b' -> set_scratch_line_color "blue"
   | 'g' -> set_scratch_line_color "green"
   | 'w' -> set_scratch_line_color "white"
   | 'c' -> set_scratch_line_color "cyan"
   | 'm' -> set_scratch_line_color "magenta"
   | 'r' -> set_scratch_line_color "red"
   | 'y' -> set_scratch_line_color "yellow"
   | 'k' -> set_scratch_line_color "black"
   | 'v' -> set_scratch_figure Vline
   | 'h' -> set_scratch_figure Hline
   | 's' -> set_scratch_figure Segment
   | 'C' -> set_scratch_figure Circle
   | 'p' -> set_scratch_figure Point
   | 'P' -> set_scratch_figure Polygone
   | 'f' -> set_scratch_figure Finish
   | ' ' -> clear_scratch_figure ()
   | '+' -> color_incr := abs !color_incr
   | '-' -> color_incr := - abs !color_incr
   | _ -> ());
  Graphics.set_line_width !scratch_line_width;
  G.set_color !scratch_line_color;;

type event =
   | E_Up
   | E_Down
   | E_Key of char
   | E_No_Move
   | E_Big_Move of int * int
   | E_Small_Move of int * int;;

let print_event oc = function
  | E_Up -> Printf.fprintf oc "up"
  | E_Down -> Printf.fprintf oc "down"
  | E_Key c -> Printf.fprintf oc "Key: %c" c
  | E_No_Move -> Printf.fprintf oc "no move"
  | E_Big_Move (x, y) -> Printf.fprintf oc "big move: %i %i" x y
  | E_Small_Move (x, y) -> Printf.fprintf oc "small move: %i %i" x y;;

let print_events oc =
  List.iter (fun e -> Printf.fprintf oc " %a" print_event e);; 

let get_events =
  let add_key kp c evs = if kp then E_Key c :: evs else evs in
  let add_button b evs = if b then E_Down :: evs else E_Up :: evs in
  let get_move e e0 =
    let x0 = e0.mouse_x and y0 = e0.mouse_y in
    let x = e.mouse_x and y = e.mouse_y in
    let l = abs (x - x0) + abs (y - y0) in
    if l = 0 then E_No_Move else
    if l <= 2 then E_Small_Move (x, y) else E_Big_Move (x, y) in
  let add_move e e0 evs = get_move e e0 :: evs in
  (fun e0 e ->
   match e with
   | {mouse_x = x; mouse_y = y; button = b; keypressed = kp; key = c} ->
       add_move e e0 (add_button b (add_key kp c [])));;

let find_next_events l =
  let e0 = Graphics.wait_next_event [Poll] in
  get_events e0 (Graphics.wait_next_event l);;

let find_scratch_events () =
  let evts =
    find_next_events [Mouse_motion; Button_down; Button_up; Key_pressed] in
  (* print_events stderr evts; prerr_endline "";*)
  evts;;

let draw_point x y =
  match !scratch_line_width with
  | 1 -> G.plot x y
  | _ -> G.fill_circle x y ((2 * !scratch_line_width + 3) / 4);;

(* Enter drawing on slide *)
let rec scratch_draw00 () =
  let x, y = G.mouse_pos () in
  G.moveto x y;
  scratch_draw0 ()

(* Attente du mouvement, le bouton est enfonc�. *)
and scratch_draw0 () =
  match find_scratch_events () with
  | (E_Big_Move (x, y) | E_Small_Move (x, y)) :: _ -> scratch_draw1 x y
  | E_No_Move :: E_Up :: _ -> scratch_draw20 ()
  | E_No_Move :: _ :: E_Key c :: _ -> scratch_char_from scratch_draw0 c
  | E_No_Move :: E_Down :: _ -> scratch_draw10 ()
  | _ -> scratch_draw0 ()

(* D�placement et trac� bouton enfonc�. *)
and scratch_draw1 x y =
  G.lineto x y;
  scratch_draw10 ()

and scratch_draw10 () =
  match find_scratch_events () with
  | (E_Big_Move (x, y) | E_Small_Move (x, y)) :: _ -> scratch_draw1 x y
  | E_No_Move :: _ :: E_Key c :: _ -> scratch_char_from scratch_draw10 c
  | E_No_Move :: E_Up :: _ -> enter_draw ()
  | _ -> scratch_draw10 ()
  
(* D�placement et trac� bouton lev�. *)
and scratch_draw2 x y =
  G.lineto x y;
  scratch_draw20 ()

and scratch_draw20 () =
  match find_scratch_events () with
  | (E_Big_Move (x, y) | E_Small_Move (x, y)) :: _ -> scratch_draw2 x y
  | E_No_Move :: _ :: E_Key c :: _ -> scratch_char_from scratch_draw20 c
  | E_No_Move :: E_Down :: _ -> enter_draw ()
  | _ -> scratch_draw20 ()

(* Entr�e dans le dessin: on attend un clic avant de commencer. *)
and enter_draw () =
   (* prerr_endline "enter_draw up or down"; Pervasives.flush stderr; *)
   wait_button_pressed (scratch_char_from enter_draw);
   (* prerr_endline "pressed"; Pervasives.flush stderr;
   let x, y = G.mouse_pos () in
   prerr_endline (Printf.sprintf "in enter_draw x = %i y = %i" x y); *)
   scratch_draw00 ()

and scratch_char_from scratch c =
  draw_handle_char c;
  handle_figure !scratch_figure scratch

and handle_figure fig scratch =
  match fig with
  | No_figure -> scratch ()
  | Point -> scratch_points scratch
  | Hline | Vline | Segment | Circle | Polygone | Finish ->
      clear_scratch_figure (); scratch ()

and scratch_points scratch =
  wait_button_pressed (scratch_char_from scratch);
  let x, y = G.mouse_pos () in
  draw_point x y;
  handle_figure !scratch_figure scratch;;

let do_draw () = save_excursion Cursor_spraycan !scratch_line_color enter_draw;;

let draw () = only_on_screen do_draw ();;

