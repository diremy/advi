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

(* Cursors. *)
let cursor_write = Cursor_pencil;;
let cursor_draw = Cursor_spraycan;;
let cursor_settings = Cursor_sizing;;

(* Scratch line color and width. *)

let set_scratch_line_color, get_scratch_line_color =
  let scratch_line_color = ref G.red in
  (fun c ->
    (* Be careful not to emit a warning if there is no modification. *)
    if c > 0 && c <> !scratch_line_color then begin
      scratch_line_color := c;
      Misc.warning (Printf.sprintf "Setting scratch font color to %d" c)
    end),
  (fun () -> !scratch_line_color);;

let set_scratch_line_color_string s =
  set_scratch_line_color (Dvicolor.parse_color s);;
Options.add "-scratch-line-color"
 (Arg.String set_scratch_line_color_string)
 "<color>  set the color of the pen used\
 \n\t when scratching slides,\
 \n\t (the default scratch pen color is red)."
;;

let color_incr = ref 10;;
let set_positive_color_increment () =
  Misc.warning (Printf.sprintf "Setting color increment to positive");
  color_incr := abs !color_incr;;
let set_negative_color_increment () =
  Misc.warning (Printf.sprintf "Setting color increment to negative");
  color_incr := - abs !color_incr;;

let incr_scratch_line_r_color () =
 set_scratch_line_color (get_scratch_line_color () + (!color_incr lsl 16));;

let incr_scratch_line_g_color () =
 set_scratch_line_color (get_scratch_line_color () + (!color_incr lsl 8));;

let incr_scratch_line_b_color () =
 set_scratch_line_color (get_scratch_line_color () + !color_incr);;

let default_scratch_line_width = 2;;
let set_scratch_line_width, get_scratch_line_width =
  let scratch_line_width = ref default_scratch_line_width in
  (fun i ->
    (* No warning if there is no modification. *)
    if i > 0 && i <> !scratch_line_width then begin
      Misc.warning (Printf.sprintf "Setting scratch line width to %d" i);
      scratch_line_width := i
    end),
  (fun () -> !scratch_line_width);;
Options.add "-scratch-line-width"
 (Arg.Int set_scratch_line_width)
 (Printf.sprintf
   "<int>  set the width of the pen used\
   \n\t when scratching slides,\
   \n\t (the default scratch pen width is %i)."
   default_scratch_line_width)
;;

let incr_scratch_line_width () =
  set_scratch_line_width (get_scratch_line_width () + 1);;
let decr_scratch_line_width () =
  set_scratch_line_width (get_scratch_line_width () - 1)
;;

(*** Scratch font color, kind, and size settings. *)

(* Font parsing and printing utilities. *)
let scan_font_name font = Scanf.sscanf font "-%s@-%s@-%s@-%s@-%s@-";;

let build_font_name sz s1 s2 s3 s4 s5 =
  Printf.sprintf
    "-%s-%s-%s-%s-%s--%d-*-*-*-*-*-iso8859-1"
    s1 s2 s3 s4 s5 sz;;

(* Font color. *)
let set_scratch_font_color, get_scratch_font_color =
  let scratch_font_color = ref G.red in
  (fun c ->
    (* Be careful not to emit a warning if there is no modification. *)
    if c > 0 && c <> !scratch_font_color then begin
      scratch_font_color := c;
      Misc.warning (Printf.sprintf "Setting scratch font color to %d" c)
    end),
  (fun () -> !scratch_font_color);;

let set_scratch_font_color_string s =
  set_scratch_font_color (Dvicolor.parse_color s);;
Options.add "-scratch-font-color"
 (Arg.String set_scratch_font_color_string)
 "<color>  set the color of the font used\
 \n\t when scratching slides,\
 \n\t (the default scratch font color is \"red\")."
;;

let incr_scratch_font_r_color () =
 set_scratch_font_color (get_scratch_font_color () + (!color_incr lsl 16));;

let incr_scratch_font_g_color () =
 set_scratch_font_color (get_scratch_font_color () + (!color_incr lsl 8));;

let incr_scratch_font_b_color () =
 set_scratch_font_color (get_scratch_font_color () + !color_incr);;


(* Font type setting. *)
let set_scratch_font, get_scratch_font =
  let scratch_font_ref =
    ref "-adobe-times-bold-r-normal--18-180-75-75-p-99-iso8859-1" in
  (fun s ->
    (* Be careful not to emit a warning if there is no modification. *)
    if s <> !scratch_font_ref then begin
      Misc.warning
        (Printf.sprintf "Setting scratch font to %s" s);
      scratch_font_ref := s
    end),
  (fun () -> !scratch_font_ref);;

Options.add "-scratch-font"
 (Arg.String set_scratch_font)
 "<font>  set the font used when scratching slides,\
 \n\t (the default scratch font is the X font specification\
 \n\t \"-*-times-bold-r-normal--18-180-75-75-p-99-iso8859-1\")."
;;

(* Font size. *)
let default_scratch_font_size = 18;;
let get_scratch_font_size, set_scratch_font_size =
  let scratch_font_size = ref default_scratch_font_size in
  (fun () -> !scratch_font_size),
  (fun sz ->
     scratch_font_size := sz)
;;

let make_scratch_font font =
  scan_font_name font (build_font_name (get_scratch_font_size ()));;

let update_scratch_font () =
  set_scratch_font (make_scratch_font (get_scratch_font ()));;

let incr_scratch_font_size () =
  set_scratch_font_size (get_scratch_font_size () + 1);
  update_scratch_font ()
;;

let decr_scratch_font_size () =
  set_scratch_font_size (get_scratch_font_size () - 1);
  update_scratch_font ()
;;

(*** Scratching utilities:
     - cautious_set_font sets the font to a given argument.
     - save_excursion executes a function with the current scratching
       settings and restore the original ones at the end.
     - wait_button_pressed executes a function on keys entered, while
       waiting for a click that will end the execution of f. *)
let cautious_set_font fnt =
  try Graphics.set_font fnt with
  | G.Graphic_failure s -> Misc.warning s;;

let set_font_to_scratch_font () = cautious_set_font (get_scratch_font ());;

let save_excursion cursor color f =

  let current_color = Graphics.get_color () in
  let current_line_width = Graphics.get_line_width () in
  let current_font = Graphics.get_font () in
  let current_cursor = Graphics.get_cursor () in

  let restore () =
    G.set_color current_color;
    Graphics.set_line_width current_line_width;
    Graphics.set_font current_font;
    set_cursor current_cursor in

  G.set_color color;
  Graphics.set_line_width (get_scratch_line_width ());
  set_font_to_scratch_font ();
  set_cursor cursor;

  try f (); restore ()
  with x -> restore (); if x <> Exit then raise x
;;

let rec wait_button_pressed f =
  match Graphics.wait_next_event [Button_down; Key_pressed] with
  | {mouse_x = x; mouse_y = y; button = btn; keypressed = kp; key = c} ->
      if kp then begin f c; wait_button_pressed f end else
      if not btn then wait_button_pressed f;;

(*** Scratching characters on the screen. *)

let end_write () = raise Exit;;

(* Scratch write settings:
   for each setting bound char we call the corresponding setting function.
   Otherwise we just warn. *)
let scratch_write_settings_handle_char = function
  | '>' -> incr_scratch_font_size ()
  | '<' -> decr_scratch_font_size ()
  | 'R' -> incr_scratch_font_r_color ()
  | 'G' -> incr_scratch_font_g_color ()
  | 'B' -> incr_scratch_font_b_color ()
  | 'b' -> set_scratch_font_color G.blue
  | 'g' -> set_scratch_font_color G.green
  | 'w' -> set_scratch_font_color G.white
  | 'c' -> set_scratch_font_color G.cyan
  | 'm' -> set_scratch_font_color G.magenta
  | 'r' -> set_scratch_font_color G.red
  | 'y' -> set_scratch_font_color G.yellow
  | 'k' -> set_scratch_font_color G.black
  | '+' -> set_positive_color_increment ()
  | '-' -> set_negative_color_increment ()
  | '?' -> Grdev.help_screen Config.scratch_write_splash_screen
  | c ->
     Misc.warning (Printf.sprintf "Unknown scratch write setting key: %C" c);;

(* The writing function:
   write char c at position x y then computes next scratching position
   and calls the main write scractching loop. *)
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
     G.set_color (get_scratch_font_color ());
     scratch_write px py
   end else begin
     let tx, ty = G.text_size (String.make 1 c) in
     prev_xs := x :: !prev_xs;
     prev_size_y := max ty !prev_size_y;
     G.draw_char c;
     scratch_write (x + tx) y end

(* Main scratch write loop:
   - each button press changes the place where scratching occurs,
   - Esc ends scratching.
   - ^Q or ^Z enters scratch write settings mode.
   - each key press is written at current scratching position using
     scratch_write_char that tail calls back scratch_write
     at the new scratching position. *)
and scratch_write x y =
   match Graphics.wait_next_event [Button_down; Key_pressed] with
   | {mouse_x = nx; mouse_y = ny; button = btn;
      keypressed = kp; key = c} ->
       if kp then
        begin match c with
        | '' -> end_write ()
        | '' | '' ->
          set_cursor cursor_settings;
          scratch_write_settings ()
        | c -> scratch_write_char c x y
        end else
       if btn then scratch_write nx ny else scratch_write x y

and enter_scratch_write () =
  let x, y = G.mouse_pos () in
  G.moveto x y;
  set_font_to_scratch_font ();
  G.set_color (get_scratch_font_color ());
  set_cursor cursor_write;
  scratch_write x y

(* Main write settings loop. *)
and scratch_write_settings () =
   match Graphics.wait_next_event [Button_down; Key_pressed] with
   | {mouse_x = nx; mouse_y = ny; button = btn;
      keypressed = kp; key = c} ->
       if kp then
        begin match c with
        | '' | '' | '' | 'q' ->
        (* Esc, ^Q, ^Z, or q means quit the scratch write settings mode
           and reenter scratch writing.
           Hence, two successive ^Z just do nothing (except changing
           the cursor chape for a while). *)
           set_cursor cursor_write;
           enter_scratch_write ()
        | c ->
        (* In any other case, treat the setting according to the character,
           and go on write settings. *)
           scratch_write_settings_handle_char c;
           scratch_write_settings ()
        end
;;

(* Handling key presses while waiting for a mouse click
   that designates the place where write scratching should begin:
   - Esc, q, or s means quit write,
   - ^Q or ^Z means toggle the scratch write settings mode,
   - any other character when scratch write settings mode is on
     means a setting (to handle with scratch_write_settings_handle_char),
   - any other character when not in scratch write settings mode,
     means warning the user that he must click somewhere. *)
let waiting_to_enter_scratch_write =
  let scratch_write_settings_mode = ref false in
  (function c ->
   if !scratch_write_settings_mode then begin
     match c with
     (* Esc, q, or s means quit scratch write. *)
     | '' | 'q' | 's' ->
       (* Quit preserving invariant. *)
       scratch_write_settings_mode:= false;
       set_cursor cursor_write
     | '' | '' ->
       (* Quit scratch write settings mode. *)
       set_cursor cursor_write;
       (* Toggle scratch write settings mode. *)
       scratch_write_settings_mode := false
     | c ->
       scratch_write_settings_handle_char c
   end else begin
     match c with
     (* Esc, q, or s means quit scratch write. *)
     | '' | 'q' | 's' ->
       (* Quit preserving invariant. *)
       scratch_write_settings_mode:= false;
       end_write ()
     | '' | '' ->
       (* Enter scratch write settings mode. *)
       set_cursor cursor_settings;
       scratch_write_settings_mode := true
     | '?' -> Grdev.help_screen Config.scratch_write_splash_screen
     | c -> Misc.warning "Click to start scratch writing"
   end)
;;

(* The main routine to begin write scratching:
   - wait for a click then enter scratching. *)
let start_scratch_write () =
  wait_button_pressed waiting_to_enter_scratch_write;
  enter_scratch_write ();;

let do_write () =
  save_excursion cursor_write (get_scratch_font_color ()) start_scratch_write;;

let write () = only_on_screen do_write ();;

(*** Scratching figures: drawing lines and figures on the screen. *)

let end_draw () = raise Exit;;

type scratch_figure =
   | Point | Hline | Vline | Segment | Circle | Polygone | Finish | No_figure;;
let scratch_figure = ref No_figure;;
let set_scratch_figure f = scratch_figure := f;;
let clear_scratch_figure () = set_scratch_figure No_figure;;

let scratch_draw_setting_handle_char c =
  (match c with
   | '>' -> incr_scratch_line_width ()
   | '<' -> decr_scratch_line_width ()
   | 'R' -> incr_scratch_line_r_color ()
   | 'G' -> incr_scratch_line_g_color ()
   | 'B' -> incr_scratch_line_b_color ()
   | 'b' -> set_scratch_line_color G.blue
   | 'g' -> set_scratch_line_color G.green
   | 'w' -> set_scratch_line_color G.white
   | 'c' -> set_scratch_line_color G.cyan
   | 'm' -> set_scratch_line_color G.magenta
   | 'r' -> set_scratch_line_color G.red
   | 'y' -> set_scratch_line_color G.yellow
   | 'k' -> set_scratch_line_color G.black
   | 'v' -> set_scratch_figure Vline
   | 'h' -> set_scratch_figure Hline
   | 's' -> set_scratch_figure Segment
   | 'C' -> set_scratch_figure Circle
   | 'p' -> set_scratch_figure Point
   | 'P' -> set_scratch_figure Polygone
   | 'f' -> set_scratch_figure Finish
   | ' ' -> clear_scratch_figure ()
   | '+' -> set_positive_color_increment ()
   | '-' -> set_negative_color_increment ()
   | '?' -> Grdev.help_screen Config.scratch_draw_splash_screen
   | c ->
      Misc.warning (Printf.sprintf "Unknown scratch draw setting key: %C" c));
  Graphics.set_line_width (get_scratch_line_width ());
  G.set_color (get_scratch_line_color ());;

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
  match get_scratch_line_width () with
  | 1 -> G.plot x y
  | _ -> G.fill_circle x y ((2 * get_scratch_line_width () + 3) / 4);;

let draw_figure f =
  let (x0, y0 as p0) = G.mouse_pos () in
  wait_button_pressed scratch_draw_setting_handle_char; 
  let (x1, y1 as p1) = G.mouse_pos () in
  f p0 p1;;

(* Draws an horizontal line segment between origin point (x0, y0) to
   end point (x1, y1). *)
let draw_hline (x0, y0) (x1, y1) = G.moveto x0 y0; G.lineto x1 y0;;

(* Draws a vertical line segment between origin point (x0, y0) to
   end point (x1, y1). *)
let draw_vline (x0, y0) (x1, y1) = G.moveto x0 y0; G.lineto x0 y1;;

(* Draws a line segment between origin point (x0, y0) to
   end point (x1, y1). *)
let draw_segment (x0, y0) (x1, y1) = G.moveto x0 y0; G.lineto x1 y1;;

let distance x0 y0 x1 y1 =
  let dx = x1 - x0 and dy = y1 - y0 in
  sqrt (float_of_int (dx * dx + dy * dy));;

(* Draws a cercle with diameter (x0, y0) (x1, y1). *)
let draw_circle (x0, y0) (x1, y1) =
  let c_x = (x0 + x1 + 1) / 2
  and c_y = (y0 + y1 + 1) / 2
  and r = Misc.round (distance x0 y0 x1 y1) in
  G.draw_circle c_x c_y r;;

(* Enter drawing on slide *)
let rec scratch_draw00 () =
  let x, y = G.mouse_pos () in
  G.moveto x y;
  G.set_color (get_scratch_line_color ());
  scratch_draw0 ()

(* Waiting for movements, button is pressed. *)
and scratch_draw0 () =
  match find_scratch_events () with
  | (E_Big_Move (x, y) | E_Small_Move (x, y)) :: _ -> scratch_draw1 x y
  | E_No_Move :: E_Up :: _ -> scratch_draw20 ()
  | E_No_Move :: _ :: E_Key c :: _ -> scratch_char_from scratch_draw0 c
  | E_No_Move :: E_Down :: _ -> scratch_draw10 ()
  | _ -> scratch_draw0 ()

(* Move and draw while button is pressed. *)
and scratch_draw1 x y =
  G.lineto x y;
  scratch_draw10 ()

and scratch_draw10 () =
  match find_scratch_events () with
  | (E_Big_Move (x, y) | E_Small_Move (x, y)) :: _ -> scratch_draw1 x y
  | E_No_Move :: _ :: E_Key c :: _ -> scratch_char_from scratch_draw10 c
  | E_No_Move :: E_Up :: _ -> enter_draw ()
  | _ -> scratch_draw10 ()
  
(* Move and draw while button is up. *)
and scratch_draw2 x y =
  G.lineto x y;
  scratch_draw20 ()

and scratch_draw20 () =
  match find_scratch_events () with
  | (E_Big_Move (x, y) | E_Small_Move (x, y)) :: _ -> scratch_draw2 x y
  | E_No_Move :: _ :: E_Key c :: _ -> scratch_char_from scratch_draw20 c
  | E_No_Move :: E_Down :: _ -> enter_draw ()
  | _ -> scratch_draw20 ()

(* Starting drawing: wait for a clic to begin. *)
and enter_draw () =
   (* prerr_endline "enter_draw up or down"; Pervasives.flush stderr; *)
   wait_button_pressed (scratch_char_from enter_draw);
   (* prerr_endline "pressed"; Pervasives.flush stderr;
   let x, y = G.mouse_pos () in
   prerr_endline (Printf.sprintf "in enter_draw x = %i y = %i" x y); *)
   scratch_draw00 ()

and scratch_char_from scratch c =
  scratch_draw_setting_handle_char c;
  handle_figure scratch

and handle_figure scratch =
  match !scratch_figure with
  | No_figure -> scratch ()
  | Point -> scratch_points scratch
  | Hline -> draw_figure draw_hline; handle_figure scratch
  | Vline -> draw_figure draw_vline; handle_figure scratch
  | Segment -> draw_figure draw_segment; handle_figure scratch
  | Circle -> draw_figure draw_circle; handle_figure scratch
  | Polygone | Finish ->
      wait_button_pressed scratch_draw_setting_handle_char; 
      let (x1, y1 as p1) = mouse_pos () in
      clear_scratch_figure ();
      handle_figure scratch

and scratch_points scratch =
  wait_button_pressed (scratch_char_from scratch);
  let x, y = G.mouse_pos () in
  draw_point x y;
  handle_figure scratch;;

let do_draw () =
  save_excursion cursor_draw (get_scratch_line_color ()) enter_draw;;

let draw () = only_on_screen do_draw ();;
