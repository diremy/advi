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

(* A simple terminal to handle simple editions and user's interaction. *)

type term = {
 mutable lines : string array;
 (* Height in lines. *)
 mutable height : int;
 (* Width in characters. *)
 mutable width : int;
 (* Height in pixels. *)
 mutable gheight : int;
 (* Width in pixels. *)
 mutable gwidth : int;
 (* Lower left corner of the terminal screen. *)
 mutable gx : int;
 mutable gy : int;
 (* Coordinates of the cursor in the char array. *)
 mutable cursor_x : int;
 mutable cursor_y : int;
 (* Coordinates of the cursor in the graphics window. *)
 mutable cursor_gx : int;
 mutable cursor_gy : int;
 (* Colors *)
 mutable cursor_color : Graphics.color;
 mutable foreground : Graphics.color;
 mutable background : Graphics.color;
 mutable border_width_color : Graphics.color;
 (* Decorations *)
 mutable border_width : int;
 mutable title : string;
 (* Font *)
 mutable font : string;
 mutable font_size_x : int;
 mutable font_size_y : int;
};;

let draw_cursor t cc cf gx gy =
 Graphics.set_color cc;
 Graphics.fill_rect gx gy t.font_size_x t.font_size_y;
 let c = t.lines.(t.cursor_y).[t.cursor_x] in
 Graphics.moveto t.cursor_gx t.cursor_gy;
 Graphics.set_color cf;
 Graphics.draw_char c;
 Graphics.set_color t.foreground;;

let show_cursor t =
 draw_cursor t t.cursor_color t.background t.cursor_gx t.cursor_gy;;

let hide_cursor t =
 draw_cursor t t.background t.foreground t.cursor_gx t.cursor_gy;;

let htab t h =
 let h = (max 0 h) mod t.width in
 hide_cursor t;
 t.cursor_x <- h;
 t.cursor_gx <- t.gx + t.font_size_x * h;
 show_cursor t;;
 
let vtab t v =
 let v = (max 0 v) mod t.height in
 hide_cursor t;
 t.cursor_y <- v;
 t.cursor_gy <- t.gy + t.font_size_y * v;
 show_cursor t;;

let draw_border t =
 let w = t.border_width
 and h = t.border_width
 and gh = t.gheight
 and gw = t.gwidth
 and x = t.gx
 and y = t.gy
 in
 Graphics.set_color t.border_width_color;

 Graphics.fill_rect x (y - h) (gw + w) w;
 Graphics.fill_rect (x + gw) y w (gh + h);

 Graphics.fill_rect (x - w) (y - h) w (gh + h);
 Graphics.fill_rect (x - w) (y + gh) (gw + w) w;

 let title = t.title in
 let tx, ty = Graphics.text_size title in
 let hmargin = ((gw + 2 * w) - tx) / 2 in
 let vmargin = max 0 ((h - ty) / 2) in
 let title =
   if hmargin >= 0 then title
   else String.sub title 0 t.width in
 Graphics.moveto (x - w + max 0 hmargin) (y + gh + vmargin);
 Graphics.set_color t.foreground;
 Graphics.draw_string title;;

let draw_term t =
 Graphics.set_color t.background;
 Graphics.fill_rect t.gx t.gy t.gwidth t.gheight; 
 draw_border t;
 show_cursor t;;

let redraw t =
 draw_term t;
 Array.iteri
  (fun i s ->
     Graphics.moveto t.gx (t.gy + t.font_size_y * i);
     Graphics.draw_string s)
  t.lines;
 show_cursor t;;

let clear t =
 Graphics.set_color t.background;
 Graphics.fill_rect
   t.gx t.gy (t.width * t.font_size_x) (t.height * t.font_size_y);
 Graphics.set_color t.foreground;
 Array.iter (fun s -> String.fill s 0 (String.length s) ' ') t.lines;
 htab t 0;
 vtab t (t.height - 1)
;;

let print_nl t = htab t 0; vtab t (t.cursor_y - 1);;

let rec print_str t s =
 hide_cursor t;
 let l = String.length s in
 let n =
  try String.index s '\n'
  with Not_found -> l in
 let sx, sy = Graphics.text_size s in
 let ncursor_gx = t.cursor_gx + sx in

 let print_it s =
   let l = String.length s in
   Graphics.moveto t.cursor_gx t.cursor_gy;
   Graphics.set_color t.background;
   Graphics.fill_rect t.cursor_gx t.cursor_gy (l * t.font_size_x) t.font_size_y;
   Graphics.set_color t.foreground;
   Graphics.draw_string s;
   String.blit s 0 t.lines.(t.cursor_y) t.cursor_x l;
   t.cursor_gx <- ncursor_gx;
   t.cursor_x <- t.cursor_x + l in

 let print_cont t scont =
   print_it scont;
   t.cursor_x <- 0;
   t.cursor_gx <- t.gx;
   let v = (max 0 (t.cursor_y - 1)) mod t.height in
   t.cursor_y <- v;
   t.cursor_gy <- t.gy + t.font_size_y * v in

 let print_endline =
   let endline = String.make 1 ' ' in
   fun t -> print_cont t endline
 and print_continue =
   let endline = String.make 1 '\\' in
   fun t -> print_cont t endline in

 begin
  if n < l then begin
    print_str t (String.sub s 0 n);
    print_endline t;
    if l - n > 1 then print_str t (String.sub s (n + 1) (l - n - 1));

  end else begin
    let lim = t.gx + t.gwidth in
    if ncursor_gx < lim then print_it s else
    if ncursor_gx = lim then print_continue t else
    begin
     let nbc = t.width - t.cursor_x - 1 in
     print_str t (String.sub s 0 nbc);
     print_continue t;
     print_str t (String.sub s nbc (l - nbc));
    end
  end
 end;
 show_cursor t
;;

let print_chr t c =
 match c with
 | '\n' -> print_nl t
 | _ -> print_str t (String.make 1 c);;

let make_term_gen fg bg bw bwc cc x y h w =
 let font_size_x = 6
 and font_size_y = 13 in
 let t = {
   lines = Array.init h (fun i -> String.make w ' ');
   height = h;
   width = w;
   gheight = h * font_size_y;
   gwidth = w * font_size_x;
   gx = x;
   gy = y;
   cursor_x = 0;
   cursor_y = h - 1;
   cursor_gx = x;
   cursor_gy = y + (h - 1) * font_size_y;

   cursor_color = cc;
   foreground = fg;
   background = bg;
   border_width_color = bwc;
   border_width = bw;
   title = Printf.sprintf "Gterm %ix%i" w h;

   font = Printf.sprintf "%ix%i" font_size_x font_size_y;
   font_size_x = 6;
   font_size_y = 13;
  } in
 t;;

let set_title t s =
 let tx, ty = Graphics.text_size s in
 if tx <= t.width then t.title <- s
 else invalid_arg (Printf.sprintf "set_title: ``%s'' is too long" s);;

let make_term =
  make_term_gen
  Graphics.black Graphics.white
  10 Graphics.black
   0x6FFFFF;;

(* Basic functions to edit. *)
let end_of_line t =
  let line = t.lines.(t.cursor_y) in
  let l = String.length line in
  prerr_endline (string_of_int l);
  htab t l;;

let beginning_of_line t = htab t 0;;
let forward_char t = htab t (t.cursor_x + 1);;
let backward_char t = htab t (t.cursor_x - 1);;
let next_line t = vtab t (t.cursor_y - 1);;
let previous_line t = vtab t (t.cursor_y + 1);;
let backspace t =
 backward_char t;
 print_chr t ' ';
 backward_char t;;
let kill_line t =
 (* Still to implement *)
 print_str t " ";;

(* Still to implement
let scroll_one_line_down t =
let scroll_one_line_up t =
let scroll_one_window_up t =
let scroll_one_window_down t =
*)

module G = Graphics;;

module Graphics = GraphicsY11;;

let rec edit t =
  let evt = Graphics.wait_next_event [Graphics.Key_pressed] in
  if evt.Graphics.keypressed then
    let c = evt.Graphics.key in
    begin match c with
    | '' -> beginning_of_line t
    | '' -> backward_char t
    | '' -> end_of_line t
    | '' -> forward_char t
    | '' | '' -> backspace t
    | '' -> kill_line t
    | '' -> redraw t
    | '' -> next_line t
    | '' -> previous_line t
    | '\n' | '\r' -> print_nl t
    | c -> prerr_endline (String.make 1 c); print_chr t c end;
    edit t;;

(* Basic functions to input strings. *)
let rec flush_keys () =
 if Graphics.key_pressed () then
   let c = Graphics.read_key () in flush_keys ();;

let get_line =
  let b = Buffer.create 11 in
  let get t =
    flush_keys ();
    let limx = t.cursor_x
    and limy = t.cursor_y in
    let rec read t =
Graphics.synchronize ();
      let evt = Graphics.wait_next_event [Graphics.Key_pressed] in
      if evt.Graphics.keypressed then
      match evt.Graphics.key with
      | '\n' | '\r' ->
         print_nl t;
         let s = Buffer.contents b in
         Buffer.clear b;
         s
      | '' ->
          if t.cursor_x = limx then () else
           begin
             let s = Buffer.contents b in
             Buffer.clear b;
             let nl = String.length s - 1 in
             if nl > 0 then Buffer.add_string b (String.sub s 0 nl);
             backspace t;
           end;
          read t
      | c ->
         Buffer.add_char b c;
         print_chr t c;
         read t
      else read t in
    read t in
  get;;

(* Try it ! *)
let ask_user t s1 s2 s3 =
(*prerr_endline "Clearing terminal ";
GraphicsY11.init ();*)
(* clear t;
redraw t;
prerr_endline "Terminal Cleared "; *)
 vtab t 16; htab t 15; print_str t s1;
 vtab t 12; htab t 10; print_str t s2;
 vtab t 8; htab t 15; print_str t s3;
prerr_endline "Setting prompt ";
 let answer = get_line t in
 flush_keys ();
 match answer with
 | "yes" -> true
 | _ -> false;;

let ask s1 s2 =
 let t = make_term_gen
 G.green G.black
 25 G.red
 0x6FFFFF
 50 80 24 80 in
 draw_term t;
 prerr_endline "Starting input ";
(*Graphics.synchronize ();*)
 let b = ask_user t s1 s2 in
 Graphics.synchronize ();
prerr_endline "Synchronized!";
 b;;

let ask_to_launch command =
prerr_endline "Asking before launching";
 ask
  "Attempt to launch the following command"
  command
  "Do you want to execute it ? <yes/no> " ;;

(*Graphics.set_color Graphics.red; Graphics.fill_rect 0 0 800 600; redraw t;;*)
