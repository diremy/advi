(***********************************************************************)
(*                                                                     *)
(*                             Active-DVI                              *)
(*                                                                     *)
(*                   Projet Cristal, INRIA Rocquencourt                *)
(*                                                                     *)
(*  Copyright 2003 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the GNU Lesser General Public License.          *)
(*                                                                     *)
(*  Jun Furuse, Didier Rémy and Pierre Weis.                           *)
(*  Contributions by Roberto Di Cosmo, Didier Le Botlan,               *)
(*  Xavier Leroy, and Alan Schmitt.                                    *)
(*                                                                     *)
(*  Based on Mldvi by Alexandre Miquel.                                *)
(***********************************************************************)

(*  A laser pointer to point to the slides.
    Pierre Weis.                                                       *)

(* $Id$ *)
open Graphics;;

let pointer_color = Graphics.red;;
let not_pointer_color = Graphics.background;;

(* Pointer size is at least 4 *)
let pointer_size = 40;;

type pointer = {
  mutable bkg_img : image;
  mutable ptr_img : image;
  mutable x : int;
  mutable y : int;
  size : int;
};;

(* Creating the pointer image. *)

(*
In the backing store (not to disturb the screen).

   0 - Save some background image somewhere (for instance where is the mouse)
   1 - Clear this area
   2 - Draw the pointer
   3 - Save this image
   4 - Restore the background image
*)

let make_white_image h w =
  let pixmap = Array.make_matrix h w not_pointer_color in
  make_image pixmap;;

let color_as_transp c img =
  let pixmap = dump_image img in
  for i = 0 to Array.length pixmap - 1 do
    let l = pixmap.(i) in
    for j = 0 to Array.length l - 1 do
      if l.(j) = c then l.(j) <- transp
    done
  done;
  make_image pixmap;;

(* Draw the pointer with lower left corner at (x, y). *)
let draw_pointer x y w =
  let r = w / 2 - 2 in
  let d = r / 2 in
  set_color not_pointer_color;
  fill_rect x y w w;
  set_color pointer_color;
  let xc = x + r + 1
  and yc = y + r + 1 in
  fill_circle xc yc r;
  set_color not_pointer_color;
  fill_circle xc yc d;;

(* Create a pointer with lower left corner at (x, y). *)
let create_pointer x y w =
  (* Save the initial background. *)
  let bkg_img = get_image x y w w in
  (* Draw the pointer, centered at (x, y). *)
  draw_pointer x y w;
  (* Get the corresponding image. *)
  let pointer_image =
    color_as_transp not_pointer_color (get_image x y w w) in
  (* Restore the background. *)
  draw_image bkg_img x y;
  { bkg_img = bkg_img;
    ptr_img = pointer_image;
    x = x; y = y; size = w;
  };;

(* Clear the actual pointer. *)
let clear_pointer ptr = draw_image ptr.bkg_img ptr.x ptr.y;;

(* Show the pointer: first save the background, then draw the pointer. *)
let show_pointer ptr x y =
  (* Save actual background. *)
  blit_image ptr.bkg_img x y; 
  (* Move the pointer. *)
  ptr.x <- x; ptr.y <- y;
  draw_image ptr.ptr_img x y;
;;

let move_pointer ptr x y =
  if x <> ptr.x || y <> ptr.y then begin
    clear_pointer ptr;
    show_pointer ptr x y;
  end;;

let switch_on_laser_beam () =
 
 let (x, y) = mouse_pos () in
 let laser_pointer =
   GraphicsY11.only_on_backing_store (create_pointer x y) pointer_size in

 show_pointer laser_pointer x y;

 try while true do
   match
     wait_next_event
       [Mouse_motion; Button_down; Button_up; Key_pressed;] with
   | { mouse_x = x; mouse_y = y;
       button = btn;
       keypressed = kp;
       key = c; } ->
       move_pointer laser_pointer x y;
       if kp then begin
         match c with
         | '' -> raise Exit
         | c ->
            Misc.warning
             (Printf.sprintf "Laser pointer: unknown key binding %C" c)
       end
   done with
 | Exit -> clear_pointer laser_pointer;;

let laser_beam = GraphicsY11.only_on_screen switch_on_laser_beam;;
