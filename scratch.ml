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

(* The ``scratching'' facility for Active-DVI: you can interactively
   annotate your slides by ``scratching'' on them. *)

module G = Graphics;;

module Graphics = GraphicsY11;;

open Graphics;;

let scratch_color = ref G.red;;

let set_scratch_color s =
  let i =
    try int_of_string s with
    | Failure "int_of_string" ->
        Misc.warning (Printf.sprintf "Cannot understand %s as a color" s);
        G.black in
  scratch_color := i;;

Options.add "-scratch-color"
 (Arg.String set_scratch_color)
 "INT\tSet the color used when scratching slides (default red)"
;;

let scratch_width = ref 2;;

let set_scratch_width i = scratch_width := i;;

Options.add "-scratch-width"
 (Arg.Int set_scratch_width)
 (Printf.sprintf 
   "INT\tSet the width of the pen used when scratching slides (default %i)"
   !scratch_width)
;;

let scratch_lineto x y = if y > 0 then G.lineto x y;;

let rec scratch x y =
   match Graphics.wait_next_event [Mouse_motion; Button_up] with
   | {mouse_x = nx; mouse_y = ny; button = btn} ->
       if nx <> x || ny <> y then begin
           scratch_lineto nx ny;
           scratch nx ny end else
       if btn then scratch nx ny
;;

let do_on_screen f x =
  G.remember_mode false;
  GraphicsY11.display_mode true;
  f x;
  G.remember_mode true;
  GraphicsY11.display_mode false;;

let scratch_from x y = do_on_screen (scratch x) y;;

let rec f () =
 G.set_color !scratch_color;
 G.set_line_width !scratch_width;
 let cursor = get_cursor () in
 set_cursor Cursor_pencil;
 try 
   match Graphics.wait_next_event [Button_down] with
   | {mouse_x = x; mouse_y = y} ->
       G.moveto x y;
       scratch_from x y;
       set_cursor cursor
 with x -> set_cursor cursor; raise x
;;
