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

(* Saving screen shots. *)

open Image;;

type x = int and y = int and w = int and h = int;;

let output_area fname x y w h =
  let img = Graphic_image.get_image x y w h in
  Image.save fname None [] (Rgb24 img);;

let save_area fname x y w h =
  let screen_w = Graphics.size_x () and screen_h = Graphics.size_y () in
  if x >= 0 && x <= screen_w &&
     y >= 0 && y <= screen_h &&
     h >= 0 && y + h <= screen_h &&
     w >= 0 && x + w <= screen_h then output_area fname x y w h;;

let save_page fname =
  let w = Graphics.size_x () and h = Graphics.size_y () in
  output_area fname 0 0 w h;;

let cntr = ref 0;;

let save () =
  let fname = Printf.sprintf "shot%d.jpg" !cntr in save_page fname;;
