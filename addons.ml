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
(*  Roberto Di Cosmo                                                   *)
(*                                                                     *)
(*  Addons for programmed backgrounds                                  *)
(*                                                                     *)
(***********************************************************************)

(* $Id$ *)

open Graphics;;
open Gradient;;
open Grdev;;

(* Return the color of a valid point (i.e. inside the graphics window). *)
let point_color x y =
  let x = min x (size_x () - 1)
  and y = min y (size_y () - 1) in
  GraphicsY11.point_color x y;;

(* Gradients *)
let hgradient c1 c2
  {v_size_x = w; v_size_y = h; v_off_x = xoff; v_off_y = yoff} =
(*  let c1 = white in
  let c2 = point_color (xoff + w) (yoff + h) in*)
  Gradient.grad_rect (Rect_Horizontal (c1, c2)) xoff yoff w h;;

let vgradient c1 c2
  {v_size_x = w; v_size_y = h; v_off_x = xoff; v_off_y = yoff} =
(*  let c1 = white in
  let c2 = point_color (xoff + w) (yoff + h) in *)
  grad_rect (Rect_Vertical (c1, c2)) xoff yoff w h;;

let d1gradient c1 c2
  {v_size_x = w; v_size_y = h; v_off_x = xoff; v_off_y = yoff} =
(*  let c1 = white in
  let c2 = point_color (xoff + w) (yoff + h) in*)
  grad_rect (Rect_Diagonal1 (c1, c2)) xoff yoff w h;;

let d2gradient c1 c2
  {v_size_x = w; v_size_y = h; v_off_x = xoff; v_off_y = yoff} =
(*  let c1 = white in
  let c2 = point_color (xoff + w) (yoff + h) in *)
  grad_rect (Rect_Diagonal2 (c1, c2)) xoff yoff w h;;

let dgradient = d1gradient;;

let center {v_size_x = w; v_size_y = h; v_off_x = xoff; v_off_y = yoff} =
  let xc = xoff + (w + 1) / 2
  and yc = yoff + (h + 1) / 2 in
  xc, yc;;

let cgradient c1 c2
  ({v_size_x = w; v_size_y = h; v_off_x = xoff; v_off_y = yoff} as viewport) =
(*  let c1 = white in
  let c2 = point_color (xoff + w) (yoff + h) in *)
  let xc, yc = center viewport in
  grad_rect (Rect_Centered (c1, c2, xc, yc)) xoff yoff w h;;

let circgradient c1 c2
  ({v_size_x = w; v_size_y = h; v_off_x = xoff; v_off_y = yoff} as viewport) =
(*  let c1 = white in
  let c2 = point_color (xoff + w) (yoff + h) in*)
  let xc, yc = center viewport in
  grad_rect (Rect_Circular (c1, c2, xc, yc)) xoff yoff w h;;
