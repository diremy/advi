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

(*  Predefined gradient color background drawing functions.
    Roberto Di Cosmo, Pierre Weis.                                     *)

(* $Id$ *)

(* Gradients:
   - h is horizontal,
   - v is vertical,
   - d1 is parallel to the first bissector (d stands for diagonal),
   - d2 is parallel to the second bissector,
   - c is centered (enlarging squares with a common center),
   - circ is circular (also centered using enlarging circles) *)

open Graphics;;
open Gradient;;
open Grdev;;

let hgradient {
    argcolor = c2; argcolorstart = c1;
    argfunviewport = {vx = x; vy = y; vw = w; vh = h};
    argviewport = _;
   } =
  Gradient.grad_rect (Rect_Horizontal (c1, c2)) x y w h;;

let vgradient {
    argcolor = c2; argcolorstart = c1;
    argfunviewport = {vx = x; vy = y; vw = w; vh = h};
    argviewport = _;
   } =
  Gradient.grad_rect (Rect_Vertical (c1, c2)) x y w h;;

let d1gradient {
    argcolor = c2; argcolorstart = c1;
    argfunviewport = {vx = x; vy = y; vw = w; vh = h};
    argviewport = _;
   } =
  Gradient.grad_rect (Rect_Diagonal1 (c1, c2)) x y w h;;

let d2gradient {
    argcolor = c2; argcolorstart = c1;
    argfunviewport = {vx = x; vy = y; vw = w; vh = h};
    argviewport = _;
   } =
  Gradient.grad_rect (Rect_Diagonal2 (c1, c2)) x y w h;;

(* For compatibility (already compatibility, when this feature is not
   yet available to any distribution :) *)
let dgradient = d1gradient;;

let center {vx = x; vy = y; vw = w; vh = h} =
  let xc = x + (w + 1) / 2
  and yc = y + (h + 1) / 2 in
  xc, yc;;

let cgradient {
    argcolor = c2; argcolorstart = c1;
    argfunviewport = {vx = x; vy = y; vw = w; vh = h} as viewport;
    argviewport = _;
   } =
  let xc, yc = center viewport in
  Gradient.grad_rect (Rect_Centered (c1, c2, xc, yc)) x y w h;;

let circgradient {
    argcolor = c2; argcolorstart = c1;
    argfunviewport = {vx = x; vy = y; vw = w; vh = h} as viewport;
    argviewport = _;
   } =
  let xc, yc = center viewport in
  Gradient.grad_rect (Rect_Circular (c1, c2, xc, yc)) x y w h;;
