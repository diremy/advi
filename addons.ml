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

open Graphics

(* Utils *)

let point_color x y =
        let x' = min ((size_x ())-1) x and y' = min ((size_y ())-1) y in
        GraphicsY11.point_color x' y';;

(* compute an intermediate color (i/w) between c and c' *)
let mean_color c c' i w =
  let rgb_of_color c =
    let b = (c land 0x0000ff) in
    let g = (c land 0x00ff00) lsr 8 in
    let r = (c land 0xff0000) lsr 16 in
    r, g, b in
  let r, g, b  = rgb_of_color c  in
  let r', g', b' = rgb_of_color c' in
  rgb ((r *i + r' *(w-i)) / w) ((g*i + g'*(w-i)) / w) ((b*i + b'*(w-i)) / w);;

let grad i startc ?endc w = 
  mean_color startc (match endc with None -> white | Some c -> c) i w;;

(* Gradients *)

let hgradient (w,h,xoff,yoff) =
  set_line_width 0;
  let col = point_color 0 0 in
  for x=0 to w-1 do
    set_color (grad (w-x) col w);
    fill_rect (xoff+x) yoff 1 h;
  done

let vgradient (w,h,xoff,yoff) = 
  set_line_width 0;
  let col = point_color 0 0 in
  for y=0 to h-1 do
    set_color (grad y col h);
    fill_rect xoff (yoff+y) w 1;
  done

(* N.B.: this code properly draws lines inside the Graphics window, by 
   accurately computing the exact x and y coordinates for each diagonal line.
   Nevertheless, if we choose the "faster, less precise" drawing algorithm in
   the Graphics library, some areas get painted in black depending on
   the geometry (and probably some roundigs in the layers down below me ... )
   Hence, do *not* remove the set_line_width 1 in the function below!
 *)

let dgradient (w,h,xoff,yoff) = 
  let col = point_color 0 0 in
  let dline x y x' y' = moveto x y; lineto x' y' in
  set_line_width 1; (* this line is -essential- do not remove! *)
  for dl=0 to w+h-1 do
    set_color (grad dl col (w+h));
    if h<=w then 
      if dl <= h then dline dl 0 0 dl
      else
	  if dl < w then dline dl 0 (dl-h) h
	  else dline w (dl-w) (dl-h) h
    else
      if dl <= w then dline dl 0 0 dl
      else
	  if dl < h then dline 0 dl h (dl-h)
	  else dline w (dl-w) (dl-h) h
  done
