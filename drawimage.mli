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

type ratiopts =
   | ScaleOriginal
      (* leave the image at its original native size  *)
   | ScaleAuto
      (* scale to fit requested area *)
   | ScaleCenter
      (* scale as needed to cover the image, keep original Ratio and center *)
   | ScaleTop
      (* scale x coords to align to top of the screen, keep original Ratio  *)
   | ScaleBottom
      (* scale x coords to align to bottom of the screen,
         keep original Ratio  *)
   | ScaleLeft
      (* scale y coords to align to left of the screen, keep original Ratio  *)
   | ScaleRight
      (* scale y coords to align to right of the screen, keep original Ratio  *)
   | ScaleTopLeft
   | ScaleBottomLeft
   | ScaleTopRight
   | ScaleBottomRight
;;

(* Blending *)
type blend =
   | Normal | Multiply | Screen | Overlay (* | SoftLight | HardLight *)
   | ColorDodge | ColorBurn | Darken | Lighten | Difference
   | Exclusion (* | Luminosity | Color | Saturation | Hue *);;

type alpha = float;;
(** Alpha channel specification. *)

type image_size = int * int;;
(** The size of an image in pixels. *)

type ps_bbox = int * int * int * int;;
(** The PostScript bounding box of an image as recorded in an
   encapsulated PostScript file that contains it. *)

val f : string -> bool -> float -> blend ->
        ps_bbox option -> 
        ratiopts -> bool -> image_size -> (int * int) -> unit;;
(** [f filename whitetransp alpha blend 
      (llx, lly, urx, ury) antialias (width, height) (x0, y0)]
   draws an eps [filename] with bounding box [(llx,lly,urx,ury)]
   in the size [(width, height)] pixels at [x0, y0] (top-left corner).
   If [whitetransp] is true, white pixels are treated as transparent.
   [alpha] specifies the alpha level of the image.
   [blend] is the color blending function for rendering. 
   [antialias] controls antialiasing mode
 *)

val clean_cache : unit -> unit;;
