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

type t = {
    width : int ;
    height : int ;
    voffset : int ;
    hoffset : int ;
    graymap : GlGlyph.t
  } ;;

let scale_size len off ratio =
  let left = off
  and right = len - off in
  let left' = int_of_float (ceil (ratio *. float left))
  and right' = int_of_float (ceil (ratio *. float right)) in
  let len' = left' + right'
  and off' = left' in
  len', off' 
;;

let from_char_def cdef ratio =
  let ncols = cdef.GlFont.width
  and nrows = cdef.GlFont.height
  and hot_col = cdef.GlFont.hoffset
  and hot_row = cdef.GlFont.voffset in
  let (ncols', hot_col') = scale_size ncols hot_col ratio
  and (nrows', hot_row') = scale_size nrows hot_row ratio in
  { width = ncols' ;
    height = nrows' ;
    hoffset = hot_col' ;
    voffset = hot_row' ;
    graymap = cdef.GlFont.graymap } 
;;
