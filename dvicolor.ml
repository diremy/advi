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
(*  Jun Furuse, Didier R�my and Pierre Weis.                           *)
(*  Contributions by Roberto Di Cosmo, Didier Le Botlan,               *)
(*  Xavier Leroy, and Alan Schmitt.                                    *)
(*                                                                     *)
(*  Based on Mldvi by Alexandre Miquel.                                *)
(***********************************************************************)

open Graphics;;

type color = Graphics.color;;

let named_colors = [
  "black", black;
  "white", white;
  "red", red;
  "green", green;
  "blue", blue;
  "yellow", yellow;
  "cyan", cyan;
  "magenta", magenta;
];;

let cmyk c m y k =
  let r = 255 - c
  and g = 255 - m
  and b = 255 - y in
  (* k is ignored *)
  rgb r g b;;

let parse_color_args = function
  | ["rgb"; rs; gs; bs] ->
      let r = int_of_float (255.0 *. float_of_string rs)
      and g = int_of_float (255.0 *. float_of_string gs)
      and b = int_of_float (255.0 *. float_of_string bs) in
      rgb r g b
  | ["cmyk"; cs; ms; ys; ks] ->
      let c = int_of_float (255.0 *. float_of_string cs)
      and m = int_of_float (255.0 *. float_of_string ms)
      and y = int_of_float (255.0 *. float_of_string ys)
      and k = int_of_float (255.0 *. float_of_string ks) in
      cmyk c m y k
  | ["gray"; gs] ->
      let g = int_of_float (255.0 *. float_of_string gs) in
      rgb g g g
  | [s] ->
      begin
	try 
	  let {Color.r = r; Color.g = g; Color.b = b} = Color.color_parse s in
	  rgb r g b
	with _ ->
            (* Try known colors *)
            try List.assoc (String.lowercase s) named_colors
            with Not_found ->
            (* Try an explicit 0xFFFFFF integer *)
            try int_of_string s
            (* Otherwise emit a warning and give a default *)
            with Failure _ ->
              Misc.warning (Printf.sprintf "unknown color %s@." s);
              0x000000
      end
  | _ -> 0x000000;;