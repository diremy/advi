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

open Graphics;;

type color = Graphics.color;;

let cmyk c m y k = 
  (* found at http://community.borland.com/article/0,1410,17948,00.html *)
  (* I believe this is not at all correct... 
     but there is no free altanatives. *)
  let r = if c +. k < 1.0 then 1.0 -. (c +. k) else 0.0 in
  let g = if m +. k < 1.0 then 1.0 -. (m +. k) else 0.0 in
  let b = if y +. k < 1.0 then 1.0 -. (y +. k) else 0.0 in 
  let f c = Misc.round (c *. 255.0) in
  rgb (f r) (f g) (f b);;

let dvips_named_colors = [
  "GreenYellow", (cmyk 0.15 0. 0.69 0.);
  "Yellow", (cmyk 0. 0. 1.0 0.);
  "Goldenrod", (cmyk 0. 0.10 0.84 0.);
  "Dandelion", (cmyk 0. 0.29 0.84 0.);
  "Apricot", (cmyk 0. 0.32 0.52 0.);
  "Peach", (cmyk 0. 0.50 0.70 0.);
  "Melon", (cmyk 0.0 0.46 0.50 0.);
  "YellowOrange", (cmyk 0.0 0.42 1.0 0.);
  "Orange", (cmyk 0.0 0.61 0.87 0.);
  "BurntOrange", (cmyk 0.0 0.51 1.0 0.);
  "Bittersweet", (cmyk 0.0 0.75 1.0 0.24);
  "RedOrange", (cmyk 0.0 0.77 0.87 0.);
  "Mahogany", (cmyk 0.0 0.85 0.87 0.35);
  "Maroon", (cmyk 0.0 0.87 0.68 0.32);
  "BrickRed", (cmyk 0.0 0.89 0.94 0.28);
  "Red", (cmyk 0.0 1.0 1.0 0.);
  "OrangeRed", (cmyk 0.0 1.0 0.50 0.);
  "RubineRed", (cmyk 0.0 1.0 0.13 0.);
  "WildStrawberry", (cmyk 0.0 0.96 0.39 0.);
  "Salmon", (cmyk 0.0 0.53 0.38 0.);
  "CarnationPink", (cmyk 0.0 0.63 0.0 0.);
  "Magenta", (cmyk 0.0 1.0 0.0 0.);
  "VioletRed", (cmyk 0.0 0.81 0.0 0.);
  "Rhodamine", (cmyk 0.0 0.82 0.0 0.);
  "Mulberry", (cmyk 0.34 0.90 0.0 0.02);
  "RedViolet", (cmyk 0.07 0.90 0.0 0.34);
  "Fuchsia", (cmyk 0.47 0.91 0.0 0.08);
  "Lavender", (cmyk 0.0 0.48 0.0 0.);
  "Thistle", (cmyk 0.12 0.59 0.0 0.);
  "Orchid", (cmyk 0.32 0.64 0.0 0.);
  "DarkOrchid", (cmyk 0.40 0.80 0.20 0.);
  "Purple", (cmyk 0.45 0.86 0.0 0.);
  "Plum", (cmyk 0.50 1.0 0.0 0.);
  "Violet", (cmyk 0.79 0.88 0.0 0.);
  "RoyalPurple", (cmyk 0.75 0.90 0.0 0.);
  "BlueViolet", (cmyk 0.86 0.91 0.0 0.04);
  "Periwinkle", (cmyk 0.57 0.55 0.0 0.);
  "CadetBlue", (cmyk 0.62 0.57 0.23 0.);
  "CornflowerBlue", (cmyk 0.65 0.13 0.0 0.);
  "MidnightBlue", (cmyk 0.98 0.13 0.0 0.43);
  "NavyBlue", (cmyk 0.94 0.54 0.0 0.);
  "RoyalBlue", (cmyk 1.0 0.50 0.0 0.);
  "Blue", (cmyk 1.0 1.0 0.0 0.);
  "Cerulean", (cmyk 0.94 0.11 0.0 0.);
  "Cyan", (cmyk 1.0 0.0 0.0 0.);
  "ProcessBlue", (cmyk 0.96 0.0 0.0 0.);
  "SkyBlue", (cmyk 0.62 0.0 0.12 0.);
  "Turquoise", (cmyk 0.85 0.0 0.20 0.);
  "TealBlue", (cmyk 0.86 0.0 0.34 0.02);
  "Aquamarine", (cmyk 0.82 0.0 0.30 0.);
  "BlueGreen", (cmyk 0.85 0.0 0.33 0.);
  "Emerald", (cmyk 1.0 0.0 0.50 0.);
  "JungleGreen", (cmyk 0.99 0.0 0.52 0.);
  "SeaGreen", (cmyk 0.69 0.0 0.50 0.);
  "Green", (cmyk 1.0 0.0 1.0 0.);
  "ForestGreen", (cmyk 0.91 0.0 0.88 0.12);
  "PineGreen", (cmyk 0.92 0.0 0.59 0.25);
  "LimeGreen", (cmyk 0.50 0.0 1.0 0.);
  "YellowGreen", (cmyk 0.44 0.0 0.74 0.);
  "SpringGreen", (cmyk 0.26 0.0 0.76 0.);
  "OliveGreen", (cmyk 0.64 0.0 0.95 0.40);
  "RawSienna", (cmyk 0.0 0.72 1.0 0.45);
  "Sepia", (cmyk 0.0 0.83 1.0 0.70);
  "Brown", (cmyk 0.0 0.81 1.0 0.60);
  "Tan", (cmyk 0.14 0.42 0.56 0.);
  "Gray", (cmyk 0.0 0.0 0.0 0.50);
  "Black", (cmyk 0.0 0.0 0.0 1.0);
  "White", (cmyk 0.0 0.0 0.0 0.);
];;

let gr_named_color = [
  "black", black;
  "white", white;
  "red", red;
  "green", green;
  "blue", blue;
  "yellow", yellow;
  "cyan", cyan;
  "magenta", magenta;
];;

let named_colors =
  let color_htable = Hashtbl.create 83 in
  let add_named_color (n, c) =
    Hashtbl.add color_htable (String.lowercase n) c in
  List.iter add_named_color gr_named_color;
  List.iter add_named_color dvips_named_colors;
  color_htable;;

let find_named_color n = Hashtbl.find named_colors (String.lowercase n);;

let default_color = find_named_color "gray";;

let parse_color s =
  (*prerr_endline ("Parsing " ^ s);*)
  (* Try known colors *)
  try
    let c = find_named_color s in
    (*prerr_endline (s ^ " named color");*)
    c
  with Not_found ->
    try
      let {Color.r = r; Color.g = g; Color.b = b} = Color.color_parse s in
      (*prerr_endline (s ^ " X color");*)
      rgb r g b
    with _ ->
      (* Try an explicit 0xFFFFFF integer *)
      int_of_string s;;

let cautious_parse_color s =
  (* Try the regular way first. *)
  try parse_color s
  (* It it fails, emit a warning and give a default gray *)
  with Failure _ ->
    Misc.warning (Printf.sprintf "unknown color %s." s);
    default_color;;

let parse_color_args = function
  | [("rgb" as s); rs; gs; bs] ->
      (try
         let r = Misc.round (* int_of_float *) (255.0 *. float_of_string rs)
         and g = Misc.round (* int_of_float *) (255.0 *. float_of_string gs)
         and b = Misc.round (* int_of_float *) (255.0 *. float_of_string bs) in
         rgb r g b
       with
       | Failure _ ->
          Misc.warning
            (Printf.sprintf "cannot understand %s color parameters." s);
          default_color)
  | [("cmyk" as s); cs; ms; ys; ks] ->
      (try
         let c = float_of_string cs
         and m = float_of_string ms
         and y = float_of_string ys
         and k = float_of_string ks in
         cmyk c m y k
       with
       | Failure _ ->
          Misc.warning
            (Printf.sprintf "cannot understand %s color parameters." s);
          default_color)
  | [("gray" | "grey" as s); gs] ->
      (try 
         let g = Misc.round (* int_of_float *) (255.0 *. float_of_string gs) in
         rgb g g g
       with
       | Failure _ ->
          Misc.warning
            (Printf.sprintf "cannot understand %s color parameters." s);
          default_color)
  | [s] -> cautious_parse_color s
  | _ -> default_color;;
