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

open Graphics;;

type color = Graphics.color;;

let dvips_named_colors = [
"GreenYellow", (0.15, 0., 0.69, 0.);
"Yellow", (0., 0., 1.0, 0.);
"Goldenrod", (0., 0.10, 0.84, 0.);
"Dandelion", (0., 0.29, 0.84, 0.);
"Apricot", (0., 0.32, 0.52, 0.);
"Peach", (0., 0.50, 0.70, 0.);
"Melon", (0.0, 0.46, 0.50, 0.);
"YellowOrange", (0.0, 0.42, 1.0, 0.);
"Orange", (0.0, 0.61, 0.87, 0.);
"BurntOrange", (0.0, 0.51, 1.0, 0.);
"Bittersweet", (0.0, 0.75, 1.0, 0.24);
"RedOrange", (0.0, 0.77, 0.87, 0.);
"Mahogany", (0.0, 0.85, 0.87, 0.35);
"Maroon", (0.0, 0.87, 0.68, 0.32);
"BrickRed", (0.0, 0.89, 0.94, 0.28);
"Red", (0.0, 1.0, 1.0, 0.);
"OrangeRed", (0.0, 1.0, 0.50, 0.);
"RubineRed", (0.0, 1.0, 0.13, 0.);
"WildStrawberry", (0.0, 0.96, 0.39, 0.);
"Salmon", (0.0, 0.53, 0.38, 0.);
"CarnationPink", (0.0, 0.63, 0.0, 0.);
"Magenta", (0.0, 1.0, 0.0, 0.);
"VioletRed", (0.0, 0.81, 0.0, 0.);
"Rhodamine", (0.0, 0.82, 0.0, 0.);
"Mulberry", (0.34, 0.90, 0.0, 0.02);
"RedViolet", (0.07, 0.90, 0.0, 0.34);
"Fuchsia", (0.47, 0.91, 0.0, 0.08);
"Lavender", (0.0, 0.48, 0.0, 0.);
"Thistle", (0.12, 0.59, 0.0, 0.);
"Orchid", (0.32, 0.64, 0.0, 0.);
"DarkOrchid", (0.40, 0.80, 0.20, 0.);
"Purple", (0.45, 0.86, 0.0, 0.);
"Plum", (0.50, 1.0, 0.0, 0.);
"Violet", (0.79, 0.88, 0.0, 0.);
"RoyalPurple", (0.75, 0.90, 0.0, 0.);
"BlueViolet", (0.86, 0.91, 0.0, 0.04);
"Periwinkle", (0.57, 0.55, 0.0, 0.);
"CadetBlue", (0.62, 0.57, 0.23, 0.);
"CornflowerBlue", (0.65, 0.13, 0.0, 0.);
"MidnightBlue", (0.98, 0.13, 0.0, 0.43);
"NavyBlue", (0.94, 0.54, 0.0, 0.);
"RoyalBlue", (1.0, 0.50, 0.0, 0.);
"Blue", (1.0, 1.0, 0.0, 0.);
"Cerulean", (0.94, 0.11, 0.0, 0.);
"Cyan", (1.0, 0.0, 0.0, 0.);
"ProcessBlue", (0.96, 0.0, 0.0, 0.);
"SkyBlue", (0.62, 0.0, 0.12, 0.);
"Turquoise", (0.85, 0.0, 0.20, 0.);
"TealBlue", (0.86, 0.0, 0.34, 0.02);
"Aquamarine", (0.82, 0.0, 0.30, 0.);
"BlueGreen", (0.85, 0.0, 0.33, 0.);
"Emerald", (1.0, 0.0, 0.50, 0.);
"JungleGreen", (0.99, 0.0, 0.52, 0.);
"SeaGreen", (0.69, 0.0, 0.50, 0.);
"Green", (1.0, 0.0, 1.0, 0.);
"ForestGreen", (0.91, 0.0, 0.88, 0.12);
"PineGreen", (0.92, 0.0, 0.59, 0.25);
"LimeGreen", (0.50, 0.0, 1.0, 0.);
"YellowGreen", (0.44, 0.0, 0.74, 0.);
"SpringGreen", (0.26, 0.0, 0.76, 0.);
"OliveGreen", (0.64, 0.0, 0.95, 0.40);
"RawSienna", (0.0, 0.72, 1.0, 0.45);
"Sepia", (0.0, 0.83, 1.0, 0.70);
"Brown", (0.0, 0.81, 1.0, 0.60);
"Tan", (0.14, 0.42, 0.56, 0.);
"Gray", (0.0, 0.0, 0.0, 0.50);
"Black", (0.0, 0.0, 0.0, 1.0);
"White", (0.0, 0.0, 0.0, 0.);
];;

let gr_named_colors = [
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
  (* found at http://community.borland.com/article/0,1410,17948,00.html *)
  (* I believe this is not at all correct... 
     but there is no free altanatives. *)
  let r = if c +. k < 1.0 then 1.0 -. (c +. k) else 0.0 in
  let g = if m +. k < 1.0 then 1.0 -. (m +. k) else 0.0 in
  let b = if y +. k < 1.0 then 1.0 -. (y +. k) else 0.0 in 
  let f c = int_of_float (c *. 255.0 +. 0.5) in
  rgb (f r) (f g) (f b);;

let named_colors = gr_named_colors @ 
  (List.map (fun (n,(c,m,y,k)) -> String.lowercase n, cmyk c m y k) 
     dvips_named_colors);;

let parse_color_args = function
  | ["rgb"; rs; gs; bs] ->
      let r = int_of_float (255.0 *. float_of_string rs)
      and g = int_of_float (255.0 *. float_of_string gs)
      and b = int_of_float (255.0 *. float_of_string bs) in
      rgb r g b
  | ["cmyk"; cs; ms; ys; ks] ->
      let c = float_of_string cs
      and m = float_of_string ms
      and y = float_of_string ys
      and k = float_of_string ks in
      cmyk c m y k
  | ["gray"; gs] ->
      let g = int_of_float (255.0 *. float_of_string gs) in
      rgb g g g
  | [s] ->
      begin
	try 
	  let {Color.r = r; Color.g = g; Color.b = b} = Color.color_parse s in
          (*prerr_endline (s ^ " X color");*)
	  rgb r g b
	with _ ->
            (* Try known colors *)
            try 
            let c = List.assoc (String.lowercase s) named_colors in
            (*prerr_endline (s ^ " named color");*)
            c
            with Not_found ->
            (* Try an explicit 0xFFFFFF integer *)
            try int_of_string s
            (* Otherwise emit a warning and give a default *)
            with Failure _ ->
              Misc.warning (Printf.sprintf "unknown color %s." s);
              0x000000
      end
  | _ -> 0x000000;;
