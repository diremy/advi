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

(* Saving shots. *)

open Image;;

type x = int and y = int and w = int and h = int;;

let get_file_prefix, set_file_prefix =
 let file_prefix = ref "shot" in
 (fun () -> !file_prefix),
 (fun s -> file_prefix := s)
;;

Options.add
  "-image-file-prefix"
  (Arg.String set_file_prefix)
  (Printf.sprintf
     "<string>  set to <string> the prefix of file images\
     \n\t saved by ^X-^S during the presentation,\
     \n\t (the default is %s)." (get_file_prefix ()));;

(* File numbering counter. *)
let get_file_number, incr_file_number, set_file_number =
  let cntr = ref 0 in
  (fun () -> !cntr),
  (fun () -> incr cntr),
  (fun i -> cntr := i);;

Options.add
  "-first-image-file-number"
  (Arg.Int set_file_number)
  (Printf.sprintf
     "<int>  set to <int> the starting number of file images\
     \n\t saved by ^X-^S during the presentation,\
     \n\t (the default is %d)." (get_file_number ()));;

let new_fname () =
  let fname =
    Printf.sprintf "%s%d.jpg" (get_file_prefix ()) (get_file_number ()) in
  incr_file_number ();
  fname;;

let output_area fname x y w h =
  let img = Graphic_image.get_image x y w h in
  Image.save fname None [] (Rgb24 img);;

let save_area_file fname x y w h =
  let screen_w = Graphics.size_x () and screen_h = Graphics.size_y () in
  if x >= 0 && x <= screen_w &&
     y >= 0 && y <= screen_h &&
     h >= 0 && y + h <= screen_h &&
     w >= 0 && x + w <= screen_h then output_area fname x y w h;;

let save_area x y w h =
  let fname = new_fname () in
  save_area_file fname x y w h;;

let save_page_file fname =
  let w = Graphics.size_x () and h = Graphics.size_y () in
  output_area fname 0 0 w h;;

let save_page () =
  let fname = new_fname () in
  save_page_file fname;;
