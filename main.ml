(*
 * advi - A DVI previewer
 * Copyright (C) 2000  Alexandre Miquel
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
 *
 * See the GNU Lesser General Public License version 2.1 for more
 * details (enclosed in the file LGPL).
 *)

open Format ;;

module Dev = Grdev ;;
module View = Dviview (* .Make(Dev) *) ;;

(*** Parsing command-line arguments ***)

let crop_flag = ref true ;;
let dviname = ref None ;;
let hmargin = ref (Dimension.Cm 1.0) ;;
let vmargin = ref (Dimension.Cm 1.0) ;;
let geometry = ref "864x864" ;;

let set_dim r s =
  r := Dimension.dimen_of_string s ;;
let set_geom g =
  View.set_autoresize false; geometry := g;;

let spec_list = [
  ("-geometry", Arg.String set_geom,
   "GEOM\tSets the (maximum) geometry GEOM") ;
  ("-g", Arg.String set_geom,
   "GEOM\tSame as -geometry GEOM") ;
  ("-crop", Arg.Set crop_flag,
   "\tCrop the window to the best size (default)") ;
  ("-no-crop", Arg.Clear crop_flag,
   "\tDisable cropping (* depreciated *)") ;
  ("-nocrop", Arg.Clear crop_flag,
   "\tDisable cropping") ;
  ("-nomargins", Arg.Unit
     (fun () -> set_dim hmargin "0cm"; set_dim vmargin "0cm"),
   "\tCancel horizontal and vertical margins") ;
  ("-hmargin", Arg.String (set_dim hmargin),
   "DIMEN\tHorizontal margin  (default: 1cm)") ;
  ("-vmargin", Arg.String (set_dim vmargin),
   "DIMEN\tVertical margin    (default: 1cm)");
] ;;

let usage_msg =
  Printf.sprintf "usage: %s [OPTIONS] DVIFILE" Sys.argv.(0) ;;

let set_dviname s =
  match !dviname with
  | None -> dviname := Some s
  | Some _ -> () ;;

let standalone_main () =
  let sort = List.sort (fun (s1, _, _) (s2, _, _) -> compare s1 s2) in
  let options =
    Misc.pretty_options (spec_list @ sort (Misc.all_options ())) in
  Arg.parse options set_dviname usage_msg ;
  let filename = match !dviname with
  | None -> 
      (* Test if file can be found, otherwise print console stuff. *)
      let name = Config.splash_screen in
      begin
	try let channel = open_in name in close_in channel
	with Sys_error s ->
	  eprintf "%s@.Try %s -help for more information@."
            usage_msg Sys.argv.(0) ;
	  exit 1
      end;
      name
  | Some s -> s in
  View.set_crop !crop_flag ;
  View.set_hmargin !hmargin ;
  View.set_vmargin !vmargin ;
  View.set_geometry !geometry ;
  try
    View.main_loop filename ; exit 0
  with View.Error s | Failure s | Graphics.Graphic_failure s ->
    Format.printf "Fatal error: %s@." s; exit 1 ;;

let rec interactive_main () =
  Format.printf "Dvi file name: @?" ;
  let filename = input_line stdin in
  if Sys.file_exists filename then begin
    View.set_crop !crop_flag ;
    View.set_hmargin !hmargin ;
    View.set_vmargin !vmargin ;
    View.set_geometry !geometry ;
    View.main_loop filename
  end else begin
    Format.printf "File `%s' does not exists.@." filename ;
    Format.printf "Please make another choice.@." ;
    interactive_main ()
  end ;;

let kill() = Gs.kill();;

let _ = at_exit kill;;
let quit = 3;;
let _ =
  Sys.set_signal quit
    (Sys.Signal_handle (fun _ -> kill(); exit 0));;

let main =
  if !Sys.interactive
  then interactive_main
  else standalone_main ;;

let _ = Unix.putenv "GHOSTVIEW" "Test";;

(Misc.handle_fatal_error main () : unit) ;;
kill();;
