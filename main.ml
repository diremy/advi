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

open Format;;

(*** Parsing command-line arguments ***)

let crop_flag = ref true;;
let dviname = ref None;;
let hmargin = ref (Dimension.Cm 1.0);;
let vmargin = ref (Dimension.Cm 1.0);;
let geometry = ref "864x864";;

let set_dim r s =
  r := Dimension.dimen_of_string s;;
let set_geom g =
  Dviview.set_autoresize false; geometry := g;;

let spec_list = [
  ("-geometry", Arg.String set_geom,
   "GEOM\tSets the (maximum) geometry GEOM");
  ("-g", Arg.String set_geom,
   "GEOM\tSame as -geometry GEOM");
  ("-crop", Arg.Set crop_flag,
   "\tCrop the window to the best size (default)");
  ("-nocrop", Arg.Clear crop_flag,
   "\tDisable cropping");
  ("-nomargins", Arg.Unit
     (fun () -> set_dim hmargin "0cm"; set_dim vmargin "0cm"),
   "\tSuppress horizontal and vertical margins");
  ("-hmargin", Arg.String (set_dim hmargin),
   "DIMEN\tHorizontal margin  (default: 1cm)");
  ("-vmargin", Arg.String (set_dim vmargin),
   "DIMEN\tVertical margin    (default: 1cm)");
];;

let usage_msg =
  Printf.sprintf "usage: %s [OPTIONS] DVIFILE" Sys.argv.(0);;

let set_dviname s =
  match !dviname with
  | None -> dviname := Some s
  | Some _ -> ();;

let standalone_main () =
  let sort = List.sort (fun (s1, _, _) (s2, _, _) -> compare s1 s2) in
  let options =
    Options.pretty (spec_list @ sort (Options.all ())) in
  Arg.parse options set_dviname usage_msg;
  let filename = match !dviname with
  | None -> 
      (* Test if file can be found, otherwise print console stuff. *)
      let name = Config.splash_screen in
      begin
	try let channel = open_in name in close_in channel
	with Sys_error s ->
	  eprintf "%s@.Try %s -help for more information@."
            usage_msg Sys.argv.(0);
	  Launch.exit 1
      end;
      name
  | Some s -> s in
  Dviview.set_crop !crop_flag;
  Dviview.set_hmargin !hmargin;
  Dviview.set_vmargin !vmargin;
  Dviview.set_geometry !geometry;
  Dviview.main_loop filename

let rec interactive_main () =
  printf "Dvi file name: @?";
  let filename = input_line stdin in
  if Sys.file_exists filename then begin
    Dviview.set_crop !crop_flag;
    Dviview.set_hmargin !hmargin;
    Dviview.set_vmargin !vmargin;
    Dviview.set_geometry !geometry;
    try
      Dviview.main_loop filename
    with Dviview.Error s | Failure s | Graphics.Graphic_failure s ->
      eprintf "Fatal error: %s@." s
  end else begin
    printf "File `%s' does not exists.@." filename;
    printf "Please make another choice.@.";
    interactive_main ()
  end;;

at_exit Gs.kill;;
at_exit Grdev.kill_all_embedded_apps;;

let quit = 3;;
Sys.set_signal quit (Sys.Signal_handle (fun _ -> Launch.exit 0));;

let main = if !Sys.interactive then interactive_main else standalone_main;;

Unix.putenv "GHOSTVIEW" "Test";;

(Misc.handle_fatal_error main () : unit);;
