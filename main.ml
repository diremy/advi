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

open Format;;

(*** Parsing command-line arguments ***)

let dvi_filename = ref None;;
let set_dvi_filename s = dvi_filename := Some s;;

let crop_flag = ref true;;

let hmargin = ref (Dimension.Cm 1.0);;
let vmargin = ref (Dimension.Cm 1.0);;
let geometry = ref (Ageometry.parse "864x864");;

let set_geom g = 
  Dviview.set_autoresize false; geometry := (Ageometry.parse g)
;;

let set_dim r s = r := Dimension.dimen_of_string s;;

let print_advi_version () =
  Printf.printf
      "The Active-DVI previewer and graphics presenter, version %.2f+%i (%s)"
      Config.advi_version_number Config.advi_sub_version_number
      Config.advi_version_date;
  exit 0;;

let version_spec nm =
  nm, Arg.Unit print_advi_version, "\tPrint the current Active-DVI version";;
 
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
   "DIMEN\tHorizontal margin (default: 1cm)");
  ("-vmargin", Arg.String (set_dim vmargin),
   "DIMEN\tVertical margin (default: 1cm)");
  version_spec "-v";
  version_spec "-version";
  ] in

List.iter (fun (nm, act, man) -> Options.add nm act man) spec_list;;

let usage_msg =
  Printf.sprintf "usage: %s [OPTIONS] DVIFILE" Sys.argv.(0);;

let sort_advi_options () =
  let sort = List.sort (fun (s1, _, _) (s2, _, _) -> compare s1 s2) in
  Options.pretty (sort (Options.all ()));;

let get_advi_options () =
 (* We must add an option that uses the list of options we are defining.
    We use a reference to break the cycle. *)
 let advi_options = ref [] in
 Options.add
   "-options-file"
   (Arg.String
      (fun fname ->
        Userfile.load_options_file
          !advi_options set_dvi_filename usage_msg fname))
     "STRING\tLoad this file when parsing this option to set up options\n\
      \t(to override the options of the default ~/.advirc or ~/.advi/advirc\n\
      init file).";
 advi_options := sort_advi_options ();
 !advi_options;;

let advi_options = get_advi_options ();;

let init_arguments () =
 Userfile.load_init_files advi_options set_dvi_filename usage_msg;
 Arg.parse advi_options set_dvi_filename usage_msg;
;;

let set_dvi_geometry () =
  Dviview.set_crop !crop_flag;
  Dviview.set_hmargin !hmargin;
  Dviview.set_vmargin !vmargin;
  Dviview.set_geometry !geometry
;;

(*
at_exit Gs.kill;;
*)


(* Even in case of signal, we kill embedded processes. *)
Sys.set_signal Sys.sigquit (Sys.Signal_handle (fun _ -> Launch.exit 0));;

(*
let main = if !Sys.interactive then interactive_main else standalone_main;;
*)

Unix.putenv "GHOSTVIEW" "Test";;

let standalone_init () =
  init_arguments ();
  Rc.init ();
  let filename = 
    match !dvi_filename with
    | None -> 
        (* Test if file can be found, otherwise print console stuff. *)
	let name = Config.splash_screen in
	begin
	  try let channel = open_in name in close_in channel
	  with Sys_error s ->
	    eprintf "%s@.Try %s -help for more information@."
              usage_msg Sys.argv.(0);
(*
            Launch.exit 1
*)
	    ()
	end;
	name
    | Some s -> s 
  in
  set_dvi_geometry ();
  let device = GrDev.dvidevice !geometry in
  Dviview.init device filename (* it shows the widget *)
;;

standalone_init ();;

(* To quit nicely, killing all embedded processes. *)
at_exit GrDev.destroy_all;;

try
  GMain.main ()
with
| Exit -> Launch.exit 0;;
