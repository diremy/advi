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

(* The current version of Active-DVI *)
let advi_version_number = PACKAGE_VERSION;;
(* let advi_build_date = BUILD_DATE;; *)

let advi_full_version = advi_version_number
  (* Printf.sprintf "%s (build %s)" advi_version_number advi_build_date;; *)

(* Can we use gs ? *)
let have_gs = HAVE_GS;;

(* Can we use camlimages ? Not used anymore? What happens if camlimages 
   is not there? *) 
let have_camlimages = HAVE_CAMLIMAGES;;

(* Various commands paths. *)
let kpsewhich_path = "kpsewhich";;
let gs_path = ref GS_PATH;;
let gzip_path = "gzip";;
let gunzip_path = "gunzip";;
let bunzip2_path = "bunzip2";;

(* Various configurations for PS and TeX *)
let texpicts_kind = "graphic/figure";;
let psheaders_kind = "PostScript header";;

(* Data files directory paths. *)
let texdir_path = "";;
let database_name = "ls-R"
let database_path = Filename.concat texdir_path database_name;;

(* Location of splash.dvi and test/caml.eps *)
let advi_loc = "ADVI_TEXDIR";;
let etc_advi_loc = "ADVI_TEXDIR";;
let splash_screen = Filename.concat advi_loc "splash.dvi";;
let scratch_draw_splash_screen =
  Filename.concat advi_loc "scratch_draw_splash.dvi";;
let scratch_write_splash_screen =
  Filename.concat advi_loc "scratch_write_splash.dvi";;
