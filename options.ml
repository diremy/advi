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

(* To add options to the command line in different modules way *)
let options = ref [];;

let all () = !options;;

let add option_name action man =
  options := (option_name, action, man) :: !options;;

(* A special case: flag options *)
let flag initial option_name message =
  let r = ref initial in
  add option_name
    (if initial then Arg.Clear r else Arg.Set r)
    message;
  r;;

(* Another special case: debug options *)
let make_debug r s =
  if !r then prerr_endline s;
  true;;

let debug option_name message =
  let r = ref false in
  add option_name (Arg.Set r) message;
  make_debug r;;

(* To print debugging messages. *)
let debug_endline =
  let f = debug "--debug" "General debug" in
  fun s -> ignore (f s);;

Misc.forward_debug_endline := debug_endline;;

(* Some global options *)

let pson = 
  if Config.have_gs then
    flag true
      "-nogs"
      "  Turn off the display of inlined Postscript.\
      \n\t (the default is to display inlined Postscript)."
  else ref false;;
let dops = ref !pson;;

let global_display_mode = ref false;;
let set_global_display_mode b =
  GraphicsY11.global_display_mode b;
  global_display_mode := b;;

add "-fg"
 (Arg.Unit (fun () -> set_global_display_mode true))
 "  Set the drawing policy to ``screen only'',\
 \n\t (the default is to draw both on the screen and in the memory).";;

add "-w"
 (Arg.String
    (function
     | "a" -> Misc.set_warnings false
     | "A" -> Misc.set_warnings true
     | s -> raise (Arg.Bad (Printf.sprintf "-w %s is unknown" s))))
 "<flags>  Enable/disable warnings according to <flags>,\
 \n\t A/a enable/disable all warnings";;
 \n\t (the default is \"A\", to enable all warnings).";;
