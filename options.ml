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
    ("\t" ^ message);
  r;;

(* A special case: debug options *)
let make_debug ?label r s = 
  if !r then 
    match label with
    | None -> prerr_endline s
    | Some l -> prerr_endline (l ^ ": " ^ s)
;;

let debug ?label option_name message =
  let r = ref false in
  add option_name (Arg.Set r) ("\t" ^ message);
  make_debug ?label r;;

(* To print debugging messages. *)
let debug_endline =
  let f = debug "-debug" "General debug" in
  fun s -> ignore (f s);;

Misc.forward_debug_endline := debug_endline;;


(* Command line options *)

let pretty all_options = 
  let tab (o, s, m) =
    let s = Misc.split_string m (function '\t' -> true | _ -> false) 0 in
    if String.length m > 0 && m.[0] = '\t' then "" :: s else s in
  let tab_options = List.map tab all_options in
  let width =
    2 + 
    List.fold_left2
      (fun w (o, _, _) -> function
        | [] | _ :: [] -> w
        | m :: _ -> max w (String.length o + String.length m))
      0 all_options tab_options in
  let margin = "\n" ^ String.make (width + 1) ' ' in
  let indent o = function
    | [] -> assert false
    | [h] -> h
    | h :: m :: t ->
        let length = width - String.length o - String.length h in
        let hm = h ^ String.make length ' ' ^ m in
        if t = [] then hm
        else hm ^ String.concat margin t in
  List.map2 (fun  (o, s, _ ) ml -> (o, s, indent o ml))
    all_options tab_options;;

(* Some global options *)

let pson = 
  if Config.have_gs then
    flag true
      "-nogs" "Turn off display of inlined Postscript"
  else ref false;;
let dops = ref !pson;;

let global_display_mode = ref false;;
let set_global_display_mode b =
  raise (Failure "set_global_display_mode")
;;

add "-fg"
 (Arg.Unit (fun () -> set_global_display_mode true))
 "\tDraw in the foreground";;

add "-w"
 (Arg.String
    (function
     | "a" -> Misc.set_warnings false
     | "A" -> Misc.set_warnings true
     | s -> raise (Arg.Bad (Printf.sprintf "-w %s is unknown" s))))
 "STRING\tA/a enable/disable all warnings";;
