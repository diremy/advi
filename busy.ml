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

let show_busy =
    Options.flag true
    "-nowatch"
    "\tDon't display a watch when busy";;

let busy_delay = ref 0.5;;

Options.add
  "-watch"
  (Arg.Float (fun x -> busy_delay := x))
  (Printf.sprintf
     "FLOAT\tDelay before the watch cursor appears (default %fs)" !busy_delay);;

type busy =
   | Free | Busy | Pause | Disk | Question | Selection | Move;;

let free_cursor = GraphicsY11.Cursor_left_ptr;;
let busy_cursor = GraphicsY11.Cursor_watch;;
let pause_cursor = GraphicsY11.Cursor_right_side;;
let disk_cursor = GraphicsY11.Cursor_exchange;;
let question_cursor = GraphicsY11.Cursor_question_arrow;;
let selection_cursor = GraphicsY11.Cursor_xterm;;
let move_cursor = GraphicsY11.Cursor_fleur;;

let set_cursor, restore_cursor =
  let last_cursor = ref free_cursor in
  (function cursor ->
    last_cursor := cursor;
    GraphicsY11.set_cursor cursor),
  (function () ->
    GraphicsY11.set_cursor !last_cursor);;

(* To be called before system calls that make take a long time *)
let busy_timeout = ref None;;
let set_busy_cursor () = set_cursor GraphicsY11.Cursor_watch;;
let start_timer () =
  try
    busy_timeout := Some (Timeout.add !busy_delay set_busy_cursor)
  with
  | _ -> ();;

(* Stop the busy cursor, remove the timer if any and restore previous cursor. *)
let stop_busy cursor =
  set_cursor cursor;
  match !busy_timeout with
  | Some timeout ->
      begin try Timeout.remove timeout with Not_found -> () end
  | None -> ();;

(* Set the cursor when we want to refect the state as a cursor modification. *)
let set = function
  | Pause -> stop_busy pause_cursor
  | Free -> stop_busy free_cursor
  | Disk -> set_cursor disk_cursor
  | Busy -> if !show_busy then start_timer ()
  | Question -> stop_busy question_cursor
  | Selection -> stop_busy selection_cursor
  | Move -> set_cursor move_cursor;;


