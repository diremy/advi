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
   | Free | Busy | Pause | Disk | Question | Selection | Move
   | Resize | Resize_x | Resize_y
;;

let free_cursor = GraphicsY11.Cursor_left_ptr;;
let busy_cursor = GraphicsY11.Cursor_watch;;
let pause_cursor = GraphicsY11.Cursor_right_side;;
let disk_cursor = GraphicsY11.Cursor_exchange;;
let question_cursor = GraphicsY11.Cursor_question_arrow;;
let selection_cursor = GraphicsY11.Cursor_xterm;;
let move_cursor = GraphicsY11.Cursor_fleur;;
let resize_cursor = GraphicsY11.Cursor_diamond_cross;;
let resize_cursor_x = GraphicsY11.Cursor_sb_h_double_arrow;;
let resize_cursor_y = GraphicsY11.Cursor_sb_v_double_arrow;;

let set_cursor, restore_cursor, last_cursor =
  let last_cursor = ref free_cursor in
  (function cursor -> last_cursor := cursor; GraphicsY11.set_cursor cursor),
  (function () -> GraphicsY11.set_cursor !last_cursor),
  (function () -> !last_cursor);;

(* To be called before system calls that may take a long time *)
let busy_timeout = ref None;;
let set_busy_cursor () = set_cursor GraphicsY11.Cursor_watch;;

(** Starts a timer which triggers the indication of a busy state. *)
let start_timer () =
  try
    busy_timeout := Some (Timeout.add !busy_delay set_busy_cursor)
  with
  | _ -> ();;

(* Stop the busy cursor, remove the timer if any and restore previous cursor. *)

let stop_busy () =
  match !busy_timeout with
  | Some timeout ->
      begin try Timeout.remove timeout with Not_found -> () end
  | None -> ();;

let non_busy cursor =
  stop_busy (); 
  set_cursor cursor;;

(* Set the cursor when we want to reflect the state as a cursor modification. *)
let set = function
  | Pause -> non_busy pause_cursor
  | Free -> non_busy free_cursor
  | Disk -> set_cursor disk_cursor
  | Busy -> if !show_busy then start_timer ()
  | Question -> non_busy question_cursor
  | Selection -> non_busy selection_cursor
  | Move -> set_cursor move_cursor
  | Resize -> set_cursor resize_cursor
  | Resize_x -> set_cursor resize_cursor_x
  | Resize_y -> set_cursor resize_cursor_y;;

let temp_set c =
  stop_busy ();
  let c =
    match c with
    | Pause -> pause_cursor
    | Free -> free_cursor
    | Disk -> disk_cursor
    | Busy -> busy_cursor
    | Question -> question_cursor
    | Selection -> selection_cursor
    | Move -> move_cursor
    | Resize -> resize_cursor
    | Resize_x -> resize_cursor_x
    | Resize_y -> resize_cursor_y in
  GraphicsY11.set_cursor c;;

let stop () =
 non_busy (last_cursor ());;

