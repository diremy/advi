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

(* The module that changes the cursor to reflect the internal state of
   the program. *)

type busy =
   | Free | Busy | Pause | Disk | Question | Selection | Move
   | Resize | Resize_w | Resize_h | Resize_d
(** Different states of the computation. *)

val start_timer : unit -> unit;;
(** Starts a timer which triggers the indication of a busy state. *)

val restore_cursor : unit -> unit;;
(** Restore the last cursor saved by a busy timer. *)

val set : busy -> unit;;
(** Set the cursor to reflect the given busy state.
    Remove the busy timer if necessary, record the current cursor,
    and set the cursor to the appropriate value. *)

val temp_set : busy -> unit;;
(** Temporary set: set but do not record the given cursor. *)

val stop : unit -> unit;;
(** Remove the busy timer and reset the last cursor set by [Busy.set]. *)
