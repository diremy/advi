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

(* The module that changes the cursor to reflect the internal state of
   the program. *)

type busy =
   | Free | Busy | Pause | Disk | Question | Selection;;
(** Different states of the computation. *)

val start_timer : unit -> unit;;
(** Starts a timer which triggers the indication of a busy state. *)
val restore_cursor : unit -> unit;;
(** Restore the last cursor saved by a busy timer. *)
val set : busy -> unit;;
(** Remove the busy timer and set the given cursor. *)
