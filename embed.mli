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

(* Embedded applications *)
type app_mode =
   | Sticky
      (* A [Sticky] application is launched once and only once.
         They are never killed when a new slide is visualized.
         As soon as launched, a [Sticky] application remains visible
         throughout the show.
         To do: add a tag name for each [Sticky] application, in order
         to kill it on demand. *)
   | Persistent
      (* A [Sticky] application that is not visible out of the slide
         that launched it. A [Persistent] application is launched once
         and only once and keeps running throughout the show.
         A [Persistent] application must be embeddable, since its
         window must be mapped and unmapped. *)
   | Ephemeral
      (* An [Ephemeral] is an application that is launched when the
         page it appears in is visualised. It is killed when going
         to another slide. If the page is visible again, the
         application will be launched again as well. *)
;;

val embed_app :
  string -> app_mode -> string -> int -> int -> int -> int -> unit;;
val kill_embedded_app : int -> string -> unit;;
val kill_all_embedded_apps : unit -> unit;;
val kill_ephemeral_apps : unit -> unit;;
