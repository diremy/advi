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

val all : unit -> (string * Arg.spec * string) list;;
val add : string -> Arg.spec -> string -> unit;;
(** [add opt spec man] add the option [opt] to the command line
   with specification [spec] and man info [man]. *)

(*val pretty : ((string * Arg.spec * string) list as 'a) -> 'a;;*)
(** To pretty print options, use '\t' in the info string, the part
   of the info message starting after tabulation will be aligned. 
   Typically, the option first part of the message is the name of
   the argument to the option. If no '\t' is present, the message
   will not be aligned. *)

val debug : string -> string -> (string -> bool);;
(** [make_debug option message] create an option flag that is false by
   default and that can be set with [option] with info [message]; 
   then it returns a function to that prints its argument on stderr, 
   but only when the [option] is set. *) 

val flag : bool -> string -> string -> bool ref;;
(** [option_flag init opt mes] creates a boolean flag with [init] 
   as initial value declares the optional argument [opt] with info
   message [mes] that sets or unsets the flag, according to the value 
   of [init]. *)

val pson : bool ref;;
(** [pson] when set means do call gs for drawing inline Postscript. *)

val dops : bool ref;;
(** Temporary value, reset to [pson] when reloading the file. *)

val global_display_mode : bool ref;;
(** Tells whether display must always happen in foreground. *)
