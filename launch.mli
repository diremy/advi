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
(*  Jun Furuse, Didier R�my and Pierre Weis.                           *)
(*  Contributions by Roberto Di Cosmo, Didier Le Botlan,               *)
(*  Xavier Leroy, and Alan Schmitt.                                    *)
(*                                                                     *)
(*  Based on Mldvi by Alexandre Miquel.                                *)
(***********************************************************************)

val add_embed : (unit -> unit) -> unit;;
val add_persist : (unit -> unit) -> unit;;
val add_unmap_embed : (unit -> unit) -> unit;;

val unmap_persistent_apps : unit -> unit;;
val launch_embedded_apps : unit -> unit;;

val can_execute_command : string -> bool;;
val parse_shell_command : string -> string array
val fork_process : string -> int
val advi_process : int

val whiterun : unit -> bool
val add_whiterun_command : string -> unit
val dump_whiterun_commands : unit -> unit

val exit : int -> unit 
(* Same as [Pervasives.exit], but does not execute the functions
   registered by [at_exit] when the exiting process is forked one. 
   In the ADVI program, you MUST use this function instead of
   [Pervasives.exit] !!!! *)
