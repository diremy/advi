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

(* filters and returns list. *)
val reverse_filter : ('a -> bool) -> 'a list -> 'a list;;
val reverse_map : ('a -> 'b) -> 'a list -> 'b list;;
val reverse_concat : 'a list -> 'a list -> 'a list;;

val has_prefix : string -> string -> bool;;
val has_suffix : string -> string -> bool;;
exception Match;;
val get_suffix : string -> string -> string;;
val split_string : string -> (char -> bool) -> int -> string list;;
val zap_to_char : char -> string -> string;;
val catenate_sep : string -> string list -> string;;
val int_or_float_of_string : string -> int;;
val is_digit : char -> bool;;

val fatal_error : string -> 'a;;
val handle_fatal_error : (unit -> unit) -> unit -> unit;;

val warning : string -> unit;;

(* 
   [debug mes] prints mes to [stderr] if debug mode is on 
*)
    
val debug_endline : string -> unit;;

val forward_debug_endline : (string -> unit) ref;;
