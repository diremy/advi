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

(* A proper rounding function for positive floats. *)
val round : float -> int;;

type file_name = string;;
type dir_name = string;;
type line_number = int;;
type modifiers = int;;

(* filters and returns list. *)
val reverse_filter : ('a -> bool) -> 'a list -> 'a list;;
val reverse_map : ('a -> 'b) -> 'a list -> 'b list;;
val reverse_concat : 'a list -> 'a list -> 'a list;;

(* String additionals. *)
val has_prefix : string -> file_name -> bool;;
val has_suffix : string -> file_name -> bool;;
exception Match;;
val get_suffix : string -> file_name -> string;;
val split_string : string -> (char -> bool) -> int -> string list;;
val zap_to_char : char -> string -> string;;
val int_or_float_of_string : string -> int;;
val is_digit : char -> bool;;
val string_substitute_var : (char -> string) -> string -> string;;
 (** [string_substitute_var env s] Substitute any [@] one character long
     variable into string [s] according to valuation [env].*)
val contains_string : string -> string -> bool;;
 (** [contains_string s pat] Checks if string [s] contains the string pattern
     [pat]. *)
val string_prefix : char -> string -> string;;
val string_suffix : char -> string -> string;;
val filename_extension : file_name -> string;;
 (** Raise [Not_found] if [char] cannot be found in the [string] argument. *)
(* Lifting *)

val lift : ('a -> unit) -> 'a option -> unit

(* Handlers and raisers. *)
val fatal_error : string -> 'a;;
val handle_fatal_error : (unit -> unit) -> unit -> unit;;

(* Warnings. *)
val set_warnings : bool -> unit;;
val warning : string -> unit;;

(* Explicit and temporary debugging. *)
val debug_stop : string -> unit;;

(* To print a message if debugging is on. *)
val debug_endline : string -> unit;;

val push_back_key_event : char -> modifiers -> unit;;
(** [push_back_key_event c ms] pushes a key press [c]
  with a given modifier list [ms] into the Active-DVI's events queue. *)

(* The necessary forwards: not to be called directly. *)
val forward_debug_endline : (string -> unit) ref;;
val forward_push_back_key_event : (char -> int -> unit) ref;;

