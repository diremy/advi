(*
 * advi - A DVI previewer
 * Copyright (C) 2000  Alexandre Miquel
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
 *
 * See the GNU Lesser General Public License version 2.1 for more
 * details (enclosed in the file LGPL).
 *)

open Format ;;
open Dvicommands;;

type known_status = {
   mutable hasps: bool;
   mutable bkgd_local_prefs: Grdev.bgoption list;
   mutable bkgd_prefs: Grdev.bkgd_prefs
};;

type status =
   | Unknown
   | Known of known_status

type page = {
    counters : int array ;
    commands : string;
    mutable status : status;
    mutable line : (int * string option) option;
    text : string;
  } ;;

type t = {
    preamble : preamble ;
    prelude : string ;
    pages : page array ;
    xrefs : (string, int) Hashtbl.t;
    postamble : postamble ;
    font_map : (int * font_def) list
  } ;;

exception Error of string ;;

val load : string -> t ;;
val parse_string : string -> command list ;;
val parse_page : page -> command list ;;
val string_iter : (command -> unit) -> string -> unit ;;
val page_iter : (command -> unit) -> page -> unit ;;
val page_step : (command -> unit) -> page -> (unit -> bool);;

val fprint_preamble : formatter -> preamble -> unit ;;
val fprint_postamble : formatter -> postamble -> unit ;;
val fprint_font_def : formatter -> font_def -> unit ;;
val fprint_command : formatter -> command -> unit ;;
