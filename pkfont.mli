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

type bitmap =
  | Packed of int * string
  | Unpacked of string ;;

(* Character definitions.

   Units are expressed in pixels or in scaled pixels.
   Note that 1 pixel = 65536 scaled pixels. *)

type char_def = {
    code : int ;
    tfm_width : int ;
    dx : int ;       (* scaled pixels *)
    dy : int ;       (* scaled pixels *)
    width : int ;    (* pixels *)
    height : int ;   (* pixels *)
    hoffset : int ;  (* pixels *)
    voffset : int ;  (* pixels *)
    mutable bitmap : bitmap
  } ;;

(* PK fonts *)

type t = {
    text : string ;
    design_size : int ;
    checksum : string ;
    hppp : int ;
    vppp : int ;
    defs : char_def list
  } ;;

val load : string -> t ;;
val find_char_def : t -> int -> char_def ;;
val unpack : char_def -> unit ;;
