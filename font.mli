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

type char_def = {
    code : int ;
    dx : int ;
    dy : int ;
    width : int ;
    height : int ;
    hoffset : int ;
    voffset : int ;
    bitmap : string
  } ;;

type t ;;

val find : string -> int -> t ;;
val find_char_def : t -> int -> char_def ;;
