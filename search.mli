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

(* [font_path fontname dpi] returns a filename path corresponding
   to the PK file for font [fontname] at resolution [dpi].
   It raises [Not_found] if such a path could not be found.

   Note that there is no warranty that the returned path corresponds
   to a real file, or that the corresponding file is a valid PK file
   for the given font at the given resolution. *)
val font_path : string -> int -> string ;;

(* [true_file_name OPTIONS FILE] return the true FILE name
   for a file (call kpsewitch)  under OPTIONS *)
val true_file_name : string list -> string -> string

(* [true_file_names OPTIONS FILES] return the list of true FILES names
   for files (call kpsewitch)  under OPTIONS *)
val true_file_names : string list -> string list -> string list
