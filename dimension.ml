(*
 * advi - A DVI previewer
 * Copyright (C) 2001 INRIA
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
type dimen =
   | Px of int
   | Pt of float
   | Pc of float
   | In of float
   | Bp of float
   | Cm of float
   | Mm of float
   | Dd of float
   | Cc of float
   | Sp of int;;

let is_digit c = c >= '0' && c <= '9';;

let dimen_of_string str =
  let len = String.length str in
  let i = ref 0 in
  while !i < len && is_digit str.[!i] do incr i done ;
  if !i < len && str.[!i] = '.' then begin
    incr i ;
    while !i < len && is_digit str.[!i] do incr i done
  end ;
  let (pref, suff) =
    (String.sub str 0 !i, String.sub str !i (len - !i)) in
  let f = float_of_string pref in
  match suff with
  | "" -> Px (int_of_float f)
  | "pt" -> Pt f
  | "pc" -> Pc f
  | "in" -> In f
  | "bp" -> Bp f
  | "cm" -> Cm f
  | "mm" -> Mm f
  | "dd" -> Dd f
  | "cc" -> Cc f
  | "sp" -> Sp (int_of_float f)
  | _ -> invalid_arg (Printf.sprintf "unknown unit `%s'" suff);;

let normalize = function
  | Px n -> Px n
  | Pt f -> In (Units.from_to Units.PT Units.IN f)
  | Pc f -> In (Units.from_to Units.PC Units.IN f)
  | In f -> In f
  | Bp f -> In (Units.from_to Units.BP Units.IN f)
  | Cm f -> In (Units.from_to Units.CM Units.IN f)
  | Mm f -> In (Units.from_to Units.MM Units.IN f)
  | Dd f -> In (Units.from_to Units.DD Units.IN f)
  | Cc f -> In (Units.from_to Units.CC Units.IN f)
  | Sp n -> In (Units.from_to Units.SP Units.IN (float n));;

