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

exception Error of string ;;

(*** Low-level routines for reading integers ***)

let input_uint8 = input_byte ;;

let input_int8 ch =
  let n = input_byte ch in
  if n < 0x80 then n else n - 0x100 ;;

let input_uint16 ch =
  let n0 = input_byte ch in
  let n1 = input_byte ch in
  (n0 lsl 8) + n1 ;;

let input_int16 ch =
  let n0 = input_byte ch in
  let n1 = input_byte ch in
  let n = (n0 lsl 8) + n1 in
  if n < 0x8000 then n else n - 0x10000 ;;

let input_uint24 ch =
  let n0 = input_byte ch in
  let n1 = input_byte ch in
  let n2 = input_byte ch in
  (n0 lsl 16) + (n1 lsl 8) + n2 ;;

let input_int24 ch =
  let n0 = input_byte ch in
  let n1 = input_byte ch in
  let n2 = input_byte ch in
  let n = (n0 lsl 16) + (n1 lsl 8) + n2 in
  if n < 0x800000 then n else n - 0x1000000 ;;

let arch64_input_int32 ch =
  let n0 = input_byte ch in
  let n1 = input_byte ch in
  let n2 = input_byte ch in
  let n3 = input_byte ch in
  let n = (n0 lsl 24) + (n1 lsl 16) + (n2 lsl 8) + n3 in
  if n < 0x80000000 then n else n - 0x100000000 ;;

let arch32_input_int32 ch =
  let n0 = input_byte ch in
  let n1 = input_byte ch in
  let n2 = input_byte ch in
  let n3 = input_byte ch in
  (* checking if this 32-bit integer
     fits into a 31-bit Caml integer *)
  match n0 lsr 6 with
  | 0|3 -> (n0 lsl 24) + (n1 lsl 16) + (n2 lsl 8) + n3
  | 1|2 -> raise (Error "input_uint32: too large 32-bit integer")
  | _ -> assert false ;;

let input_int32 =
  match Sys.word_size with
  | 32 -> arch32_input_int32
  | 64 -> arch64_input_int32
  | size ->
      failwith
	(Printf.sprintf "Pkfont: cannot work on a %d-bit architecture" size) ;;

let input_string ch n =
  let str = String.create n in
  really_input ch str 0 n ;
  str ;;

let skip_bytes ch n =
  seek_in ch (pos_in ch + n) ;;

