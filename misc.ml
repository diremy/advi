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

(* Reverse and filters list l according to f. (Pour faire plaisir à Gérard) *)
let reverse_filter f l =
  let rec filter res = function
    | [] -> res
    | a :: l -> if f a then filter (a :: res) l else filter res l in
  filter [] l;;

let reverse_map f l =
  let rec map res = function
    | [] -> res
    | a :: l -> map (f a :: res) l in
  map [] l;;

(* Concat of List-1 and List-2 is  2-tsiL :: List-1 *)
let rec reverse_concat l1 = function
  | [] -> l1
  | a :: b -> reverse_concat (a :: l1) b;;

(* Strings *)

exception False;;

let has_prefix pre str =
  let lpre = String.length pre in
  let lstr =  String.length str in
  lstr >= lpre &&
  begin
    try
      for i = 0 to lpre - 1 do if str.[i] <> pre.[i] then raise False done;
      true;
    with False -> false
  end;;

let has_suffix suf str =
  let lsuf = String.length suf in
  let lstr =  String.length str in
  lstr >= lsuf &&
  begin
    try
      let d = lstr - lsuf in
      for i = 0 to lsuf - 1 do if str.[d + i] <> suf.[i] then raise False done;
      true;
    with False -> false
  end;;

exception Match;;

let get_suffix pre str =
  let lpre = String.length pre in
  let lstr = String.length str in
  if has_prefix pre str then String.sub str lpre (lstr - lpre)
  else raise Match;;

let rec split_string s p start =
  let len = String.length s
  and i = ref start in
  while !i < len && p s.[!i] do incr i done;
  if !i >= len then [] else begin
    let i0 = !i in
    while !i < len && not (p s.[!i]) do incr i done;
    let i1 = !i in
    String.sub s i0 (i1 - i0) :: split_string s p i1
  end;;

let zap_to_char c s =
  let len = String.length s
  and i = ref 0 in
  while !i < len && s.[!i] <> c do incr i done;
  let i0 = !i+1 in
  if i0 >= len then "" else String.sub s i0 (len - i0);;

let catenate_sep sep = function 
  | [] -> ""
  | x :: l -> List.fold_left (fun s s' -> s ^ sep ^ s') x l;;

let int_or_float_of_string s =
  try int_of_string s with _ -> truncate (float_of_string s);;

let is_digit c = c >= '0' && c <= '9';;

let string_replace pat templ str =
  let result = Buffer.create (String.length str * 2) in
  let patlen = String.length pat in
  let find pat str at =
    let rec find_aux pos =
      if String.sub str pos patlen = pat then pos
      else find_aux (pos + 1) in
    try find_aux at with _ -> raise Not_found in
  let rec replace pos =
    try
      let fpos = find pat str pos in
      Buffer.add_string result (String.sub str pos (fpos - pos));
      Buffer.add_string result templ;
      replace (fpos + patlen)
    with
    | Not_found ->
        Buffer.add_string result
          (String.sub str pos (String.length str - pos));
        Buffer.contents result in
  replace 0;;

(* Fatal error in advi's code. *)
exception Fatal_error of string;;
let fatal_error x = raise (Fatal_error x);;

let handle_fatal_error f () =
  try f () with Fatal_error s -> prerr_endline s; exit 1;;

(* To emit a warning. *)
let emit_warning mes =
  Printf.fprintf stderr "Warning: %s" mes;
  prerr_newline ();;

let warnings = ref true;;

let set_warnings b = warnings := b;;

let warning s = if !warnings then emit_warning s;;

let forward_debug_endline = ref (function (s : string) -> ());;

let debug_endline s = !forward_debug_endline s;;
