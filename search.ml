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

let temp_filename = Filename.temp_file "advi" "";;
let database_mtime = ref 0.0;;
let database_table = Hashtbl.create 257;;

(* Add local path to search environment. *)

exception Command;;

let command_string com opt =
  let command = Printf.sprintf "%s %s > %s" com opt temp_filename in
  let exit_status = Sys.command command in
  if exit_status <> 0 then raise Command else
  try
    let ch = open_in temp_filename in
    try
      let res = input_line ch in
      close_in ch;
      res
    with x -> close_in ch; raise x
  with _ ->
    Misc.warning
      (Printf.sprintf "Error %d while executing %s %s" exit_status com opt);
    raise Command
;;

let addpath elem var kind =
  let oldv =
    try Unix.getenv var with
    | Not_found ->
        try command_string Config.kpsewhich_path
              (Printf.sprintf "-show-path='%s'" kind)
        with Command -> "" in
  let newv = oldv ^ ":" ^ elem in
  Unix.putenv var newv
;;

addpath Config.advi_loc "PSHEADERS" Config.psheaders_kind;
addpath Config.advi_loc "TEXPICTS"  Config.texpicts_kind;;

at_exit (fun () -> try Sys.remove temp_filename with _ -> ());;

let is_space = function
  | ' ' | '\n' | '\r' | '\t' -> true
  | _ -> false
;;

let remove_spaces line =
  let len = String.length line in
  let p = ref 0 and q = ref len in
  while !p < len && is_space line.[!p] do incr p done;
  while !q > !p && is_space line.[!q - 1] do decr q done;
  if !p = 0 && !q = len then line else
  String.sub line !p (!q - !p)
;;

let reload_database () =
  let ch = open_in Config.database_path in
  Hashtbl.clear database_table;
  let curr_dir = ref "" in
  try while true do
    let line = remove_spaces (input_line ch) in
    let len = String.length line in
    if len > 0 && line.[0] <> '%' then begin
      if line.[len - 1] = ':'
      then curr_dir := String.sub line 0 (len - 1)
      else Hashtbl.add database_table line (!curr_dir ^ "/" ^ line)
    end
  done with
  | End_of_file -> close_in ch
  | e -> close_in ch; raise e
;;

let reload_if_changed_database () =
  try
    let stats = Unix.stat Config.database_path in
    let mtime = stats.Unix.st_mtime in
    if mtime > !database_mtime then begin
      database_mtime := mtime;
      reload_database ()
    end
  with _ -> ()
;;

let database_font_path fontname dpi =
  reload_if_changed_database ();
  let name = Printf.sprintf "%s.%dpk" fontname dpi in
  let path = Hashtbl.find database_table name in
  Config.texdir_path ^ "/" ^ path
;;

let true_file_name options file =
  let args = String.concat " " (options @ [file]) in
  try command_string Config.kpsewhich_path args
  with
  | Command ->
      Misc.warning (Printf.sprintf "%s is not found" file);
      raise Not_found
;;

let true_file_names options files =
  List.fold_right
    (fun file st ->
      try true_file_name options file :: st with Not_found -> st)
    files []
;;

let kpsewhich_font_path fontname dpi =
  match true_file_names ["-dpi=" ^ string_of_int dpi; "-mktex=pk"]
                        [fontname ^ ".pk"] with
  | name :: _ when name <> "" -> name
  | _ -> raise Not_found
;;

let unix_font_path fontname dpi =
  try database_font_path fontname dpi
  with _ -> kpsewhich_font_path fontname dpi
;;

let msdos_font_path fontname dpi =
  let filename = Printf.sprintf "%s.%dpk" fontname dpi in
  try Filename.concat (Sys.getenv "PKFONTS") filename
  with _ -> filename
;;

let font_path =
  match Sys.os_type with
  | "Unix" -> unix_font_path
  | "Win32" -> msdos_font_path
  | _ -> raise Not_found
;;
