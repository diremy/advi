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

let database_mtime = ref 0.0;;
let database_table = Hashtbl.create 257;;

let unwind_protect f x g y =
  try let v = f x in g y; v with
  | exc -> g y; raise exc;;

let open_process_in cmd =
  let (in_read, in_write) = Unix.pipe () in
  let pid =
    Unix.create_process "/bin/sh" [| "/bin/sh"; "-c"; cmd |]
      Unix.stdin in_write Unix.stderr in
  Unix.close in_write;
  let inchan = Unix.in_channel_of_descr in_read in
  pid, inchan;;

let close_process_in (pid, inchan) =
  close_in inchan;
  let rec wait () =
    try snd (Unix.waitpid [] pid) with
    | Unix.Unix_error(Unix.EINTR, _, _) -> wait () in
  wait ();;

exception Command;;

let command_string com opt =
  let command = Printf.sprintf "%s %s" com opt in
  Misc.debug_endline (Printf.sprintf "command_string: launching %s" command);
  try
    let pid, chan as pidchan = open_process_in command in
    unwind_protect input_line chan close_process_in pidchan 
  with
  | Unix.Unix_error (c, _, _) ->
      Misc.warning
        (Printf.sprintf "Error %s while executing %s %s"
          (Unix.error_message c) com opt);
      raise Command
  | End_of_file -> ""
;;

(* Add local path to search environment. *)
let addpath elem var kind =
  let oldv =
    try Unix.getenv var with
    | Not_found ->
        try command_string Config.kpsewhich_path
              (Printf.sprintf "-show-path='%s'" kind) with
        | Command -> "" in
  let newv = oldv ^ ":" ^ elem in
  Unix.putenv var newv
;;

addpath Config.advi_loc "PSHEADERS" Config.psheaders_kind;
addpath Config.advi_loc "TEXPICTS"  Config.texpicts_kind;;

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
      else Hashtbl.add database_table line (Filename.concat !curr_dir line)
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
  Filename.concat Config.texdir_path path
;;

let true_file_name options file =
  let args = String.concat " " (options @ [file]) in
  try
    let s = command_string Config.kpsewhich_path args in
    if s = "" then raise Command;
    s
  with
  | Command ->
      Misc.warning (Printf.sprintf "file %s is not found" file);
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
