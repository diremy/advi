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

let normalize path =
  let full = not (Filename.is_relative path) in
  let finalslash = path.[String.length path-1] = '/' in
  let tks = Misc.split_string path (function '/' -> true | _ -> false) 0 in
  let rec remove = function
    | x :: xs -> 
	begin match x :: remove xs with
	| x :: [] -> [x]
    	| "." :: xs -> xs (* remove . *)
    	| x :: ".." :: xs when x <> ".." -> xs (* remove dir/.. *)
	| l -> l
	end
    | [] -> []
  in
  let path = Misc.catenate_sep "/" (remove tks) in
  (if full then "/" else "") ^ path ^ (if finalslash then "/" else "")
;;

let fullpath fromdir path =
  (* get full path (weaker than Junix.realpath) *)
  if Filename.is_relative path then begin
    normalize (Filename.concat fromdir path)
  end else path
;;

(* Tilde substitution *)
(* skip to next / *)
let rec next_slash s n =
  if  n >= String.length s or s.[n] = '/' 
  then n
  else next_slash s (succ n);;

let tilde_subst s =
 try
  if s = "" or s.[0] <> '~' then s 
  else
    let len = String.length s in
    if len = 1 then Sys.getenv "HOME"
    else match s.[1] with
     | '/' -> 
        Filename.concat (Sys.getenv "HOME") (String.sub s 2 (len - 2))
     | _ ->
       let final = next_slash s 1 in
       let user = String.sub s 1 (pred final) in
       let pwnam = Unix.getpwnam user in
         if succ final >= len then pwnam.Unix.pw_dir
         else
          Filename.concat pwnam.Unix.pw_dir 
               (String.sub s (succ final) (len - (succ final)))
 with
  | Unix.Unix_error(_, _, _) -> s
  | Sys_error _ -> s
  | Not_found -> s
;;

let rec digdir dir perm =
  (* try to create the directory dir *)
  if Sys.file_exists dir then () 
  else begin
    let pdir = Filename.dirname dir in
    digdir pdir perm;
    Unix.mkdir dir perm
  end
;;

let prepare_file file =
  let dirname = Filename.dirname file in
  if Sys.file_exists dirname then ()
  else begin
    Misc.debug_endline ("Creating directory " ^ dirname ^ "... " );
    try 
      digdir dirname 0o700;
      Misc.debug_endline "done"
    with
    | Unix.Unix_error (e, _, _) ->
	prerr_endline (Unix.error_message e)
  end
;;

(* User preferences and cache handling *)

let default_user_dir = "~/.advi"
;;

let user_dir = 
  let dir = try Sys.getenv "ADVIDIR" with _ -> default_user_dir in
  try
    tilde_subst dir
  with
  | _ -> "./.advi"
;;

let cache_dir =
  if true then
    Filename.concat (Unix.getcwd ()) ".advi"
  else
    Filename.concat user_dir "cache"
;;

let advi_page_no_file = Filename.concat cache_dir "advi_page_no";;

let save_page_no n =
 try
   let oc = open_out advi_page_no_file in
   output_string oc (string_of_int n);
   output_char oc '\n';
   close_out oc
 with _ ->
   Misc.warning 
    (Printf.sprintf
       "Cannot write file %s to record page number." advi_page_no_file);;
