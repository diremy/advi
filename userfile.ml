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

let is_absolute path = not (Filename.is_relative path);;

let normalize path =
  let full = is_absolute path in
  let finalslash = path.[String.length path - 1] = '/' in
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
  if is_absolute path then path else normalize (Filename.concat fromdir path)
;;

(* Tilde substitution *)
(* skip to next / *)
let rec next_slash s n =
  if  n >= String.length s || s.[n] = '/' then n else
  next_slash s (succ n)
;;

let tilde_subst s =
 try
  if s = "" || s.[0] <> '~' then s else
  let len = String.length s in
  if len = 1 then Sys.getenv "HOME" else
  match s.[1] with
  | '/' -> 
     Filename.concat (Sys.getenv "HOME") (String.sub s 2 (len - 2))
  | _ ->
     let final = next_slash s 1 in
     let user = String.sub s 1 (pred final) in
     let pwnam = Unix.getpwnam user in
     if succ final >= len then pwnam.Unix.pw_dir else
      Filename.concat pwnam.Unix.pw_dir 
        (String.sub s (succ final) (len - succ final))
 with
 | Unix.Unix_error (_, _, _) -> s
 | Sys_error _ -> s
 | Not_found -> s
;;

let rec digdir dir perm =
  (* try to create the directory dir *)
  if not (Sys.file_exists dir) then
    let pdir = Filename.dirname dir in
    Misc.debug_endline ("Creating directory " ^ pdir ^ "... " );
    digdir pdir perm;
    Unix.mkdir dir perm;
    Misc.debug_endline "done"
;;

(* cautious_perm_digdir digs a directory with cautious permissions,
   i.e. drwx------ *)
let cautious_perm_digdir dir =  digdir dir 0o0700;;

let prepare_file file =
  let dirname = Filename.dirname file in
  if not (Sys.file_exists dirname) then begin
    try cautious_perm_digdir dirname
    with
    | Unix.Unix_error (e, _, _) ->
	Misc.warning (Unix.error_message e)
    end
;;

(* User preferences. *)
(* User options files. *)
let default_user_advi_dir = tilde_subst "~/.advi";;

let user_advi_dir =
  let dir = try Sys.getenv "ADVIDIR" with _ -> default_user_advi_dir in
  try tilde_subst dir with
  | _ -> "./.advi";;

let default_init_file0 = "/etc/advirc";;
let default_init_file1 = tilde_subst "~/.advirc";;
let default_init_file2 =
  tilde_subst (Filename.concat default_user_advi_dir "advirc");;
let default_init_file3 = ".advirc";;

let init_files = [
  default_init_file0;
  default_init_file1;
  default_init_file2;
  default_init_file3;
];;

let load_options_file options set_dvi_filename usage_msg fname =
 Rc.cautious_parse_file fname options set_dvi_filename usage_msg;;

let load_init_files options set_dvi_filename usage_msg =
 List.iter
   (fun fname ->
      if Sys.file_exists fname
      then Rc.cautious_parse_file fname options set_dvi_filename usage_msg)
   init_files;;

(* Cache directory *)

(* test is the directory dirname can serve as a cache directory;
   if it does not exist try to create it. *)
let can_be_cache_directory dirname =
   Sys.file_exists dirname &&
   try
     Unix.access dirname [Unix.R_OK; Unix.W_OK; Unix.X_OK; Unix.F_OK];
     true
   with
   | Unix.Unix_error _ -> false;;

let mk_user_advi_cache_dir dirname =
  cautious_perm_digdir dirname;
  if can_be_cache_directory dirname then dirname else raise Not_found;;

let default_user_advi_cache_dir =
  let d0 = Filename.concat (Unix.getcwd ()) ".advi" in
  if can_be_cache_directory d0 then d0 else
  let d1 = tilde_subst "~/.advi" in
  if can_be_cache_directory d1 then d1 else
  let d2 = Filename.concat "" (Filename.concat "tmp" ".advi") in
  if can_be_cache_directory d2 then d2 else
  try mk_user_advi_cache_dir d2 with
  | _ ->
    try mk_user_advi_cache_dir d1 with
    | _ ->
      try mk_user_advi_cache_dir d0 with
      | _ ->
        Misc.warning "Cannot find a cache directory";
        d0;;

let advi_cache_dir =
  let d = default_user_advi_cache_dir in
  Misc.debug_endline (Printf.sprintf "Using %s as cache directory." d);
  ref d;;

let set_advi_cache_dir s =
  if can_be_cache_directory s then advi_cache_dir := s else
  Misc.warning (Printf.sprintf "Cannot use %s as a cache directory" s);;

Options.add
 "-cache-dir"
 (Arg.String set_advi_cache_dir)
 "STRING\tSet the cache directory (default ./.advi)";;

let get_advi_cache_dir () = !advi_cache_dir;;

(* Writing page current number to the file advi_page_number_file. *)
let write_page_number =
 Options.flag false "-page-number"
  "Ask advi to write the current page number in a file (default is no)";;

let advi_page_number_file =
  ref (Filename.concat (get_advi_cache_dir ()) "advi_page_number");;

let set_page_number_file s = advi_page_number_file := s;;

Options.add "-page-number-file"
 (Arg.String set_page_number_file)
 "STRING\tSet the name of the file where \
  advi could write the current page number\n\
  \t(default is file \"advi_page_number\" in the cache directory\n\
  \t(default \"~/.advi\"))";;

let save_page_number n =
 if !write_page_number then
 try
   let oc = open_out !advi_page_number_file in
   output_string oc (string_of_int n);
   output_char oc '\n';
   close_out oc
 with _ ->
   Misc.warning 
     (Printf.sprintf
        "Cannot write file %s to record page number." !advi_page_number_file);;

(* Initialization of advi_page_number_file. *)
let init_advi_page_number_file () =
 if !write_page_number then prepare_file !advi_page_number_file;;

Rc.at_init init_advi_page_number_file;;
