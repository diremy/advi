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
  let path = String.concat "/" (remove tks) in
  Printf.sprintf "%s%s%s"
    (if full then "/" else "") path (if finalslash then "/" else "")
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

let user_home_dir = try Sys.getenv "HOME" with _ -> "~";;

let tilde_subst s =
 try
  if s = "" || s.[0] <> '~' then s else
  let len = String.length s in
  if len = 1 then user_home_dir else
  match s.[1] with
  | '/' -> 
     Filename.concat user_home_dir (String.sub s 2 (len - 2))
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
let cautious_perm_digdir dir = digdir dir 0o0700;;

let prepare_file file =
  let dirname = Filename.dirname file in
  if not (Sys.file_exists dirname) then begin
    try cautious_perm_digdir dirname
    with
    | Unix.Unix_error (e, _, _) ->
	Misc.warning (Unix.error_message e)
    end
;;

(* The DVI file currently read. *)
let dvi_filename = ref None;;
let set_dvi_filename s = dvi_filename := Some s;;
let get_dvi_filename () =
  match !dvi_filename with
  | None -> raise Not_found
  | Some fname -> fname;;

let get_dvi_file_dirname () =
  match !dvi_filename with
  | None -> raise Not_found
  | Some fname -> Filename.dirname fname;;

(* User preferences. *)
(* User options files. *)
let default_user_advi_dir = Filename.concat user_home_dir ".advi";;

let user_advi_dir =
  let dir =
    try Sys.getenv "ADVIDIR" with _ -> default_user_advi_dir in
  try tilde_subst dir with
  | _ -> ".advi";;

let default_init_file0 = "/etc/advirc";;
let default_init_file1 = Filename.concat user_home_dir ".advirc";;
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
      then load_options_file options set_dvi_filename usage_msg fname)
   init_files;;

(* Cache directory *)

(* Test if the directory dirname can serve as a cache directory *)
let can_be_cache_directory dirname =
   Sys.file_exists dirname &&
   try
     Unix.access dirname [Unix.R_OK; Unix.W_OK; Unix.X_OK; Unix.F_OK];
     true
   with
   | Unix.Unix_error _ -> false;;

(* Try to dig a directory to serve as cache,
   Raise Not_found if the directory still cannot be
   used as a cache directory. *)
let mk_user_advi_cache_dir dirname =
  cautious_perm_digdir dirname;
  if can_be_cache_directory dirname then dirname else raise Not_found;;

let get_user_advi_cache_dir () =
  let add_user_defined_tmp dirs = 
    try Sys.getenv "TMPDIR" :: dirs with
    | Not_found -> dirs in
  let add_system_tmp_dir dirs =
    Filename.dirname (Filename.temp_file "" "") :: dirs in
  let add_current_dvi_dirname dirs =
    try get_dvi_file_dirname () :: dirs with
    | Not_found -> dirs in
  let add_user_tmp_dir dirs =
    Filename.concat user_home_dir ".advi" :: dirs in
  let dirs =
    add_user_defined_tmp
     (add_system_tmp_dir
       (add_current_dvi_dirname
         (add_user_tmp_dir []))) in
  try List.find can_be_cache_directory dirs with
  | Not_found ->
      let rec find_cache_dir = function
      | [] ->
          Misc.warning "Cannot find a cache directory, try to use .";
          Unix.getcwd ()
      | d :: ds ->
         try mk_user_advi_cache_dir d with
         | _ -> find_cache_dir ds in
      find_cache_dir dirs;;    

let advi_cache_dir = ref None;;

let set_advi_cache_dir d =
  if can_be_cache_directory d then begin
    Misc.debug_endline (Printf.sprintf "Using %s as cache directory." d);
    advi_cache_dir := Some d end else
  Misc.warning (Printf.sprintf "Cannot use %s as a cache directory" d);;

Options.add
 "-cache-dir"
 (Arg.String set_advi_cache_dir)
 "STRING\tSet the cache directory (default /tmp)";;

(* Get the actual advi cache directory.
   If it has not yet been set (i.e. it was not specified on the command
   line or in init files), try to figure out a reasonable default:
   - if user's environment variable TMPDIR is set and the corresponding
     directory is writtable, use it,
   - otherwise, if /tmp can serve as a cache use it,
   - otherwise, if the current file directory
     is possible use it, 
   - otherwise, if $HOME/.advi is available use it,
   - otherwise, if none of those is possible, try to use the current
     working directory as a cache.
*)
let get_advi_cache_dir () =
  match !advi_cache_dir with
  | Some d -> d
  | None ->
      let d = get_user_advi_cache_dir () in
      set_advi_cache_dir d;
      d;;

(* Writing page current number to the file advi_page_number_file. *)
let write_page_number =
 Options.flag false "-page-number"
  "Ask advi to write the current page number in a file (default is no)";;

let advi_page_number_file = ref None;;

let set_page_number_file s = advi_page_number_file := Some s;;
let get_page_number_file () =
  match !advi_page_number_file with
  | Some fname -> fname
  | None ->
      let fname =
        Filename.concat (get_advi_cache_dir ()) "advi_page_number" in
      set_page_number_file fname;
      fname;;

Options.add "-page-number-file"
 (Arg.String set_page_number_file)
 "STRING\tSet the name of the file where \
  advi could write the current page number\n\
  \t(default is file \"advi_page_number\" in the cache directory\n\
  \t(default \"~/.advi\"))";;

let save_page_number n =
 if !write_page_number then
 try
   let oc = open_out (get_page_number_file ()) in
   output_string oc (string_of_int n);
   output_char oc '\n';
   close_out oc
 with _ ->
   Misc.warning 
     (Printf.sprintf
        "Cannot write file %s to record page number."
        (get_page_number_file ()));;

(* Initialization of advi_page_number_file. *)
let init_advi_page_number_file () =
 if !write_page_number then prepare_file (get_page_number_file ());;

Rc.at_init init_advi_page_number_file;;
