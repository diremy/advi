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

exception Error of string

let debug = Options.debug ~label: "image" "-debug-image"
    "Debug image file drawing";;

let verbose_image_access = Options.flag false
    "-verbose-image-access"
    "\tChange the cursor while image loadings";;

let image_aa = 
  Options.flag true
    "-disable-image-anti-aliasing"
    "\tDisable eps inclusion anti-aliasing"
;;

open OImage
open Color

type spec = {
    filename : string;
    width : int;
    height : int;
    bbox : (int * int * int * int) option;
    antialiase : bool;
    whitetransp : bool
  } 

let cache_prefix = "cache";;
let cache_key = "advicache";;

let memory_cache = Hashtbl.create 107;;

let cache_path spec = 
  let path = 
    let path = Userfile.fullpath (Unix.getcwd ()) spec.filename in
    let pbuf = Buffer.create (String.length path) in
    for i = 0 to String.length path - 1 do
      match path.[i] with
      | '-' -> Buffer.add_string pbuf "--"
      | '/' -> Buffer.add_char pbuf '-'
      | c -> Buffer.add_char pbuf c
    done;
    Buffer.contents pbuf
  in
  let size = Printf.sprintf "%dx%d" spec.width spec.height in
  let bbox = 
    let geom_string x =
      if x >= 0 then "+" ^ string_of_int x else string_of_int x
    in
    match spec.bbox with
    | Some (llx, lly, urx, ury) ->
        Printf.sprintf "-%s%s%s%s"
          (geom_string llx)
          (geom_string lly)
          (geom_string urx)
          (geom_string ury)
    | None -> ""
  in
  let antialiase = if spec.antialiase then "-aa" else "" in
  let whitetransp = if spec.whitetransp then "-tr" else "" in
  Filename.concat (Userfile.get_advi_cache_dir ())
    (Printf.sprintf "%s%s-%s%s%s%s"
       cache_prefix path size bbox antialiase whitetransp)
;;

let cache_load file =
  debug ("cache_load " ^ file);
  let load ic =
    let s = String.create (String.length cache_key) in
    ignore (input ic s 0 (String.length cache_key));
    if s <> cache_key then
      raise (Failure (file ^ " has no proper header"));
    let width = input_value ic in
    let height = input_value ic in
    let data = input_value ic in
    width, height, data
  in
  let opener, closer =
    if Config.gzip_path = "" then
      open_in_bin, close_in
    else
      (fun file ->
        let command = Printf.sprintf "%s -c -d %s" Config.gzip_path 
	    (Filename.quote file) in
        Unix.open_process_in command),
      (fun ic -> ignore (Unix.close_process_in ic))
  in
  let ic = opener file in
  Misc.after (fun () ->
    let width, height, data = load ic in
    new rgba32_with width height [] data)
    (fun () -> try closer ic with _ -> ())
;;

let cache_save file img =
  debug ("cache_save " ^ file);
  let save oc =
    output oc cache_key 0 (String.length cache_key);
    output_value oc img#width;
    output_value oc img#height;
    debug (Printf.sprintf "(%dx%d) %d bytes written"
             img#width img#height
             (String.length img#dump));
    output_value oc img#dump
  in
  let opener, closer =
    if Config.gzip_path = "" then
      open_out_bin, close_out
    else
      (fun file ->
        let command = Printf.sprintf "%s > %s" Config.gzip_path 
	    (Filename.quote file) in
        Unix.open_process_out command),
      (fun oc -> Pervasives.flush oc; ignore (Unix.close_process_out oc))
  in
  let oc = opener file in
  Misc.after (fun () -> save oc) (fun () -> try closer oc with _ -> ())
;;

let clean_cache () =
  (* erase the files match with "cache-*" *)
  let prefix = cache_prefix in
  let suffix = "" in
  let dh = Unix.opendir (Userfile.get_advi_cache_dir ()) in
  try while true do
    let file = Unix.readdir dh in
    if String.length file > 10 &&
       String.sub file 0 (String.length prefix) = prefix &&
       String.sub file (String.length file - String.length suffix)
                       (String.length suffix) = suffix
    then Unix.unlink (Filename.concat (Userfile.get_advi_cache_dir ()) file)
  done with End_of_file -> Unix.closedir dh
;;

let load_image spec =
  debug ("image load " ^ spec.filename);
  let image_aa_level = if !image_aa then 2.0 else 1.0 in
  try
    match fst (Image.file_format spec.filename) with
    | Image.Ps ->
        let (llx, lly, urx, ury) =
          match spec.bbox with
          | Some bbox -> bbox
          | None -> (0, 0, spec.width, spec.height)
        in
        (* for anti-aliasing *)
        let resx = float spec.width /. (float (urx - llx) /. 72.0) *. image_aa_level
        and resy = float spec.height /. (float (ury - lly) /. 72.0) *. image_aa_level
        in

        (* resolution fix *)
        (* gs dies if resolutions are too small !! (around 1.6 - 1.65dpi) *)
        let gs_min_res = 2.0 in
        let resx = max resx gs_min_res
        and resy = max resy gs_min_res
        in
        OImage.make (Ps.load_ps spec.filename (Some (llx, lly, urx, ury))
		       [Image.Load_Resolution (resx, resy)])
    | _ -> OImage.load spec.filename []
  with
  | Failure s ->
      Misc.warning ("Failed to load " ^ spec.filename ^ ": " ^ s);
      raise Exit
  | Image.Wrong_file_type ->
      Misc.warning ("Unsupported graphic format in: " ^ spec.filename);
      raise Exit
  | _ ->
      Misc.warning ("Failed to load " ^ spec.filename);
      raise Exit
;;

let transform image spec =
  let image =
    match OImage.tag image with
    | Index8 i -> Rgba32 i#to_rgba32
    | Index16 i -> Rgba32 i#to_rgba32
    | Rgb24 i -> Rgb24 i
    | Rgba32 i -> Rgba32 i
    | _ -> raise (Failure "color model is not supported")
  in
  let white = {r = 255; g = 255; b = 255} in
  let diff = 15 * 15 in
  let image =
    match image with
    | Rgb24 i ->
        let width = i#width
        and height = i#height in
        let rgba = new rgba32 width height in
        for y = 0 to height - 1 do
          for x = 0 to width - 1 do
            let rgb = i#unsafe_get x y in
            let a =
              if spec.whitetransp then
		let d = Rgb.square_distance rgb white in
		if d < diff then 
		  255 * d / diff
		else 255
	      else 255
	    in
            rgba#unsafe_set x y { color = rgb; alpha = a }
          done
        done;
        rgba
    | Rgba32 i -> i
    | _ -> assert false
  in
  let image = image#resize None spec.width spec.height in
  image
(*
  match spec.bbox with
  | None -> image
  | Some(sx, sy, dx, dy, w, h) ->
      let i = new rgba32_filled ow oh {color = white_rgb; alpha = 0} in
      image#blit sx sy i dx dy w h;
      i 
*)
;;

let load spec =
  let file =
    Search.true_file_name [ "-format=" ^ Config.texpicts_kind ] spec.filename 
  in
  let spec = {spec with filename = file} in
  let cache_name = cache_path spec in
  let image =
    try
      try
        Hashtbl.find memory_cache cache_name
      with
      | Not_found ->
          if (Unix.stat file).Unix.st_mtime >=
             (Unix.stat cache_name).Unix.st_mtime
          then raise Exit;
          (* Use the cache file *)
          cache_load cache_name
    with _ ->
      begin try Unix.unlink cache_name with _ -> () end;
      let image = transform (load_image spec) spec in
      Userfile.prepare_file cache_name;
      (* we have no trivial image format for rgba32! *)
      cache_save cache_name image;
      image
  in
  if image#width <> spec.width || image#height <> spec.height then
    raise (Error "load: size mismatch");
  Hashtbl.replace memory_cache cache_name image;
  cache_name, image
;;

(*
let draw (dbuf : GrGL.t) spec ~x:x0 ~y:y0 =
  try
    let y0 = y0 - spec.height in
    let clip = dbuf#clip ~x:x0 ~y:y0
	~width:spec.width ~height:spec.height in
    let cache_name, image = load spec in
    let ximg = dbuf#get_ximage_clip clip in
    for y = 0 to clip.GrDbuffer.height  - 1 do
      for x = 0 to clip.GrDbuffer.width - 1 do
	let srcc = image#unsafe_get 
	    (x + clip.GrDbuffer.xsrc) (y + clip.GrDbuffer.ysrc) in
	let dstc = GrMisc.Colour.parse (ximg#unsafe_get x y) in
	let blendf ~dst ~src = src.color in
	let finc = blendf ~dst:dstc ~src:srcc in
	ximg#unsafe_set x y (GrMisc.Colour.create finc)
      done
    done;
    dbuf#put_ximage_clip {clip with GrDbuffer.xsrc=0; GrDbuffer.ysrc=0} ximg;
    ximg#destroy
  with
  | GrDbuffer.Out -> ()
;;

let draw dbuf spec ~x ~y =
  try
    draw dbuf spec ~x ~y
  with
  | Exit -> ()
  | Unix.Unix_error (e, s1, s2) ->
      Misc.warning (Printf.sprintf "%s %s: %s" s1 s2 (Unix.error_message e))
  | Failure s -> Misc.warning s
  | e -> Misc.warning (Printexc.to_string e)
;;
*)
