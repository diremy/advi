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
(*  Jun Furuse, Didier R�my and Pierre Weis.                           *)
(*  Contributions by Roberto Di Cosmo, Didier Le Botlan,               *)
(*  Xavier Leroy, and Alan Schmitt.                                    *)
(*                                                                     *)
(*  Based on Mldvi by Alexandre Miquel.                                *)
(***********************************************************************)

let debugs = Misc.debug_endline;;

type ratiopts =
   | ScaleOriginal    (* leave the image at its original native size  *)
   | ScaleAuto        (* scale to fit requested area                  *)
   | ScaleCenter      (* scale as needed to cover the image,
                         keep original Ratio and center *)
   | ScaleTop         (* scale x coords to align to top of the screen,
                         keep original Ratio  *)
   | ScaleBottom      (* scale x coords to align to bottom of the screen,
                         keep original Ratio  *)
   | ScaleLeft        (* scale y coords to align to left of the screen,
                         keep original Ratio  *)
   | ScaleRight       (* scale y coords to align to right of the screen,
                         keep original Ratio  *)
   | ScaleTopLeft
   | ScaleBottomLeft
   | ScaleTopRight
   | ScaleBottomRight
;;

(* Blending *)
type blend =
   | Normal | Multiply | Screen | Overlay (* | SoftLight | HardLight *)
   | ColorDodge | ColorBurn | Darken | Lighten | Difference
   | Exclusion (* | Luminosity | Color | Saturation | Hue *);;

type alpha = float;;

(* look at gxblend.c of ghostscript *)
let blend_func = function
  | Normal -> raise Exit (* this case is optimized *)
  | Multiply ->
      fun dst src ->
        let t = dst * src + 0x80 in
        let t = t + t lsr 8 in
        t lsr 8
  | Screen ->
      fun dst src ->
        let t = (0xff - dst) * (0xff - src) + 0x80 in
        let t = t + t lsr 8 in
        0xff - t lsr 8
  | Overlay ->
      fun dst src ->
        let t =
          if dst < 0x80 then 2 * dst * src
          else 0xf301 - 2 * (0xff - dst) * (0xff - src) in
        let t = t + 0x80 in
        let t = t + t lsr 8 in
        t lsr 8
(*
   | SoftLight ->
   if s < 0x80 then begin
   let t = (0xff - (src lsl 1)) * art_blend_sq_diff_8[dst] in
   let t = t + 0x8000 in
   dst - t lsr 16
   end else begin
   let t = ((src lsl 1) - 0xff) * art_blend_soft_light_8[dst] in
   let t = t + 0x80 in
   let t = t + t lsr 8 in
   dst + t lsr 8
   end
*)
  | ColorDodge ->
      fun dst src ->
        if dst = 0 then 0 else if dst >= src then 0xff
        else (0x1fe * dst + src) / (src lsl 1)
  | ColorBurn ->
      fun dst src ->
        let dst = 0xff - dst in
        if dst = 0 then 0xff else if dst >= src then 0
        else 0xff - (0x1fe * dst + src) / (src lsl 1)
  | Darken ->
      fun dst src -> if dst < src then dst else src
  | Lighten ->
      fun dst src -> if dst > src then dst else src
  | Difference ->
      fun dst src ->
        let t = dst - src in
        if t < 0 then -t else t
  | Exclusion ->
      fun dst src ->
        let t = (0xff - dst) * src + dst * (0xff - src) in
        let t = t + 0x80 in
        let t = t + t lsr 8 in
        t lsr 8;;

type image_size = int * int;;

type ps_bbox = int * int * int * int;; 

open Image
open Color
open GraphicsY11

let cache_prefix = "cache";;
let cache_key = "advicache";;

let verbose_image_access = Options.flag false
    "--verbose-image-access"
    "\tChange the cursor while image loadings";;

let image_aa = 
  Options.flag true
    "-disable-image-anti-aliasing"
    "\tDisable eps inclusion anti-aliasing"
;;

let images_camlimages = Hashtbl.create 107;;
let images_graphics = Hashtbl.create 107;;

let after f g = try let x = f () in  g (); x with e -> g (); raise e;;

let cache_path file whitetransp psbbox ratiopt (w, h) =
  let file' = Userfile.fullpath (Unix.getcwd ()) file in
  let file' = if file == file' then String.copy file' else file' in
  for i = 0 to String.length file' - 1 do
    if file'.[i] = '/' then file'.[i] <- '-'
  done;
  let geom_string x =
    if x >= 0 then "+" ^ string_of_int x else string_of_int x
  in
  let bbox_str =
    match psbbox with
    | Some (llx, lly, urx, ury) ->
        Printf.sprintf "%s%s%s%s"
          (geom_string llx)
          (geom_string lly)
          (geom_string urx)
          (geom_string ury)
    | None -> ""
  in
  let ratiopt_str =
    match ratiopt with
    | ScaleAuto        -> "-a"
    | ScaleCenter      -> "-c"
    | ScaleTop         -> "-t"
    | ScaleLeft        -> "-l"
    | ScaleOriginal    -> "-o"
    | ScaleBottom      -> "-b"
    | ScaleRight       -> "-r"
    | ScaleTopLeft     -> "-tl"
    | ScaleBottomLeft  -> "-bl"
    | ScaleTopRight    -> "-tr"
    | ScaleBottomRight -> "-br"
  in
  Filename.concat (Userfile.get_advi_cache_dir ())
    (Printf.sprintf "%s%s-%dx%d%s%s%s%s"
       cache_prefix
       file'
       w h
       bbox_str
       ratiopt_str
       (if whitetransp then "-tr" else "")
       (if !image_aa then "-aa" else ""))
;;

let cache_load file =
  debugs ("cache_load " ^ file);
  let load ic =
    let s = String.create (String.length cache_key) in
    ignore (input ic s 0 (String.length cache_key));
    if s <> cache_key then
      raise (Failure (file ^ " has no proper header"));
    let rgba = input_value ic in (* bool *)
    let width = input_value ic in
    let height = input_value ic in
    let data = input_value ic in
    rgba, width, height, data
  in
  let opener, closer =
    if Config.gzip_path = "" then
      open_in_bin, close_in
    else
      (fun file ->
        let command = Printf.sprintf "%s -c -d %s" Config.gzip_path file in
        Unix.open_process_in command),
      (fun ic -> ignore (Unix.close_process_in ic))
  in
  let ic = opener file in
  after (fun () ->
    let rgba, width, height, data = load ic in
    if rgba then Rgba32 (Rgba32.create_with width height [] data)
    else Rgb24 (Rgb24.create_with width height [] data))
    (fun () -> try closer ic with _ -> ())
;;

let cache_save file img =
  debugs ("cache_save " ^ file);
  let save oc =
    output oc cache_key 0 (String.length cache_key);
    match img with
    | Rgba32 image ->
        output_value oc true;
        output_value oc image.Rgba32.width;
        output_value oc image.Rgba32.height;
        debugs (Printf.sprintf "(%dx%d) %d bytes written"
                  image.Rgba32.width image.Rgba32.height
                  (String.length (Rgba32.dump image)));
        output_value oc (Rgba32.dump image);
    | Rgb24 image ->
        output_value oc false;
        output_value oc image.Rgb24.width;
        output_value oc image.Rgb24.height;
        output_value oc (Rgb24.dump image);
    | _ -> assert false
  in
  let opener, closer =
    if Config.gzip_path = "" then
      open_out_bin, close_out
    else
      (fun file ->
        let command = Printf.sprintf "%s > %s" Config.gzip_path file in
        Unix.open_process_out command),
      (fun oc -> Pervasives.flush oc; ignore (Unix.close_process_out oc))
  in
  let oc = opener file in
  after (fun () -> save oc) (fun () -> try closer oc with _ -> ())
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

let image_load psbbox (w, h) file =
  debugs ("image load " ^ file);
  let image_aa_level = if !image_aa then 2.0 else 1.0 in
  try
    match fst (Image.file_format file) with
    | Image.Ps ->
        let (llx, lly, urx, ury) =
        match psbbox with
        | Some bbox -> bbox
        | None -> (0, 0, w, h)
        in
        (* we need anti-aliasing *)
        let resx = float w /. (float (urx - llx) /. 72.0) *. image_aa_level
        and resy = float h /. (float (ury - lly) /. 72.0) *. image_aa_level
        in

        (* resolution fix *)
        (* gs dies if resolutions are too small (around 1.6 - 1.65dpi) *)
        let gs_min_res = 2.0 in
        let resx = 
          if resx < gs_min_res then gs_min_res
          else resx
        and  resy = 
          if resy < gs_min_res then gs_min_res
          else resy
        in
        
        Ps.load_ps file (Some (llx, lly, urx, ury))
          [Load_Resolution (resx, resy)]
    | _ -> Image.load file []
  with
  | Failure s ->
      Misc.warning ("Failed to load " ^ file ^ ": " ^ s);
      raise Exit
  | Image.Wrong_file_type ->
      Misc.warning ("Unsupported graphic format in: " ^ file);
      raise Exit
  | _ ->
      Misc.warning ("Failed to load " ^ file);
      raise Exit
;;

let resize_and_make_transparent image whitetransp ratiopt (ow, oh) =
  let iw, ih = Image.size image in
  let w, h =
    (* compute the size of the scaled image according to the options *)
    match ratiopt with
    | ScaleOriginal -> iw, ih
    | ScaleAuto     -> ow, oh
    | ScaleTop | ScaleBottom ->
        ow, Pervasives.truncate (float ih *. float ow /. float iw)
    | ScaleLeft | ScaleRight ->
        Pervasives.truncate (float iw *. float oh /. float ih), oh
    | ScaleCenter | ScaleTopLeft | ScaleBottomLeft
    | ScaleTopRight | ScaleBottomRight ->
       (* In all these cases, we need to resize the image just enough
          to cover the requested area of size ow, oh
          it is when blitting the image that we will clip it differently
          according to these directives *)
       let ratiow = float ow /. float iw
       and ratioh = float oh /. float ih in
       if ratiow > ratioh then ow, Pervasives.truncate (float ih *. ratiow)
       else Pervasives.truncate (float iw *. ratioh), oh in
  let blitinfo =
  (* compute the blitting information for the image        *)
  (* None means do not blit the image                      *)
  (* Some(sx, sy, dx, dy, w, h) means copy area of widht w and  *)
  (* height h located in source at sx, dy into destination  *)
  (* at dx, dy                                              *)
    match ratiopt with
    | ScaleOriginal | ScaleAuto -> None
    | ScaleTop ->
        if oh = h && ow = w then None
        else Some(0, 0, 0, 0, min w ow, min h oh)
    | ScaleBottom ->
        if oh = h && ow = w then None
        else Some(0, max 0 (h - oh), 0, max 0 (oh - h), min w ow, min h oh)
    | ScaleLeft | ScaleRight when ow = w -> None
    | ScaleLeft -> Some(0, 0, 0, 0, min w ow, h)
    | ScaleRight -> Some(max 0 (w - ow), 0, max 0 (ow - w), 0, min w ow, h)
    | ScaleCenter | ScaleTopLeft | ScaleBottomLeft
    | ScaleTopRight | ScaleBottomRight when oh = h && ow = w -> None
    | ScaleCenter ->
        Some((max 0 (w - ow)) / 2, (max 0 (h - oh)) / 2, 0, 0,
             min w ow, min h oh)
    | ScaleTopLeft ->
        Some(0, 0, 0, 0, min w ow, min h oh)
    | ScaleBottomLeft ->
        Some(0, max 0 (h - oh), 0, 0, min w ow, min h oh)
    | ScaleTopRight ->
        Some(max 0 (w - ow), 0, 0, 0, min w ow, min h oh)
    | ScaleBottomRight ->
        Some(max 0 (w - ow), max 0 (h - oh), 0, 0, min w ow, min h oh)
  in
  let image' =
    match image with
    | Index8 i -> Rgba32 (Index8.to_rgba32 i)
    | Index16 i -> Rgba32 (Index16.to_rgba32 i)
    | Rgb24 i -> image
    | Rgba32 i -> image
    | _ -> raise (Failure "color model is not supported")
  in
  let white_rgb = {r = 255; g = 255; b = 255} in
  let diff = 15 * 15 in
  let image'' =
    if not whitetransp then image' else
    match image' with
    | Rgb24 i ->
        let width = i.Rgb24.width
        and height = i.Rgb24.height in
        let rgba = Rgba32.create width height in
        for y = 0 to height - 1 do
          for x = 0 to width - 1 do
            let rgb = Rgb24.unsafe_get i x y in
            let a =
              if whitetransp && Rgb.square_distance rgb white_rgb < diff 
	      then 0 else 255 in
            Rgba32.unsafe_set rgba x y { color = rgb; alpha = a }
          done
        done;
        Rgba32 rgba
    | Rgba32 i ->
        let width = i.Rgba32.width
        and height = i.Rgba32.height in
        for y = 0 to height - 1 do
          for x = 0 to width - 1 do
            let rgba = Rgba32.unsafe_get i x y in
            if rgba.color = white_rgb then
              Rgba32.unsafe_set i x y { color = rgba.color; alpha = 0 }
          done
        done;
        image
    | _ -> assert false
  in
  match image'' with
  | Rgb24 i ->
      let i = Rgb24.resize None i w h in
      let i =
        match blitinfo with
        | None -> i
        | Some(sx, sy, dx, dy, w, h) ->
            let i' = Rgb24.make ow oh white_rgb in
            Rgb24.blit i sx sy i' dx dy w h;
            i' in
      Rgb24 i
  | Rgba32 i ->
      let i = Rgba32.resize None i w h in
      let i =
        match blitinfo with
        | None -> i
        | Some(sx, sy, dx, dy, w, h) ->
            let i' = Rgba32.make ow oh {color = white_rgb; alpha = 1} in
            Rgba32.blit i sx sy i' dx dy w h;
            i' in
      Rgba32 i
  | _ -> assert false
;;

let load_and_resize file whitetransp psbbox ratiopt (w, h) =
  let file =
    Search.true_file_name [ "-format=" ^ Config.texpicts_kind ] file in
  let cache_name = cache_path file whitetransp psbbox ratiopt (w, h) in
  let image =
    try
      try
        Hashtbl.find images_camlimages cache_name
      with
      | Not_found ->
          if (Unix.stat file).Unix.st_mtime >=
             (Unix.stat cache_name).Unix.st_mtime
          then raise Exit;
          (* Use the cache file *)
          if !verbose_image_access
            then GraphicsY11.set_cursor Cursor_exchange;
          let im = cache_load cache_name in
          if !verbose_image_access
            then GraphicsY11.set_cursor Cursor_left_ptr;
          im
    with _ ->
      if !verbose_image_access then GraphicsY11.set_cursor Cursor_exchange;
        begin try Unix.unlink cache_name with _ -> () end;
        let image = image_load psbbox (w, h) file in
        let image =
          resize_and_make_transparent image whitetransp ratiopt (w, h) in
        Userfile.prepare_file cache_name;
        (* we have no trivial image format for rgba32! *)
        cache_save cache_name image;
        if !verbose_image_access then GraphicsY11.set_cursor Cursor_left_ptr;
        image
  in
  Hashtbl.replace images_camlimages cache_name image;
  cache_name, image
;;

let draw_image image cache_name alpha blend (w, h) (x0, y0) =
  let blend =
    try Some (blend_func blend) with _ -> None in
  (* load_and_resize may not return exactly the same size
     we specified as (w, h) *)
  let iw, ih = Image.size image in
  match image with
  | Rgb24 _ when alpha = 1.0 && blend = None ->
      (* optimized *)
      let image_graphics =
        try Hashtbl.find images_graphics cache_name with
        | Not_found ->
            let im = Graphic_image.of_image image in
            Hashtbl.add images_graphics cache_name im;
            im
      in
      Graphics.draw_image image_graphics x0 y0
  | Rgb24 _ | Rgba32 _ ->
      let alpha = truncate (alpha *. 255.0) in
      let get_src_alpha =
        match image with
        | Rgb24 image -> fun x y -> Rgb24.unsafe_get image x y, alpha
        | Rgba32 image ->
            fun x y ->
              let {color = src; alpha = a} = Rgba32.unsafe_get image x y in
              src, a * alpha / 255
        | _ -> assert false in

      let blend =
        match blend with
        | Some b -> b
        | None -> fun dst src -> src in

      let org = Graphic_image.get_image x0 y0 w h in
      let coloropt a =
        match a with
        | 0 -> fun dst src -> dst
        | 255 ->
            fun dst src ->
              {r = blend dst.r src.r;
               g = blend dst.g src.g;
               b = blend dst.b src.b }
        | _ ->
            let a' = 255 - a in
            fun dst src ->
              {r = (blend dst.r src.r * a + dst.r * a') / 255;
               g = (blend dst.g src.g * a + dst.g * a') / 255;
               b = (blend dst.b src.b * a + dst.b * a') / 255 }
      in
      for y = 0 to (min ih h) - 1 do
        for x = 0 to (min iw w) - 1 do
          let dst = Rgb24.unsafe_get org x y in
          let src, a = get_src_alpha x y in
          Rgb24.unsafe_set org x y (coloropt a dst src)
        done
      done;
      Graphic_image.draw_image (Rgb24 org) x0 y0
  | _ -> assert false
;;


let f file whitetransp alpha blend psbbox ratiopt (w, h) (x0, y0) =
  try
    let cache_name, image =
      load_and_resize file whitetransp psbbox ratiopt (w, h) in
    (* load_and_resize may not return exactly the same size
       we specified as (w, h) *)
    debugs (Printf.sprintf "Draw %s (alpha=%f blend=%s)" file
              alpha "Don't know");
    draw_image image cache_name alpha blend (w, h) (x0, y0);
    debugs "Success Draw!";
  with
  | Exit -> ()
  | Unix.Unix_error (e, s1, s2) ->
      Misc.warning (Printf.sprintf "%s %s: %s" s1 s2 (Unix.error_message e))
  | Failure s -> Misc.warning s
  | e -> Misc.warning (Printexc.to_string e)
;;
