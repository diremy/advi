(* RDC: this file is an adaptation of draw_ps, having possibly a different logic   *)
(* we might want to merge it back into a single source once the logic is finalised *)

open Image
open Color
open GraphicsY11

let cache_prefix = "cache-";;

let verbose_image_access = Misc.option_flag false
    "--verbose_image_access"
    "\tChange the cursor while image loadings";;

let images_camlimage = Hashtbl.create 107;;
let images_graphics = Hashtbl.create 107;;

type ratiopts =
   | ScaleX        (* scale x coords, but keep original Ratio *)
   | ScaleY        (* scale y coords, but keep original Ratio *)
   | Original      (* respect original size                   *)
   | FreeScale     (* scale to fit requested area             *)
;;

let white_rgb = {r = 255; g = 255; b = 255};;

let f file whitetransp alpha blend ratiopts (w, h) x0 y0 =
  let file = Search.true_file_name [] file in
  let cache_name =   (* The computed cache name *)
    let file' = Userfile.fullpath (Unix.getcwd ()) file in
    for i = 0 to String.length file' - 1 do
      if file'.[i] = '/' then file'.[i] <- '-'
    done;
    Filename.concat Userfile.cache_dir
      (Printf.sprintf "%s%s-%dx%d%s"
	 cache_prefix
	 file'
	 w h
	 (if whitetransp then "tr" else ""))
  in

  (* Load an image from the cache *)
  let load () =
    (* we have no trivial image format for rgba32! *)
    if whitetransp then begin
      let ic = open_in_bin cache_name in
      let width = input_value ic in
      let height = input_value ic in
      let data = input_value ic in
      close_in ic;
      Rgba32 (Rgba32.create_with width height data)
    end else begin
      let ic = open_in_bin cache_name in
      let width = input_value ic in
      let height = input_value ic in
      let data = input_value ic in
      close_in ic;
      Rgb24 (Rgb24.create_with width height data)
    end
  in

  (* Save an image into the cache *)
  let save = function
    | Rgba32 image ->
	begin try
	  let oc = open_out_bin cache_name in
	  output_value oc image.Rgba32.width;
	  output_value oc image.Rgba32.height;
	  output_value oc (Bitmap.dump image.Rgba32.data);
	  close_out oc
	with _ -> () end
    | Rgb24 image ->
	begin try
	  let oc = open_out_bin cache_name in
	  output_value oc image.Rgb24.width;
	  output_value oc image.Rgb24.height;
	  output_value oc (Bitmap.dump image.Rgb24.data);
	  close_out oc
	with _ -> () end
    | _ -> assert false
  in

  let image = (* here one builds the image structure *)
    try
      if (Unix.stat file).Unix.st_mtime >=
	   (Unix.stat cache_name).Unix.st_mtime
      then raise Exit;
      try
	Hashtbl.find images_camlimage cache_name
      with Not_found ->
      	(* Use the cache file *)
	if !verbose_image_access then GraphicsY11.set_cursor Cursor_exchange;
	let im = load () in
	if !verbose_image_access then GraphicsY11.set_cursor Cursor_left_ptr;
	im
    with
    | _ ->
	if !verbose_image_access then GraphicsY11.set_cursor Cursor_exchange;
      	begin try Unix.unlink cache_name with _ -> () end;
      	let image = (* image can be Rgb24, Rgba32, Index8 ... *)
	  (* we skip anti-aliasing for now ... *)
          try
            Image.load file []
          with 
            Image.Wrong_file_type -> 
              Misc.warning ("Unsupported graphic format in: " ^ file);
              raise Exit
          | Failure s ->
              Misc.warning ("Failed to load " ^ file ^ ": " ^ s);
              raise Exit
      	in
        (* convert the image to Rgba32 or Rgb24 *)
	let image = match image with
        | Rgba32 i -> image
        | Rgb24 i ->
            if whitetransp then begin
  	      let width = i.Rgb24.width
  	      and height = i.Rgb24.height
  	      in
  	      let rgba = Rgba32.create width height in
  	      for y = 0 to height - 1 do
                for x = 0 to width - 1 do
                  let rgb = Rgb24.unsafe_get i x y in
                  let a = if rgb = white_rgb then 0 else 255 in
                  Rgba32.unsafe_set rgba x y { color = rgb; alpha = a }
                done
  	      done;
              Rgba32 rgba
            end else image
        | Index8 i ->
            if whitetransp then begin
  	      let width = i.Index8.width
  	      and height = i.Index8.height
  	      in
  	      let rgba = Rgba32.create width height in
  	      for y = 0 to height - 1 do
                for x = 0 to width - 1 do
                  let index = Index8.unsafe_get i x y in
                  let rgb = i.Index8.colormap.Color.map.(index) in
                  let a = 
                    if index = i.Index8.transparent || rgb = white_rgb then 0
                    else 255
                  in
                  Rgba32.unsafe_set rgba x y { color = rgb; alpha = a }
                done
  	      done;
              Rgba32 rgba
            end else Rgba32 (Index8.to_rgba32 i)
	| _ -> assert false
	in
	let image = 
	  match image with
	  | Rgb24 i -> Rgb24 (Rgb24.resize None i w h)
	  | Rgba32 i -> Rgba32 (Rgba32.resize None i w h)
	  | _ -> assert false
	in
	Userfile.prepare_file cache_name; 
	(* we have no trivial image format for rgba32! *)
	save image;
	if !verbose_image_access then GraphicsY11.set_cursor Cursor_left_ptr;
	image
  in
  begin try ignore (Hashtbl.find images_camlimage cache_name) with Not_found ->
    Hashtbl.add images_camlimage cache_name image
  end;

  match image with
  | Rgb24 _ when alpha = 1.0 && blend = None ->
      (* optimized *)
      let image_graphics =
	try
	  Hashtbl.find images_graphics cache_name
	with
	| Not_found ->
	    let im = Graphic_image.of_image image in
	    Hashtbl.add images_graphics cache_name im;
	    im
      in
      Graphics.draw_image image_graphics x0 y0
  | Rgb24 _ | Rgba32 _ ->
(* RDC alpha blending INUTILE pour backgrounds? Let's say so for now...
      let alpha = truncate (alpha *. 255.0) in
      let get_src_alpha =
	match image with
	| Rgb24 image -> fun x y -> Rgb24.unsafe_get image x y, alpha
	| Rgba32 image ->
	    fun x y ->
	      let {color= src; alpha= a} = Rgba32.unsafe_get image x y in
	      src, a * alpha / 255
	| _ -> assert false
      in

      let blend =
	match blend with
	| Some b -> b
	| None -> fun dst src -> src
      in

      let org = Graphic_image.get_image x0 y0 w h in
      let coloropt a =
    	match a with
    	| 0 -> fun dst src -> dst
    	| 255 ->
	    fun dst src ->
	      {r= blend dst.r src.r;
	       g= blend dst.g src.g;
	       b= blend dst.b src.b }
    	| _ ->
	    let a' = 255 - a in
	    fun dst src ->
	      {r= (blend dst.r src.r * a + dst.r * a') / 255;
	       g= (blend dst.g src.g * a + dst.g * a') / 255;
	       b= (blend dst.b src.b * a + dst.b * a') / 255 }
      in
      for y = 0 to h - 1 do
    	for x = 0 to w - 1 do
	  let dst = Rgb24.unsafe_get org x y in
	  let src, a = get_src_alpha x y in
	  Rgb24.unsafe_set org x y (coloropt a dst src)
    	done
      done;
      Graphic_image.draw_image (Rgb24 org) x0 y0
*)
      ()
  | _ -> assert false;;

(* Wrapped version of [f], which catches the Exit 
   (i.e. image loading failures) *)
let f file whitetransp alpha blend ratiopts (w, h) x0 y0 =
  try
    f file whitetransp alpha blend ratiopts (w, h) x0 y0
  with
  | Exit -> ()
;; 

let clean_cache () =
  (* erase the files whose name matches with "cache-*" *)
  let prefix = cache_prefix in
  let suffix = "" in
  let suff_len = String.length suffix in
  let pref_len = String.length prefix in
  let dh = Unix.opendir Userfile.cache_dir in
  try while true do
    let file = Unix.readdir dh in
    let file_len = String.length file in
    if file_len > 10 &&
       String.sub file 0 pref_len = prefix &&
       String.sub file (file_len - suff_len) suff_len = suffix
    then Unix.unlink file
  done with End_of_file ->
    Unix.closedir dh;;


