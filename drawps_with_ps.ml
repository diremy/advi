open Image
open Color
open GraphicsY11

let verbose_image_access = Misc.option_flag false
    "--verbose_image_access"
    "\tChange the cursor while image loadings";;

let images_camlimage = Hashtbl.create 107
let images_graphics = Hashtbl.create 107

let cache_dir = ".advi"

let f file whitetransp alpha blend (llx,lly,urx,ury) (w,h) x0 y0 =
  let file = 
    try List.hd (Search.true_file_names [] [file]) with _ -> raise Not_found
  in
  let cache_name =
    let file' = String.copy file in
    for i = 0 to String.length file' - 1 do
      if file'.[i] = '/' then file'.[i] <- '-'
    done;
    let geom_string x = 
      if x >= 0 then "+" ^ string_of_int x else string_of_int x
    in
    Filename.concat cache_dir
      (Printf.sprintf "cache-%s-%dx%d%s%s%s%s%s"
	 file'
	 w h
	 (geom_string llx) 
	 (geom_string lly)
	 (geom_string urx)
	 (geom_string ury)
	 (if whitetransp then "tr" else ""))
  in

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
    | _ -> () 
  in

  let image = 
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
      	let image =
	  (* we need anti-aliasing *)
  	  let resx = float w /. (float (urx - llx) /. 72.0) *. 2.0
	  and resy = float h /. (float (ury - lly) /. 72.0) *. 2.0 
	  in
  	  Ps.load_ps file (Some (llx,lly,urx,ury)) 
	      [Load_Resolution (resx,resy)];
      	in
	let image = match image with
	| Rgb24 i ->
  	    let width = i.Rgb24.width 
  	    and height = i.Rgb24.height
  	    in
	    if whitetransp then begin
  	      let rgba = Rgba32.create width height in
  	      for y = 0 to height - 1 do
  	      for x = 0 to width - 1 do 
  		let rgb = Rgb24.unsafe_get i x y in
  		let a = 
  		  if whitetransp && rgb = {r=255;g=255;b=255} then 0
  		  else 255
  		in
  		Rgba32.unsafe_set rgba x y { color=rgb; alpha=a }
  	      done
  	      done;
  	      Rgba32 (Rgba32.resize None rgba (width/2) (height/2))
	    end else begin
  	      Rgb24 (Rgb24.resize None i (width/2) (height/2))
	    end
	| _ -> assert false
	in
      	begin try Unix.mkdir cache_dir 0o775 with _ -> () end;
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
      Graphics.draw_image image_graphics x0 (y0 -h)
  | Rgb24 _ | Rgba32 _ ->
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

      let org = Graphic_image.get_image x0 (y0 - h) w h in
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
      Graphic_image.draw_image (Rgb24 org) x0 (y0 - h)
  | _ -> assert false

let clean_cache () =
  (* erase the files match with "cache-*" *)
  let prefix = Filename.concat cache_dir "cache-" in
  let suffix = "" in
  let dh = Unix.opendir "." in
  try while true do
    let file = Unix.readdir dh in
    if String.length file > 10 &&
       String.sub file 0 (String.length prefix) = prefix &&
       String.sub file (String.length file - String.length suffix)
	               (String.length suffix) = suffix then begin
			 Unix.unlink file
		       end
  done with End_of_file -> 
    Unix.closedir dh

