(* create a opengl texture of given font *)

let sort_chardefs chardefs =
  let compare cd1 cd2 = compare cd1.Pkfont.height cd2.Pkfont.height
  in
  List.sort compare chardefs
;;

type lineconf = 
    { start : Pkfont.char_def list;
      length : int;
      waste : int;
      lc_width : int;
      lc_height : int } 
;;

type layout = 
    { char_def : Pkfont.char_def;
      x : int;
      y : int }
;;

type layout_set =
    { layouts : layout list;
      fill : float;
      texture_width : int;
      texture_height : int } 

let total chardefs =
  let total = ref 0 in
  let maxw = ref 0 in
  List.iter (fun cd ->
    total := !total + cd.Pkfont.width * cd.Pkfont.height;
    maxw := max !maxw cd.Pkfont.width) chardefs;
  !total, !maxw
;;

let sep = 4;;

let binpack chardefs =
  let total, maxw = total chardefs in
  prerr_endline (Printf.sprintf "total done");

  let layout texture_width = 
    prerr_endline (Printf.sprintf "layout width=%d" texture_width);
    let sorted_chardefs = sort_chardefs chardefs in
    let prepare_line unplaced =
      let rec aux (best, new_unplaced) last = function
  	| [] -> last, []
  	| c::cs ->
  	    if last.lc_width + sep + c.Pkfont.width > texture_width
  	    then best, new_unplaced
  	    else 
  	      let new_waste = 
  		last.waste - texture_width * (c.Pkfont.height - last.lc_height)
  		  - c.Pkfont.width * c.Pkfont.height
  	      in
  	      let new_last = 
  		{ start= unplaced;
  		  length= last.length + 1;
  		  waste= new_waste;
  		  lc_width= last.lc_width + sep + c.Pkfont.width;
  		  lc_height = c.Pkfont.height }
  	      in
  	      let new_best, new_unplaced =
  		if best.waste < new_last.waste then best, new_unplaced 
  		else new_last, cs
  	      in
  	      aux (new_best, new_unplaced) new_last cs
      in
      let hd = List.hd unplaced in
      let tl = List.tl unplaced in
      let best = { start= unplaced; 
  		   length=1; 
  		   waste= (texture_width - hd.Pkfont.width) * hd.Pkfont.height;
  		   lc_width= hd.Pkfont.width;
  		   lc_height= hd.Pkfont.height } 
      in
      aux (best, tl) best tl
    in
  
    let layouts = ref [] in
    let texture_height = ref 1 in
    let cur_y = ref 0 in
  
    let rec loop unplaced =
      let lineconf, new_unplaced = prepare_line unplaced in
      let cur_x = ref 0 in
      let rec place = function
  	| (0, _) -> ()
  	| (n, c::cs) ->
  	    layouts := { char_def= c;
  			 x= !cur_x;
  			 y= !cur_y } :: !layouts;
  	    cur_x := !cur_x + sep + c.Pkfont.width;
  	    place (n-1, cs)
  	| _ -> assert false
      in
      place (lineconf.length, unplaced);
      cur_y := !cur_y + lineconf.lc_height + sep; 
      let rec udpate_texture_height th =
  	if th >= !cur_y - 2 (* no border is ok at the bottom *)
	then th else udpate_texture_height (th * 2) 
      in
      texture_height := udpate_texture_height !texture_height;
      if new_unplaced = [] then () else loop new_unplaced
    in
      
    loop sorted_chardefs;
  
    let fill = 
      float total /. (float (texture_width * !texture_height)) *. 100.0
    in
    prerr_endline (Printf.sprintf "%dx%d (%f%%)"
  		     texture_width !texture_height fill);
    { layouts = !layouts;
      texture_width= texture_width;
      texture_height= !texture_height;
      fill= fill }
  in
  
  let startfrom =
    let rec aux n =
      if n > maxw then n else aux (n * 2)
    in
    aux 128
  in

  prerr_endline (Printf.sprintf "start from done");

  let texture_width_candidates =
    let width = ref 0 in
    List.iter (fun c -> width := !width + c.Pkfont.width) chardefs;
    prerr_endline (Printf.sprintf "max width=%d" !width);
    let rec aux n =
      if n > !width then [n] else n::aux (n*2)
    in
    aux startfrom
  in
  
  prerr_endline (Printf.sprintf "texture width candidiates done");

  let layout_sets = List.map layout texture_width_candidates in
  List.hd (List.sort (fun ls1 ls2 -> 
    if ls1.fill = ls2.fill then
      compare (abs (ls1.texture_width - ls1.texture_height))
	(abs (ls2.texture_width - ls2.texture_height))
    else compare ls2.fill ls1.fill) layout_sets)
;;

let save_texture fontname dpi layout_set =
  let { layouts= layouts;
        texture_width= texture_width;
        texture_height= texture_height } = layout_set
  in
  let img = 
    new OImage.rgb24_filled 
      texture_width texture_height
      {Color.r=255; Color.g=255; Color.b=255}
  in
  List.iter (fun l -> 
    Pkfont.unpack l.char_def;
    let bitmap = 
      match l.char_def.Pkfont.bitmap with
      |	Pkfont.Unpacked s -> s
      |	_ -> assert false
    in
    let width = l.char_def.Pkfont.width in
    let height = l.char_def.Pkfont.height in
    let get_bit x y = 
      let pos = y * width + x in
      let byte = pos lsr 3 in
      let bit = 7 - pos land 7 in
      int_of_char bitmap.[byte] land (1 lsl bit) <> 0
    in
    let blockc = 
      {Color.r= Random.int 127 + 127;
       Color.g= Random.int 127 + 127;
       Color.b= Random.int 127 + 127}
    in
    for y = 0 to height - 1 do
      for x = 0 to width - 1 do
	img#unsafe_set (l.x + x) (l.y + y) 
	  (if get_bit x y then {Color.r=0; Color.g=0; Color.b=0} 
	  else blockc)
      done
    done) layouts;
  img#save (Printf.sprintf "texture-%s-%d.jpg" fontname dpi) None []
;;

type graymap = 
    { texture : ([`luminance_alpha], [`ubyte]) GlPix.t;
      x1 : float;
      y1 : float;
      x2 : float;
      y2 : float 
    } 

type char_def = 
    { code : int;
      dx : int;
      dy : int;
      width : int;
      height : int;
      hoffset : int;
      voffset : int;
      graymap : graymap;
    } 

let create_texture layout_set =
  let gimage = GlPix.create `ubyte 
      ~width: layout_set.texture_width
      ~height: layout_set.texture_height
      ~format: `luminance_alpha
  in
  let raw = GlPix.to_raw gimage in
  let pos = GlPix.raw_pos gimage in

  (* bit slow, but sound clean *)
  for y = 0 to layout_set.texture_height - 1 do
    for x = 0 to layout_set.texture_width - 1 do
      Raw.sets raw ~pos: (pos ~x ~y) [|0;0|]
    done
  done;

  gimage,
  List.map (fun l -> 
    Pkfont.unpack l.char_def;
    let bitmap = 
      match l.char_def.Pkfont.bitmap with
      |	Pkfont.Unpacked s -> s
      |	_ -> assert false
    in
    let width = l.char_def.Pkfont.width in
    let height = l.char_def.Pkfont.height in
    let get_bit x y = 
      let pos = y * width + x in
      let byte = pos lsr 3 in
      let bit = 7 - pos land 7 in
      int_of_char bitmap.[byte] land (1 lsl bit) <> 0
    in

    let pos ~x ~y = 
      pos ~x: (l.x + x)
	  ~y: (layout_set.texture_height - 1 - (l.y + y))
    in
      
    for y = 0 to height - 1 do
      for x = 0 to width - 1 do
	let alpha = if get_bit x y then 255 else 0 in
	Raw.sets raw ~pos: (pos ~x ~y) [|255;alpha|]
      done
    done;

(*
    for y = 0 to height - 1 do
      Raw.sets raw ~pos: (pos ~x: 0 ~y: y) [|255;255|];
      Raw.sets raw ~pos: (pos ~x: (width-1) ~y: y) [|255;255|]
    done;
    for x = 0 to width - 1 do
      Raw.sets raw ~pos: (pos ~x: x ~y: 0) [|255;255|];
      Raw.sets raw ~pos: (pos ~x: x ~y: (height-1)) [|255;255|]
    done;
*)

    { code = l.char_def.Pkfont.code;
      dx= l.char_def.Pkfont.dx;
      dy= l.char_def.Pkfont.dy;
      width= l.char_def.Pkfont.width;
      height= l.char_def.Pkfont.height;
      hoffset= l.char_def.Pkfont.hoffset;
      voffset= l.char_def.Pkfont.voffset;
      graymap= { 
        texture= gimage;
        x1= float l.x /. float layout_set.texture_width;
        x2= (float (l.x + width)) /. float layout_set.texture_width;
        y2= 
          float (layout_set.texture_height (* - 1 *) - l.y) /. 
            float layout_set.texture_height;
        y1= 
          (float (layout_set.texture_height - 1 - (l.y + height))) /. 
            float layout_set.texture_height}
     }) layout_set.layouts
;;

type t =
    { name : string;
      dpi : int;
      table : char_def Table.t;
      texture : ([`luminance_alpha], [`ubyte]) GlPix.t
    }
;;

let create_texture_from_pk fontname dpi =

  let filename = Search.font_path fontname dpi in
  let pk_font = Pkfont.load filename in
  let chardefs = pk_font.Pkfont.defs in

  let layout_set = 
    prerr_endline (Printf.sprintf "binpacking start %s %d" fontname dpi);
    try
      let result = binpack chardefs in
      prerr_endline "binpacking done";
      result
    with
    | e -> 
	prerr_endline (Printf.sprintf "binpacking failed (%s)"
			 (Printexc.to_string e));
	raise e
  in
  save_texture fontname dpi layout_set;
  let texture, chardefs = create_texture layout_set in
  let hash = Hashtbl.create 107 in 
  List.iter (fun graymap -> Hashtbl.add hash graymap.code graymap) chardefs;
  let table = Table.make (Hashtbl.find hash) in 
  { name= fontname;
    dpi= dpi;
    table= table;
    texture= texture }
;;

(*** Finding a given font ***)

let find =
  let htable = Hashtbl.create 257 in
  fun fontname dpi ->
    try Hashtbl.find htable (fontname, dpi)
    with Not_found ->
      try
	let font = create_texture_from_pk fontname dpi in
        Hashtbl.add htable (fontname, dpi) font;
        font
      with _ -> raise Not_found
;;

let find_char_def font = Table.get font.table;;
