open Freetype

type face = Freetype.face

type bitmap = string

type char_def = {
    code : int ;
    dx : int ;       (* scaled pixels *)
    dy : int ;       (* scaled pixels *)
    width : int ;    (* pixels *)
    height : int ;   (* pixels *)
    hoffset : int ;  (* pixels *)
    voffset : int ;  (* pixels *)
    mutable bitmap : bitmap
  } ;;

let load_face =
  let ftengine = ref None in
  fun filename ->
    let engine = 
	match !ftengine with
	| Some e -> e
	| None ->
	    let e = Freetype.init () in
	    ftengine := Some e;
	    e
    in
    let face,_ = new_face engine filename 0 in
    set_charmap face { platform_id=3; encoding_id=1 };
    face
;;
  
let build face dpi pt unicode =
  set_char_size face (float dpi) (float dpi) pt pt;
  let advx, advy = render_char face unicode [] Render_Mono in
  let bitmapinfo = get_bitmap_info face in
  let width = bitmapinfo.bitmap_width in
  let height = bitmapinfo.bitmap_height in
  let bitmap_len = (width * height + 7) / 8 in
  let bitmap = String.create bitmap_len in
  let is_black pos =
    let x = pos mod width in
    let y = pos / width in
    if y >= height then false
    else read_bitmap face x (height - y - 1) <> 0
  in
  let pos = ref 0 in
  for i = 0 to bitmap_len - 1 do
    let char = ref 0 in
    for j = 0 to 7 do
      char := !char lsl 1; 
      if is_black !pos then char := !char lor 0x01;
      incr pos
    done;
    bitmap.[i] <- char_of_int !char
  done;
  
  { code= unicode;
    dx= 0;
    dy= 0;
    width= width;
    height= height;
    hoffset= -bitmapinfo.bitmap_left;
    voffset= bitmapinfo.bitmap_top;
    bitmap= bitmap
  }	
;;

(* Having it here is strange, but this table is with the freetype interface *)
let jis2uni jiscode =
  let h = jiscode / 0x100 in
  let l = jiscode mod 0x100 in
  let pos = ((h - 0x21) * 94 + (l - 0x21)) * 2 in
  int_of_char Jis_table.table.[pos] * 0x100 + 
    int_of_char Jis_table.table.[pos+1] 
;;
