exception Error of string

(*** Private glyphs ***)

open GrMisc;;
open Dvicolor;;

type image = {
    ximage : OXimage.ximage;
    mask : Gdk.bitmap 
  };;

type cache =
  | No_cache
  | Cached of (color * color) * image;;

type t = {
    glyph : Glyph.t;
    (* fst cache *)
    mutable cache : cache;
    (* snd cache *)
    mutable img_list : ((color * color) * image) list 
  }
	
let width g = g.glyph.Glyph.width
let height g = g.glyph.Glyph.height
let hoffset g = g.glyph.Glyph.hoffset
let voffset g = g.glyph.Glyph.voffset
let graymap g = g.glyph.Glyph.graymap

let make g =
  { glyph = g;
    cache = No_cache;
    img_list = [] };;

let get g = g.glyph;;

(* create a gradient table *)
let get_color_table =
  let htable = Hashtbl.create 257 in
  function (bg, fg as col) ->
    try Hashtbl.find htable col
    with Not_found ->
      let table = Array.make 256 0 in
      let r0 = (bg lsr 16) land 0xff
      and g0 = (bg lsr 8) land 0xff
      and b0 = bg land 0xff in
      let r1 = (fg lsr 16) land 0xff
      and g1 = (fg lsr 8) land 0xff
      and b1 = fg land 0xff in
      for i = 0 to 255 do
        let k = (255 - i) in
        let r = (k * r0 + i * r1) / 255
        and g = (k * g0 + i * g1) / 255
        and b = (k * b0 + i * b1) / 255 in
        table.(i) <- Colour.create {Color.r = r; 
				    Color.g = g;
				    Color.b = b}
      done;
      Hashtbl.add htable col table;
      table
;;

let mono_gc_white, mono_gc_black =
  let dummy_bmp = Gdk.Bitmap.create ~width:1 ~height:1 () in
  let gw = Gdk.GC.create dummy_bmp 
  and gb = Gdk.GC.create dummy_bmp in
  Gdk.GC.set_foreground gw (GDraw.color `WHITE);
  Gdk.GC.set_foreground gb (GDraw.color `BLACK);
  gw, gb
;;

let get_image g col =
  match g.cache with
  | Cached (c, img) when c = col -> img
  | _ ->
      let img =
        try List.assoc col g.img_list
        with Not_found ->
          let gmap = graymap g
          and w = width g
          and h = height g in
          (* We enforce [h <> 0] and [w <> 0] because
             ximage don't like zero-sized pixmaps. *)
	  let width = max w 1
	  and height = max h 1
	  in
          let img = OXimage.create ~kind:`FASTEST ~visual: visual 
	      ~width ~height
	  and mask = Gdk.Bitmap.create ~window: root ~width ~height ()
	  in
	  let table = get_color_table col
	  and p = ref 0 
	  in
          for i = 0 to h - 1 do
            for j = 0 to w - 1 do
	      img#unsafe_set j i table.(Char.code gmap.[!p]);
	      Gdk.Draw.point mask 
		(if Char.code gmap.[!p] <> 0 then mono_gc_white
		else mono_gc_black )
		~x: j ~y: i;
	      incr p
            done;
          done;
	  let img = { ximage = img; mask = mask }  in
          g.img_list <- (col, img) :: g.img_list;
          img
      in
      g.cache <- Cached(col, img);
      img
;;
