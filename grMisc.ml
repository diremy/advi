let debug = Options.debug ~label: "gr" "-debug-gr" "Debug graphic engine";;

exception Error of string

(* graphic initialization *)
let _ = GMain.Main.init ();;

let root = Gdk.Window.root_parent ();;
let visual = Gdk.Window.get_visual root;;
let clear_gc = Gdk.GC.create root;;
Gdk.GC.set_foreground clear_gc (GDraw.color `WHITE);;

(* misc basic functions *)
let get_root_geometry win =
  let w, h = Gdk.Window.get_size win in
  let rec aux win =
    let x, y = Gdk.Window.get_position win in
    let parent = Gdk.Window.get_parent win in
    if Gdk.Window.get_xwindow parent = Gdk.Window.get_xwindow root then x,y
    else begin
      let px, py = aux parent in px + x, py + y 
    end
  in
  let x, y = aux win in
  w, h, x, y
;;

let sync () = while Glib.Main.iteration false do () done;;

(* color conversions *)
module Colour = struct
  let gl_of_rgb rgb = 
    (float rgb.Color.r) /. 255.0,
    (float rgb.Color.g) /. 255.0,
    (float rgb.Color.b) /. 255.0
    
  let gl_of_dvi spec =
    let r = spec lsr 16 in
    let g = (spec land 0x00ff00) lsr 8 in
    let b = spec land 0x0000ff in
    float r /. 255.0, 
    float g /. 255.0, 
    float b /. 255.0

  let gdraw_of_rgb rgb =
    (`RGB (rgb.Color.r * 257, 
	   rgb.Color.g * 257, 
	   rgb.Color.b * 257) : GDraw.color)

  let dvi_of_gdraw = function
    | `RGB (r,g,b) -> (r/257) lsl 16 + (g/257) lsl 8 + (b/257)
    | _ -> raise (Error "dvi_of_gdraw: non RGB")

  let dvi_of_rgb rgb =
    Dvicolor.rgb rgb.Color.r rgb.Color.g rgb.Color.b

  let gdraw_of_dvi spec =
    let r = spec lsr 16 in
    let g = (spec land 0x00ff00) lsr 8 in
    let b = spec land 0x0000ff in
    (`RGB(r * 257, g * 257, b * 257) : GDraw.color)

  (* We can use color context ? *)    
  let create = 
    Ximage.Truecolor.color_creator visual

  let parse = 
    Ximage.Truecolor.color_parser visual

  let gdraw_of_system c = gdraw_of_rgb (parse c)
end;;
