exception Error of string

val debug : string -> unit

val root : Gdk.window
val visual : Gdk.visual
val clear_gc : Gdk.gc

val get_root_geometry : Gdk.window -> int * int * int * int
val sync : unit -> unit

module Colour :
  sig
    val gl_of_rgb : Color.rgb -> Gl.rgb
    val gl_of_dvi : int -> Gl.rgb
    val gdraw_of_rgb : Color.rgb -> GDraw.color
    val dvi_of_gdraw : GDraw.color -> Dvicolor.color
    val dvi_of_rgb : Color.rgb -> Dvicolor.color
    val gdraw_of_dvi : int -> GDraw.color
    val create : Image.rgb -> int
    val parse : int -> Image.rgb
    val gdraw_of_system : int -> GDraw.color
  end
