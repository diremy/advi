exception Error of string

type t

type image = {
    ximage : OXimage.ximage;
    mask : Gdk.bitmap 
  };;

val width : t -> int
val height : t -> int
val hoffset : t -> int
val voffset : t -> int
val graymap : t -> string

val make : Glyph.t -> t
val get : t -> Glyph.t

val get_image : t -> Dvicolor.color * Dvicolor.color -> image
