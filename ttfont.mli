type face

type bitmap = string
and char_def = {
  code : int;
  dx : int;
  dy : int;
  width : int;
  height : int;
  hoffset : int;
  voffset : int;
  mutable bitmap : bitmap;
} 

val load_face : string -> face
val build : face -> int -> int -> int -> char_def
val jis2uni : int -> int
