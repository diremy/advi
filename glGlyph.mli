type t = {
    width : int;
    height : int;
    voffset : int;
    hoffset : int;
    graymap : GlFont.graymap
  } 
;;

type char_def = GlFont.char_def;;

val from_char_def : char_def -> float -> t;;
