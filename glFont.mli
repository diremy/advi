(* Create OpenGL based font *)

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
      graymap : graymap
    } 

type t;;

val find : string -> int -> t;;
val find_char_def : t -> int -> char_def;;

