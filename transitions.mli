type direction = 
    DirRight | DirLeft | DirTop | DirBottom
  | DirTopRight | DirTopLeft | DirBottomRight | DirBottomLeft
  | DirCenter | DirNone 

type t =
    TransNone
  | TransSlide of int * direction
  | TransWipe of int * direction
  | TransBlock of int * direction
;;
