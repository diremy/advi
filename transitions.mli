type direction = 
    DirRight | DirLeft | DirTop | DirBottom
  | DirTopRight | DirTopLeft | DirBottomRight | DirBottomLeft
  | DirCenter | DirNone 

type t =
    TransNone
  | TransSlide of int option * direction
  | TransWipe of int option * direction
  | TransBlock of int option * direction
;;
