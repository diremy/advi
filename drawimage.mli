type ratiopts =
   | ScaleX        (* scale x coords, but keep original Ratio *)
   | ScaleY        (* scale y coords, but keep original Ratio *)
   | Original      (* respect original size                   *)
   | FreeScale     (* scale to fit requested area             *)
;;
val f : string -> bool -> float -> (int -> int -> int) option ->
  (int * int * int * int) option -> 
    ratiopts -> (int * int) -> (int * int) -> unit
(* [f filename whitetransp alpha blend 
      (llx,lly,urx,ury) (width,height) (x0,y0)]
   draws an eps [filename] with bounding box [(llx,lly,urx,ury)]
   in the size [(width,height)] pixels at [x0,y0] (top-left corner).
   If [whitetransp] is true, white pixels are treated as transparent.
   [alpha] specifies the alpha level of the image.
   [blend] is the color blending function for rendering *)

val clean_cache : unit -> unit
