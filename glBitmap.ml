let create ~width ~height =
  GlPix.create `bitmap ~width ~height ~format: `color_index
;;

(* returns (byte,bit) *)
let pos (bitmap : GlPix.bitmap) ~x ~y =
  GlPix.raw_pos bitmap ~x: (x/8) ~y, 7 - (x land 7)
;;

let to_raw = GlPix.to_raw;;

let of_raw raw ~width ~height =
  let size = width * height
  and len = Raw.length raw * 8 in
  if size > len then invalid_arg "GlBitmap.of_raw";
  { format = `bitmap; width = width; height = height; raw = raw }
;;
  
let get_byte (raw : [`bitmap] Raw.t) byte = Raw.get raw ~pos: byte;;

let get (raw : [`bitmap] Raw.t) byte bit =
  (Raw.get raw ~pos: byte) land (1 lsl bit) <> 0
;;

let set_byte (raw : [`bitmap] Raw.t) byte bits =
  Raw.set raw ~pos: byte bits
;;

let set (raw : [`bitmap] Raw.t) byte bit b =
  let c = Raw.get raw ~pos: byte in
  let c' = if b then c lor (1 lsl bit) else c lxor (1 lsl bit) in
  Raw.set raw ~pos: byte c'
;;

