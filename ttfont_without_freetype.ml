open Freetype

type face

type bitmap = string

type char_def = {
    code : int ;
    dx : int ;       (* scaled pixels *)
    dy : int ;       (* scaled pixels *)
    width : int ;    (* pixels *)
    height : int ;   (* pixels *)
    hoffset : int ;  (* pixels *)
    voffset : int ;  (* pixels *)
    mutable bitmap : bitmap
  } ;;

let no_freetype _ = raise (Failure "no freetype library")
let load_face = no_freetype;;
let build = no_freetype;;

let jis_unicode_table = fun x -> x;;

