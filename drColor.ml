open Special;;
open Texstack;;

type state = Dvicolor.color Texstack.t;;

let special st s =
  match split_string s 0 with
  | "color" :: "push" :: args ->
      push (Dvicolor.parse_args args) st#color
  | "color" :: "pop" :: [] ->
      pop st#color
  | _ -> () (* maybe print an error message *)
;;

