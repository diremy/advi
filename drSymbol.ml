(*
(* Symbols *)

(*
let clear_symbols st = st.last_height <- 2;;
*)

let add_char st x y code glyph =
  let g : Symbol.g =
    { Symbol.fontname = st#dvi.cur_font.name;
      Symbol.fontratio = st#dvi.cur_font.ratio;
      Symbol.glyph = glyph
    } in
(*
  st#last_height <- (GrGlyph.get glyph).Glyph.voffset;
*)
  let s : Symbol.symbol = Symbol.Glyph g in
  Symbol.add st#color.top x y code s;;

let add_line st (line, file) =
  let x = st#dvi.x_origin + int_of_float (st#dvi.conv *. float st#dvi.h)
  and y = st#dvi.y_origin + int_of_float (st#dvi.conv *. float st#dvi.v) in
  Symbol.add st#color.top x y 0 (Symbol.Line (line, file));;

let add_blank nn st width =
  let x = st#dvi.x_origin + int_of_float (st#dvi.conv *. float st#dvi.h)
  and y = st#dvi.y_origin + int_of_float (st#dvi.conv *. float st#dvi.v)
  and w = int_of_float (st#dvi.conv *. float width) in
  Symbol.add st#color.top x y nn (Symbol.Space (w, 2 (* st#last_height *)))
;;

let add_rule st x y w h =
  Symbol.add st#color.top x y 0 (Symbol.Rule (w, h));;
*)

