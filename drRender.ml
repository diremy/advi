open DrDvi;;
open DrProc;;
open Texstack;;

class type state = object
  method dvi : DrDvi.state
  method color : DrColor.state
  method proc : DrProc.state
end
;;

(*** Rendering primitives ***)

let fnt st n =
  let (mtable, gtable, cfont) =
    try
      let cfont = Table.get st#dvi.cdvi.font_table n in
      (cfont.mtable, get_gtable cfont st#dvi.sdpi, cfont)
    with Not_found -> (dummy_mtable, dummy_gtable, dummy_font) in
  st#dvi.cur_mtable <- mtable;
  st#dvi.cur_gtable <- gtable;
  st#dvi.cur_font <- cfont
;;

let put st code =
  try
    let x = st#dvi.x_origin + int_of_float (st#dvi.conv *. float st#dvi.h)
    and y = st#dvi.y_origin + int_of_float (st#dvi.conv *. float st#dvi.v)
    and glyph = Table.get st#dvi.cur_gtable code in
    if st#proc.visible then begin
(*
      begin match st#html with
      | Some _ -> st#draw_html <- (x, y, glyph) :: st#draw_html
      | None -> ()
      end;
*)
      st#dvi.device#draw#glyph ~color: st#color.top 
	~x ~y 	(glyph : GlGlyph.t);
      (* add_char st x y code glyph; *)
    end
  with _ -> ()
;;

let set st code =
  put st code;
  try
    let (dx, dy) = Table.get st#dvi.cur_mtable code in
    st#dvi.h <- st#dvi.h + dx;
    st#dvi.v <- st#dvi.v + dy
  with _ -> ()
;;

let put_rule st a b =
  let x = st#dvi.x_origin + int_of_float (st#dvi.conv *. float st#dvi.h)
  and y = st#dvi.y_origin + int_of_float (st#dvi.conv *. float st#dvi.v)
  and w = int_of_float (ceil (st#dvi.conv *. float b))
  and h = int_of_float (ceil (st#dvi.conv *. float a)) in
  (* add_rule st x (y-h) w h; *)
  if true (* st#proc.visible *) then 
    st#dvi.device#draw#rectangle ~x ~y:(y - h) ~width:w ~height:h ~filled:true ()
;;

let set_rule st a b =
  put_rule st a b;
  st#dvi.h <- st#dvi.h + b
;;
