open Special;;
open DrDvi;;
open DrProc;;

type state = {
    (* TPIC specials state *)
    mutable tpic_pensize : float;
    mutable tpic_path : (float * float) list;
    mutable tpic_shading : float;
  }
;; 

let create () = {
  tpic_pensize= 0.0;
  tpic_path= [];
  tpic_shading= 0.0
}
;; 

(* Graphics primitives *)

let pensize_f (draw : GrGL.t) pensize f arg =
  draw#push [`line];
  draw#set_line_attributes ~width: pensize ();
  f arg;
  draw#pop ()
;;

let shade_f (draw : GrGL.t) shade f arg =
  (* this will be done gl blending *) 
  let (r,g,b) = draw#foreground in 
  let r = 1.00 -. r
  and g = 1.00 -. g
  and b = 1.00 -. b in
  let r = 1.00 -. (shade *. r)
  and g = 1.00 -. (shade *. g)
  and b = 1.00 -. (shade *. b) in
  draw#push [`current];
  draw#set_foreground (r,g,b);
  f arg;
  draw#pop ()

let draw_path st pts ~pensize =
  let draw = st#dvi.device#draw in
  pensize_f draw pensize (fun () -> draw#lines pts) ()
;;

let fill_path st pts ~shade =
  let draw = st#dvi.device#draw in
  shade_f draw shade (fun () -> draw#polygon ~filled:true pts) ()
;;

let draw_arc st ~x ~y ~rx ~ry ~start ~stop ~pensize =
  let draw = st#dvi.device#draw in
  pensize_f draw pensize (fun () -> draw#arc ~filled: false
      ~x:(x-rx) ~y:(y-ry) ~width:(rx*2) ~height:(ry*2)
      ~start ~angle:(stop -. start) ()) ()
;;

let fill_arc st ~x ~y ~rx ~ry ~start ~stop ~shade =
  let draw = st#dvi.device#draw in
  shade_f draw shade (fun () -> draw#arc ~filled: true 
      ~x:(x-rx) ~y:(y-ry) ~width:(rx*2) ~height:(ry*2)
      ~start ~angle:(stop -. start) ()) ()
;;

(* Support for TPIC specials. *)

let milli_inch_to_sp = Units.from_to Units.IN Units.SP 1e-3;;

let tpic_milli_inches s = parse_float s *. milli_inch_to_sp;;

let tpic_pen st = st#dvi.conv *. st#tpic.tpic_pensize +. 0.5;;

let tpic_x st x =
  st#dvi.x_origin + int_of_float (st#dvi.conv *. (float st#dvi.h +. x));;
let tpic_y st y =
  st#dvi.y_origin + int_of_float (st#dvi.conv *. (float st#dvi.v +. y));;

let tpic_flush_path st cntr =
  let path = Array.of_list (List.rev st#tpic.tpic_path) in
  (* Convert points in path to pixel coordinates *)
  let pixpath =
    Array.map (fun (x, y) -> (tpic_x st x, tpic_y st y)) path in
  (* If shading requested and path is closed, fill *)
  if st#tpic.tpic_shading >= 0.0 &&
     Array.length path >= 2 &&
     path.(0) = path.(Array.length path - 1) &&
     st#proc.visible then
       fill_path st (Array.to_list pixpath) ~shade:st#tpic.tpic_shading;
  (* If requested, draw outline of path *)
  if cntr && st#proc.visible then
    draw_path st (Array.to_list pixpath) ~pensize:(tpic_pen st);
  (* Reset path *)
  st#tpic.tpic_path <- [];
  st#tpic.tpic_shading <- 0.0;;

let dist (x0, y0) (x1, y1) = abs (x0 - x1) + abs (y0 - y1);;

let tpic_spline_path st =
  (* Code shamelessly stolen from xdvi *)
  let path =
    Array.of_list
      (List.map (fun (x, y) -> (tpic_x st x, tpic_y st y))
                (List.rev st#tpic.tpic_path)) in
  let p =
    Array.concat [[|path.(0)|]; path; [|path.(Array.length path - 1)|]] in
  let r = ref [] in
  for i = 0 to Array.length p - 3 do
    let steps = (dist p.(i) p.(i + 1) + dist p.(i + 1) p.(i + 2)) / 4 in
    let (x2, y2) = p.(i + 2)
    and (x1, y1) = p.(i + 1)
    and (x0, y0) = p.(i) in
    for j = 0 to steps - 1 do
      let w = (j * 1000 + 500) / steps in
      let t1 = w * w / 20 in
      let w = w - 500 in
      let t2 = (750000 - w * w) / 10 in
      let w = w - 500 in
      let t3 = w * w / 20 in
      let xp = (t1 * x2 + t2 * x1 + t3 * x0 + 50000) / 100000
      and yp = (t1 * y2 + t2 * y1 + t3 * y0 + 50000) / 100000 in
      r := (xp, yp) :: !r
    done
  done;
  if st#proc.visible then
    draw_path st (List.rev !r) ~pensize:(tpic_pen st);
  st#tpic.tpic_path <- [];
  st#tpic.tpic_shading <- 0.0;;

let rad_to_deg = 45.0 /. atan 1.0;;

let tpic_arc st x y rx ry s e cntr =
  let x = tpic_x st x
  and y = tpic_y st y
  and rx = int_of_float (st#dvi.conv *. rx)
  and ry = int_of_float (st#dvi.conv *. ry)
  and s = -. s *. rad_to_deg
  and e = -. e *. rad_to_deg in
  if st#proc.visible then begin
    (* If shading requested, fill the arc *)
    if st#tpic.tpic_shading >= 0.0 then
      fill_arc st ~x ~y ~rx ~ry ~start:s ~stop:e ~shade:st#tpic.tpic_shading;
    (* If requested, draw outline of arc *)
    if cntr then
      draw_arc st ~x ~y ~rx ~ry ~start:s ~stop:e ~pensize:(tpic_pen st);
    (* Reset shading *)
  end;
  st#tpic.tpic_shading <- 0.0;;

let special st s =
  match split_string s 0 with
  | "pn" :: size :: _ ->
      st#tpic.tpic_pensize <- tpic_milli_inches size
  | "pa" :: x :: y :: _ ->
      st#tpic.tpic_path <-
        (tpic_milli_inches x, tpic_milli_inches y) :: st#tpic.tpic_path
  | "fp" :: _ ->
      tpic_flush_path st true
  | "ip" :: _ ->
      tpic_flush_path st false
  | "da" :: _ -> (* TODO: dashed lines *)
      tpic_flush_path st true
  | "dt" :: _ -> (* TODO: dotted lines *)
      tpic_flush_path st true
  | "sp" :: _ -> (* TODO: dashed/dotted splines *)
      tpic_spline_path st
  | "ar" :: x :: y :: rx :: ry :: s :: e :: _ ->
      tpic_arc st (tpic_milli_inches x) (tpic_milli_inches y)
               (tpic_milli_inches rx) (tpic_milli_inches ry)
               (parse_float s) (parse_float e)
               true
  | "ia" :: x :: y :: rx :: ry :: s :: e :: _ ->
      tpic_arc st (tpic_milli_inches x) (tpic_milli_inches y)
               (tpic_milli_inches rx) (tpic_milli_inches ry)
               (parse_float s) (parse_float e)
               true
  | "sh" :: s :: _ ->
      st#tpic.tpic_shading <- parse_float s
  | "wh" :: _ ->
      st#tpic.tpic_shading <- 0.0
  | "bk" :: _ ->
      st#tpic.tpic_shading <- 1.0
  | s :: _ ->
      Misc.warning ("Unknown pic command: " ^ s)
  | _ -> ();;
(* End of TPIC hacks *)
