open Special;;
open Misc;;

let parse st s =
  let records = get_records s in
  let file =
    try unquote (List.assoc "psfile" records)
    with Not_found -> raise (Failure "psfile: invalid special") in
  (* prerr_endline ("PSFILE=" ^ file); *)
  (* bbox *)
  let llx, lly, urx, ury =
    try
      let llx = int_or_float_of_string (List.assoc "llx" records)
      and lly = int_or_float_of_string (List.assoc "lly" records)
      and urx = int_or_float_of_string (List.assoc "urx" records)
      and ury = int_or_float_of_string (List.assoc "ury" records) in
      (* prerr_endline
         ("BBOX=" ^ Printf.sprintf "%d %d %d %d" llx lly urx ury); *)
      llx, lly, urx, ury
    with
    | _ -> raise (Failure "psfile: no bbox") in
  let width, height = (* return Big Points *)
    let w = try int_of_string (List.assoc "rwi" records) with _ -> 0
    and h = try int_of_string (List.assoc "rhi" records) with _ -> 0 in
    match w, h with
    | 0, 0 -> float (urx - llx), float (ury - lly)
    | 0, _ ->
       let h = float h /. 10.0 in
       let w = float (urx - llx) *. (h /. float (ury - lly)) in
       w, h
    | _, 0 ->
       let w = float w /. 10.0 in
       let h = float (ury - lly) *. (w /. float (urx - llx)) in
       w, h
    | _, _ -> float w /. 10.0, float h /. 10.0 in
  let dpi = ldexp (float st#sdpi) (-16) in
  let width_pixel = truncate (width /. 72.0 *. dpi) in
  let height_pixel = truncate (height /. 72.0 *. dpi) in
  (* prerr_endline (Printf.sprintf "%dx%d pixel" width_pixel height_pixel);*)
  file, (llx, lly, urx, ury), (width_pixel, height_pixel)
;;

let special st s =
  try
    let file, bbox, size = parse st s in
    let x = st#x_origin + int_of_float (st#conv *. float st#h)
    and y = st#y_origin + int_of_float (st#conv *. float st#v) 
    in
    if st#visible then
(*
      let draw =
        if has_prefix "`" file then
          Dev.draw_img (zap_to_char ' ' file)
            GrImage.ScaleAuto false 1.0 st.blend (Some bbox)
        else Dev.draw_ps file bbox in
      draw size x y
*)
      (st#device : GrDev.dvidevice)#draw#image 
	{ GrImage.filename = file;
	  GrImage.width= fst size;
	  GrImage.height= snd size;
	  GrImage.bbox= Some bbox;
	  GrImage.antialiase= true;
	  GrImage.whitetransp= false } ~x ~y
  with
  | Failure s -> Misc.warning s
  | e -> Misc.warning (Printexc.to_string e)
;;
