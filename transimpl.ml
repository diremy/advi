open Graphics
open Transitions

let current_transition = ref TransNone;;

let slide steps from =
  let w = Graphics.size_x () and h = Graphics.size_y () in
  let img = Graphics.get_image 0 0 w h in
  for i = steps - 1 downto 1 do
    let px,py =
      match from with
      |	DirLeft   -> -w/steps*i, 0
      |	DirBottom -> 0, -h/steps*i
      |	DirTop    -> 0, h/steps*i
      |	DirTopLeft   -> -w/steps*i, h/steps*i
      |	DirTopRight  -> w/steps*i, h/steps*i
      |	DirBottomRight -> w/steps*i, -h/steps*i
      |	DirBottomLeft  -> -w/steps*i, -h/steps*i
      |	DirRight | _ -> w/steps*i, 0
    in
    Graphics.draw_image img px py
  done;
;;

let wipe steps from =
  let w = Graphics.size_x () and h = Graphics.size_y () in
  (* bit naive implementation (=inefficient) *)
  for i = steps - 1 downto 1 do
    let px,py,w,h =
      match from with
      |	DirLeft   -> -w/steps*i, 0, w, h
      |	DirBottom -> 0, -h/steps*i, w, h
      |	DirTop    -> 0, h/steps*i, w, h
      |	DirTopLeft   -> -w/steps*i, h/steps*i, w, h
      |	DirTopRight  -> w/steps*i, h/steps*i, w, h
      |	DirBottomRight -> w/steps*i, -h/steps*i, w, h
      |	DirBottomLeft  -> -w/steps*i, -h/steps*i, w, h
      |	DirCenter -> 
	  let j = steps - i in
	  w/2 - w/(steps*2)*j,
	             h/2 - h/(steps*2)*j,
	              w/steps*j, h/steps*j
      |	DirRight | _ -> w/steps*i, 0, w, h
    in
    let img = Graphics.get_image px py w h in
    Graphics.draw_image img px py
  done;
;;

let block steps from =
  let w = Graphics.size_x () and h = Graphics.size_y () in
  let rec find_division dx dy =
    if dx * dy > steps then dx, dy
    else
      (* try to add dx 1 *)
      let dx1 = dx + 1 in
      let dy1 = h / (w / dx1) + 1 in
      (* try to add dy 1 *)
      let dy2 = dy + 1 in
      let dx2 = w / (h / dy2) + 1 in
      if dx1 * dy1 < dx2 * dy2 then find_division dx1 dy1 
      else find_division dx2 dy2
  in
  let dx,dy = find_division 1 1 in

  let bw = w / dx + (if w mod dx = 0 then 0 else 1) in
  let bh = h / dy + (if h mod dy = 0 then 0 else 1) in

  let img = Graphics.create_image bw bh in

  let swap order a b =
    let tmp = order.(a) in
    order.(a) <- order.(b);
    order.(b) <- tmp
  in

  let order =
    let priority =
      match from with
      | DirLeft -> fun x y -> x
      |	DirRight -> fun x y -> -x
      | DirBottom -> fun x y -> y
      |	DirTop -> fun x y -> -y
      |	DirTopLeft -> fun x y -> x-y
      |	DirTopRight -> fun x y -> -x-y
      |	DirBottomLeft -> fun x y -> x+y
      |	DirBottomRight -> fun x y -> -x+y
      |	DirCenter -> fun x y -> abs (x-dx/2) + abs (y-dy/2)
      | DirNone | _ -> fun x y -> Random.int (dx * dy)
    in
    let order = Array.init (dx * dy) (fun i -> 
      let x = i mod dx and y = i / dx in 
      priority x y, (x,y) )
    in
    Array.sort compare order;
    for i = 0 to dx * dy - 1 do
      let j = i + Random.int (dx * dy / 20) in
      if j < 0 || j >= dx * dy then ()
      else swap order i j
    done;
    order
  in
  for i = 0 to dx * dy - 1 do
    let _,(bx,by) = order.(i) in
    Graphics.blit_image img (bx * bw) (by * bh);
    Graphics.draw_image img (bx * bw) (by * bh)
  done
;;

let synchronize_transition () =
  if !current_transition = TransNone then ()
  else begin
    Graphics.remember_mode false;
    Graphics.display_mode true;
    begin match !current_transition with
    | TransSlide (step,from) -> slide step from
    | TransWipe (step,from) -> wipe step from
    | TransBlock (step,from) -> block step from
    | TransNone -> assert false
    end;
    Graphics.remember_mode true;
    Graphics.display_mode false
  end
;;


