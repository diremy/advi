(* screen shot *)

open Image
let cntr = ref 0 

let save () =
  let screen_w = Graphics.size_x () and screen_h = Graphics.size_y () in
  let img = Graphic_image.get_image 0 0 screen_w screen_h in
  Image.save  (Printf.sprintf "shot%d.jpg" !cntr) None [] (Rgb24 img)
;;
