let debug = Options.debug "--debug-grdev" "Debug grdev module";;

open Grgtk;;

type color = Dvicolor.color;;

module Symbol = Symbol.Make(Grglyph);;
 
let close_dev () = 
  debug "close_dev";
  win#hide ()
;;

let resize_dev ageom =
  debug (Printf.sprintf "resize_dev: %s" (Ageometry.to_string ageom));
  if win#size <> (ageom.Ageometry.width, ageom.Ageometry.height) then 
    win#resize ~width: ageom.Ageometry.width ~height: ageom.Ageometry.height
;;

let open_dev ageom = 
  debug "open_dev";
  resize_dev ageom;
  win#show ();
  win#set_remember_mode true;
  win#set_display_mode !Options.global_display_mode
;;

let resize_dev ageom =
  (* win#hide () is required... why ? *)
  close_dev ();
  open_dev ageom
;;

let clear_dev () =
  debug "clear_dev";
  win#set_display_mode !Options.global_display_mode;
  win#clear ();
  Symbol.clear ()
;;
