let debug = GrMisc.debug;;

open Ageometry

module Symbol = Symbol.Make(GrGlyph);;

let offset = function
  | No_offset -> None
  | Plus x -> Some x
  | Minus x -> Some (-x)
;;

class dvidevice ageom = 
  let win = GWindow.window ~title: "Advi" ~auto_shrink: true
      ?x: (offset ageom.xoffset) ?y: (offset ageom.yoffset) ()
  in

  let w = 
    let w = GrDvi.DWidget.create () in
    GtkBase.Container.set w ~width: ageom.width ~height: ageom.height;
    GtkBase.Container.add win#as_window w;
    GtkBase.Widget.show w;
    w
  in

  object (self)
    inherit GrDvi.dviwidget w as super

    method win = win

    method set_geometry ageom =
      super#resize ~width: ageom.width ~height: ageom.height;
      win#misc#set_geometry ?x: (offset ageom.xoffset) 
	?y: (offset ageom.yoffset) ();

    method show () =
      win#misc#show ();
      self#draw#set_remember_mode true;
      self#draw#set_display_mode !Options.global_display_mode

    method hide () = win#misc#hide ()

    method clear () =
      self#draw#set_display_mode !Options.global_display_mode;
      super#draw#clear ();
      Symbol.clear ()
end

let dvidevice ageom = 
  let dev = new dvidevice ageom in
  ignore (dev#connect#redraw 
	    ~callback: (fun () -> prerr_endline "redraw signal!"));
  ignore (dev#misc#connect#draw ~callback:(fun rect ->
    debug (Printf.sprintf "draw: %dx%d+%d+%d" rect.Gtk.x rect.Gtk.y
	     rect.Gtk.width rect.Gtk.height);
    dev#draw#synchronize ()));
  ignore (dev#event#connect#expose ~callback: (fun _ ->
    debug "expose";
    dev#draw#synchronize (); true));
  dev
;;
