let debug = GrMisc.debug;;

open Ageometry

(*
module Symbol = Symbol.Make(GlGlyph);;
*)

let offset = function
  | No_offset -> None
  | Plus x -> Some x
  | Minus x -> Some (-x)
;;

let idref = ref 0;;
let devices = Hashtbl.create 17;;

open GrEmbed;;

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
  let id = incr idref; !idref in
  object (self)
    inherit GrDvi.dviwidget ~width: ageom.width ~height: ageom.height w as super

    method win = win

    method set_geometry ageom =
      super#resize ~width: ageom.width ~height: ageom.height;
      win#misc#set_geometry ?x: (offset ageom.xoffset) 
	?y: (offset ageom.yoffset) ();

    method show () = win#misc#show ()

    method hide () = win#misc#hide ()

    method clear () =
      self#embed#iter (fun proc -> 
	match proc.mode with
	| Sticky -> ()
	| Persistent -> self#embed#unmap proc.name
	| Respawn -> self#embed#kill proc.name);
      super#draw#clear ()(*
;
      Symbol.clear ()
*)

    method destroy () =
      Hashtbl.remove devices self#id;
      super#destroy ()

    method id = id

    (* this registers a callback function which is executed only once
       when it is firstly exposed *)
    method set_init (init : unit -> unit) =
      let id = ref None in
      id := Some (self#event#connect#expose ~callback: (fun _ ->
	debug "first expose of device";
	begin match !id with
	| Some id -> self#misc#disconnect id
	| None -> () end;
	init (); true))
end

let dvidevice ageom = 
  let dev = new dvidevice ageom in

  Hashtbl.add devices dev#id dev;
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

let destroy_all () =
  let ds = ref [] in
  Hashtbl.iter (fun id dev -> ds := dev :: !ds) devices;
  if !ds <> [] then begin
    Misc.warning "destroy all the existing devices...";
    List.iter (fun dev -> dev#destroy ()) !ds
  end
;;
