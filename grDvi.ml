let debug = GrMisc.debug;;

let ignore_background =
    Options.flag false
    "-ignore-background"
    "\tIgnore background for antialiasing";;

exception Error of string

open Gaux
open Gdk

module DWidget = struct
  type t = [Gtk.fixed|`dwidget]
  module Signals = struct
    module S = GtkSignal
    let redraw : ([>`dwidget],_) S.t =
      { S.classe = `dwidget; S.name = "redraw";
        S.marshaller = S.marshal_unit }
    let usr1 : ([>`dwidget],_) S.t =
      { S.classe = `dwidget; S.name = "usr1";
        S.marshaller = S.marshal_unit }
    let timeout : ([>`dwidget],_) S.t =
      { S.classe = `dwidget; S.name = "timeout";
        S.marshaller = S.marshal_int }
  end
  let create : unit -> t Gtk.obj =
    let _,dwidget_new = GtkNew.make_new_widget
	~name:"DWidget" ~parent:GtkNew.FIXED 
	~signals:[Signals.redraw; Signals.usr1; 
		  (Obj.magic Signals.timeout : 'a )]
    in fun () -> GtkBase.Object.try_cast (dwidget_new ()) "DWidget"
end
;;

class dwidget_signals (obj : [>DWidget.t] Gtk.obj) = object
  inherit GContainer.container_signals obj
  method redraw = GtkSignal.connect ~sgn:DWidget.Signals.redraw obj ~after
  method usr1 = GtkSignal.connect ~sgn:DWidget.Signals.usr1 obj ~after
  method timeout = GtkSignal.connect ~sgn:DWidget.Signals.timeout obj ~after
end
;;

class dwidget_emits obj = object
  method redraw = GtkSignal.emit_unit obj ~sgn:DWidget.Signals.redraw
  method usr1 = GtkSignal.emit_unit obj ~sgn:DWidget.Signals.usr1
  method timeout ~abort = 
    GtkSignal.emit_int obj ~sgn:DWidget.Signals.timeout abort
end
;;

class dwidget obj = object
  inherit GContainer.container (obj : DWidget.t Gtk.obj)
  method put w = GtkPack.Fixed.put obj (GObj.as_widget w)
  method move w = GtkPack.Fixed.move obj (GObj.as_widget w)
  method connect = new dwidget_signals obj
  method event = new GObj.event_ops obj
  method emit = new dwidget_emits obj
end
;;

let dwidget ?border_width ?width ?height ?packing ?show () =
  let w = DWidget.create () in
  GtkBase.Container.set w ?border_width ?width ?height;
  GObj.pack_return (new dwidget w) ~packing ~show
;;


type mode = [`NORMAL | `WAIT | `WAIT_FORCE]

let string_of_mode = function
  | `NORMAL -> "normal"
  | `WAIT -> "wait"
  | `WAIT_FORCE -> "wait_force"
;;

class dviwidget ~width ~height w =
  let _ = GtkBase.Widget.realize w in
  let window = GtkBase.Widget.window w in
  let cursor = new GrCursor.t window in
  let subwin_dummy = new GrSubwindow.dummy in
  let  glarea = GlGtk.area [`RGBA; `DOUBLEBUFFER; `DEPTH_SIZE 8;
			    `BUFFER_SIZE 16] ~width ~height ()
  in
  let draw = new GrGL.t glarea in
  object (self)
    inherit dwidget w as super

    val mutable subwindow = subwin_dummy
    val mutable embed = new GrEmbed.t subwin_dummy

    val mutable width = width
    val mutable height = height
    method width = width
    method height = height
    method size = width, height
    method resize ~width:w ~height:h =
      width <- w;
      height <- h;
      glarea#misc#set_geometry ~width:w ~height:h () ;
      self#misc#set_geometry ~width:w ~height:h ()
      
    method cursor = cursor
    method subwindow = subwindow
    method embed = embed

    val mutable mode = (`NORMAL : mode)
    method mode = mode

    val handlers = Hashtbl.create 17

    method set_mode m = 
      debug ("set_mode " ^ string_of_mode m);
      mode <- m;
      Hashtbl.iter (fun m (w,id) ->
	if mode = m then w#misc#handler_unblock id
	else w#misc#handler_block id) handlers

    method register_handler m (w : GObj.widget) id =
      Hashtbl.add handlers m (w,id);
      if mode <> m then w#misc#handler_block id

    method break_sleep () =
      debug "break_sleep";
      if mode = `WAIT then GrSleep.break_all_timeouts ()

    method sleep ~breakable ~sec ~cont =
      debug "sleep";
      if breakable then self#set_mode `WAIT else self#set_mode `WAIT_FORCE;
      GrSleep.f ~sec ~cont: (fun s ->
	self#set_mode `NORMAL;
	cont s)

    method destroy () = embed#destroy (); super#destroy ()
 
    method draw = draw

    initializer
      subwindow <- new GrSubwindow.t (self :> GPack.fixed);
      embed <- new GrEmbed.t subwindow;
prerr_endline (Printf.sprintf "glarea set_geometry %dx%d" width height);
      glarea#misc#set_geometry ~width ~height ();
      self#put (glarea :> GObj.widget) ~x:0 ~y:0
  end
;;

(* not used *)
let dviwidget ?border_width ~width ~height ?packing ?show () =
  let w = DWidget.create () in
  GtkBase.Container.set w ?border_width ~width ~height;
  GObj.pack_return (new dviwidget ~width ~height w) ~packing ~show
;;


