let debug = GrMisc.debug;;

let ignore_background =
    Options.flag false
    "--ignore-background"
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

class dviDbuffer window = object (self)
  inherit GrDbuffer.t window as super

  val glyphgc = Gdk.GC.create GrMisc.root

  method private get_bg_color x y w h =
    if !ignore_background then 0xffffff else begin
      let candidates = [ x, y-1; x+w-1, y-1; x, y+h; x+w-1, y+h ] in
      let (colors, (rs,gs,bs)) = 
	List.fold_left (fun (colors, (rs,gs,bs)) (x,y) ->
	  try
	    match super#get_color ~x ~y with
	    | `RGB (r,g,b) -> colors + 1, (rs + r, gs + g, bs + b)
	    | _ -> raise (Error "#get_color returned non rgb")
	  with
	  | Error s -> raise (Error s)
	  | GrDbuffer.Out -> colors, (rs,gs,bs)) (0, (0,0,0)) candidates
      in
      if colors = 0 then 0xffffff else
      GrMisc.Colour.dvi_of_gdraw (`RGB (rs/colors, gs/colors, bs/colors))
    end

  method glyph ~color g ~x:x0 ~y:y0 =
    let w = GrGlyph.width g
    and h = GrGlyph.height g in
    let x = x0 - GrGlyph.hoffset g
    and y = y0 - GrGlyph.voffset g in
    let xmax, ymax = super#size in
    if x + w > 0 && x < xmax && y + h > 0 && y < ymax then begin
      let xsrc, xdst = if x < 0 then -x,0 else 0,x in
      let ysrc, ydst = if y < 0 then -y,0 else 0,y in
      let xw = x + w in
      let yh = y + h in
      let width = if xw > xmax then xmax - xdst else xw - xdst in
      let height = if yh > ymax then ymax - ydst else yh - ydst in
      let bg = self#get_bg_color x y w h in
      let img = GrGlyph.get_image g (bg, color) in
      let gc_backup = super#gc in
      super#set_gc glyphgc;
      super#set_clip_mask img.GrGlyph.mask;
      super#set_clip_origin ~x ~y;
      super#put_ximage ~x:xdst ~y:ydst ~xsrc ~ysrc ~width ~height 
	img.GrGlyph.ximage;
      super#set_gc gc_backup
    end
end

type mode = [`NORMAL | `WAIT | `WAIT_FORCE]

let string_of_mode = function
  | `NORMAL -> "normal"
  | `WAIT -> "wait"
  | `WAIT_FORCE -> "wait_force"
;;

class dviwidget w =
  let _ = GtkBase.Widget.realize w in
  let window = GtkBase.Widget.window w in
  let draw = new dviDbuffer window in
  let cursor = new GrCursor.t window in
  object (self)
    inherit dwidget w

    method draw = draw

    method width = draw#width
    method height = draw#height
    method size = draw#size
    method resize ~width:w ~height:h =
      draw#resize ~width:w ~height:h;
      self#misc#set_geometry ~width:w ~height:h ()
      
    method cursor = cursor
    method subwindow = new GrSubwindow.t (self :> GPack.fixed)

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

    method sleep ~breakable ~sec ~cont =
      debug "sleep";
      if breakable then self#set_mode `WAIT else self#set_mode `WAIT_FORCE;
      GrSleep.f ~sec ~cont: (fun s ->
	self#set_mode `NORMAL;
	cont s)
  end
;;

(* not used *)
let dviwidget ?border_width ~width ~height ?packing ?show () =
  let w = DWidget.create () in
  GtkBase.Container.set w ?border_width ~width ~height;
  GObj.pack_return (new dviwidget w) ~packing ~show
;;


