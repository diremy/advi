exception Error of string

module DWidget : sig
  type t = [ `base | `container | `dwidget | `fixed | `widget]
  module Signals : sig
    val redraw : ([> `dwidget], unit -> unit) GtkSignal.t
    val usr1 : ([> `dwidget], unit -> unit) GtkSignal.t
    val timeout : ([> `dwidget], int -> unit) GtkSignal.t
  end
  val create : unit -> t Gtk.obj
end
;;

class dwidget_signals : 'a Gtk.obj ->
  object
    inherit GContainer.container_signals
    constraint 'a = [> DWidget.t]
    val obj : 'a Gtk.obj
    method redraw : callback:(unit -> unit) -> GtkSignal.id
    method usr1 : callback:(unit -> unit) -> GtkSignal.id
    method timeout : callback:(int -> unit) -> GtkSignal.id
  end
;;

class dwidget_emits : [> `dwidget] Gtk.obj -> object 
  method redraw : unit 
  method usr1 : unit
  method timeout : abort: int -> unit
end

class dwidget : DWidget.t Gtk.obj -> object
  inherit GContainer.container
  val obj : DWidget.t Gtk.obj
  method emit : dwidget_emits
  method move : GObj.widget -> x:int -> y:int -> unit
  method put : GObj.widget -> x:int -> y:int -> unit
  method connect : dwidget_signals
  method event : GObj.event_ops
end
;;

type mode = [`NORMAL | `WAIT | `WAIT_FORCE]

class dviwidget : width: int -> height: int -> DWidget.t Gtk.obj ->
  object
  inherit dwidget

  method width : int
  method height : int
  method size : int * int
  method resize : width: int -> height: int -> unit

  method cursor : GrCursor.t
  method subwindow : GrSubwindow.t
  method embed : GrEmbed.t

  method mode : mode
  method set_mode : mode -> unit
  method register_handler : mode -> GObj.widget -> GtkSignal.id -> unit

  method sleep : breakable: bool -> sec: float -> 
    cont: (GrSleep.state -> unit) -> unit
  method break_sleep : unit -> unit

  method draw : GrGL.t
end

val dviwidget : ?border_width: int -> width: int -> height: int -> 
  ?packing:(GObj.widget -> unit) -> ?show: bool -> unit -> dviwidget


