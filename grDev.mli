module Symbol : Symbol.T with type glyph = GrGlyph.t;;

class dvidevice : Ageometry.t -> object
  inherit GrDvi.dviwidget

  method win : GWindow.window

  method set_geometry : Ageometry.t -> unit
  method show : unit -> unit
  method hide : unit -> unit

  method clear : unit -> unit

  method id : int
      
  method set_init : (unit -> unit) -> unit
end

val dvidevice : Ageometry.t -> dvidevice

val destroy_all : unit -> unit
