exception Error of string

class type subwindow = object
  method parent : GPack.fixed
  method create : init:(GBin.event_box -> unit) ->
      x:int -> y:int -> width:int -> height:int -> GBin.event_box
  method destroy : GBin.event_box -> unit
  method unmap : GBin.event_box -> unit
  method map : GBin.event_box -> unit
  method move : GBin.event_box -> x:int -> y:int -> unit
  method resize : GBin.event_box -> width:int -> height:int -> unit
end
;;

class t : GPack.fixed -> subwindow

class dummy : subwindow
