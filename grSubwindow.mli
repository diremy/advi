class t : GPack.fixed -> object
    method create :
      x:int -> y:int -> width:int -> height:int -> GBin.event_box
    method destroy : GBin.event_box -> unit
    method move : GBin.event_box -> x:int -> y:int -> unit
    method resize : GBin.event_box -> width:int -> height:int -> unit
  end
;;

