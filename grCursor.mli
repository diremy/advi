type mode =
  | Free | Busy | Pause | Disk | Question | Selection | Move
  | Resize | Resize_x | Resize_y
;;

class t : Gdk.window -> object
  method init : unit -> unit
  method set : mode -> unit

  method busyf : 'a 'b. ('a -> 'b) -> 'a -> 'b

  method set_busy : unit -> unit
  method unset_busy : unit -> unit
end
