exception Error of string

type app_mode = 
  | Sticky      (* always on screen *)
  | Persistent  (* only on the page which created it, but kept alive *)
  | Respawn     (* respawn apps for each page visit *)
;;

type geom;;

type process = {
    name : string;
    mode : app_mode;
    pid : int;
    ebox : GBin.event_box option;
    geom : geom
  }
;;

class t : GrSubwindow.t -> object
  method launch : name: string -> mode: app_mode -> x: int -> y: int -> 
                  width: int -> height: int -> string -> unit
  method kill : string -> unit
  method unmap : string -> unit
  method map : string -> unit
  method move : string -> x: int -> y: int -> unit
  method destroy : unit -> unit

  method iter : (process -> unit) -> unit
end
;;
