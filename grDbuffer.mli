exception Out;;

class t : Gdk.window -> object
    inherit [[`window]] GrDrawable.t

    val super : [ `window] GrDrawable.t
    val mutable store : [ `pixmap] GrDrawable.t

    val mutable width : int
    val mutable height : int
    method width : int
    method height : int
    method size : int * int

    method resize : width:int -> height:int -> unit

    val mutable display_mode : bool
    val mutable remember_mode : bool
    val mutable mode_stack : (bool * bool) list
    method remember_mode : bool
    method display_mode : bool
    method set_display_mode : bool -> unit
    method set_remember_mode : bool -> unit
    method push_mode : display:bool -> remember:bool -> unit
    method pop_mode : unit -> unit

    method synchronize : unit -> unit

    val mutable pushed_gcs : Gdk.gc list
    method push_gc : Gdk.gc -> unit
    method push_new_gc : unit -> unit
    method pop_gc : unit -> unit

    method clear : unit -> unit

    method gr_op : ([ `pixmap] GrDrawable.t -> unit) -> unit
    method get_color : x:int -> y:int -> GDraw.color
    method get_ximage :
      x:int -> y:int -> width:int -> height:int -> OXimage.ximage
    method put_ximage :
      x:int ->
      y:int ->
      ?xsrc:int ->
      ?ysrc:int -> ?width:int -> ?height:int -> OXimage.ximage -> unit

end
