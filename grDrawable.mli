class ['a] t : ?colormap: Gdk.colormap -> 'a Gdk.drawable -> object

    method drawable : 'a Gdk.drawable

    method gc : Gdk.gc
    method gc_values : Gdk.GC.values

    method color : GDraw.color -> Gdk.Color.t

    method set_background : GDraw.color -> unit
    method set_clip_mask : Gdk.bitmap -> unit
    method set_clip_origin : x:int -> y:int -> unit
    method set_clip_rectangle : Gdk.Rectangle.t -> unit
    method set_clip_region : Gdk.region -> unit
    method set_foreground : GDraw.color -> unit
    method set_gc : Gdk.gc -> unit
    method set_line_attributes :
      ?width:int ->
      ?style:Gdk.GC.gdkLineStyle ->
      ?cap:Gdk.GC.gdkCapStyle -> ?join:Gdk.GC.gdkJoinStyle -> unit -> unit
    method size : int * int

    method arc :
      x:int ->
      y:int ->
      width:int ->
      height:int ->
      ?filled:bool -> ?start:float -> ?angle:float -> unit -> unit
    method line : x:int -> y:int -> x:int -> y:int -> unit
    method lines : (int * int) list -> unit
    method point : x:int -> y:int -> unit
    method points : (int * int) list -> unit
    method polygon : ?filled:bool -> (int * int) list -> unit
    method put_image :
      x:int ->
      y:int ->
      ?xsrc:int ->
      ?ysrc:int -> ?width:int -> ?height:int -> Gdk.image -> unit
    method put_pixmap :
      x:int ->
      y:int ->
      ?xsrc:int ->
      ?ysrc:int -> ?width:int -> ?height:int -> Gdk.pixmap -> unit
    method put_rgb_data :
      width:int ->
      height:int ->
      ?x:int ->
      ?y:int ->
      ?dither:Gdk.Tags.rgb_dither ->
      ?row_stride:int -> Gpointer.region -> unit
    method rectangle :
      x:int ->
      y:int -> width:int -> height:int -> ?filled:bool -> unit -> unit
    method segments : ((int * int) * (int * int)) list -> unit
    method string : string -> font:Gdk.font -> x:int -> y:int -> unit
end
;;

