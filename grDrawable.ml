open Gaux;;
open Gdk;;

class ['a] t ?(colormap = GtkBase.Widget.get_default_colormap ()) w =
object (self)
  val colormap = colormap
  val mutable gc = GC.create w
  val w : 'a Gdk.drawable = w

  method color = GDraw.color ~colormap

  method set_foreground col = GC.set_foreground gc (self#color col)
  method set_background col = GC.set_background gc (self#color col)
  method size = Window.get_size w
  method gc_values = GC.get_values gc
  method set_clip_region = GC.set_clip_region gc
  method set_clip_origin = GC.set_clip_origin gc
  method set_clip_mask = GC.set_clip_mask gc
  method set_clip_rectangle = GC.set_clip_rectangle gc
  method set_line_attributes ?width ?style ?cap ?join () =
    let v = GC.get_values gc in
    GC.set_line_attributes gc
      ~width:(default v.GC.line_width ~opt:width)
      ~style:(default v.GC.line_style ~opt:style)
      ~cap:(default v.GC.cap_style ~opt:cap)
      ~join:(default v.GC.join_style ~opt:join)
  method point = Draw.point w gc
  method line = Draw.line w gc
  method rectangle = Draw.rectangle w gc
  method arc = Draw.arc w gc
  method polygon = Draw.polygon w gc
  method string s = Draw.string w gc s
  method put_image ~x ~y = Draw.image w gc ~xdest:x ~ydest:y
  method put_pixmap ~x ~y = Draw.pixmap w gc ~xdest:x ~ydest:y
  method put_rgb_data = Rgb.draw_image w gc
  method points = Draw.points w gc
  method lines = Draw.lines w gc
  method segments = Draw.segments w gc

  method drawable = w

  method gc = gc
  method set_gc newgc = gc <- newgc
end
;;

