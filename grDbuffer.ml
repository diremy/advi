open Gdk;;
open GrMisc;;

exception Out;;

let debug = GrMisc.debug;;

let grsync = ref false;;

Options.add
  "--debug-grsync"
  (Arg.Unit (fun x -> grsync := true))
    "\tsynchronize graphics operations";;

class t (window : Gdk.window) =
  let w, h = Gdk.Window.get_size window in
  let create_store ~width ~height = 
    let pix = Gdk.Pixmap.create ~width ~height ~window: root () in
    new GrDrawable.t pix
  in
  object (self)
    val mutable super = new GrDrawable.t window
    val mutable store = create_store ~width:w ~height:h

    val mutable width = w
    val mutable height = h
    method width = width
    method height = height
    method size = width, height
    
    method resize ~width:w ~height:h =
      debug (Printf.sprintf "resize %dx%d" w h);
      (* reinitialization of store. stored content will be lost *)	
      store <- create_store ~width:w ~height:h;
      self#clear ();
      width <- w;
      height <- h;
      
    val mutable display_mode = true
    val mutable remember_mode = true
    method display_mode = display_mode
    method remember_mode = remember_mode

    method set_display_mode v = display_mode <- v
    method set_remember_mode v = remember_mode <- v
    val mutable mode_stack = []
    method push_mode ~display ~remember =
      mode_stack <- (display_mode, remember_mode) :: mode_stack;
      self#set_display_mode display;
      self#set_remember_mode remember
    method pop_mode () =
      let d,r = 
	try List.hd mode_stack with _ -> raise (Failure "mode stack empty")
      in
      self#set_display_mode d;
      self#set_remember_mode r

    method synchronize () =
      debug "synchronize()";
      super#put_pixmap ~x: 0 ~y: 0 store#drawable

    method gr_op f =
      if !grsync then begin
	f (Obj.magic super : [`pixmap] GrDrawable.t);
	if remember_mode then f store;
	sync ()
      end else begin
	if display_mode then f (Obj.magic super : [`pixmap] GrDrawable.t);
	if remember_mode then f store
      end

    method clear () =
      if display_mode || !grsync then
	Gdk.Draw.rectangle super#drawable clear_gc ~x:0 ~y:0 ~width ~height
	  ~filled: true ();
      if remember_mode then
	Gdk.Draw.rectangle store#drawable clear_gc ~x:0 ~y:0 ~width ~height
	  ~filled: true ()

    method arc ~x ~y ~width ~height ?filled ?start ?angle () = 
      self#gr_op (fun v -> v#arc 
	  ~x ~y ~width ~height ?filled ?start ?angle ())

    method line ~x ~y ~x:x2 ~y:y2 =
      self#gr_op (fun v -> v#line ~x ~y ~x:x2 ~y:y2)
    method lines pts = self#gr_op (fun v -> v#lines pts)
    method point ~x ~y = self#gr_op (fun v -> v#point ~x ~y)
    method points pts = self#gr_op (fun v -> v#points pts)
    method polygon ?filled pts = self#gr_op (fun v -> v#polygon ?filled pts)
    method put_image ~x ~y ?xsrc ?ysrc ?width ?height img =
      self#gr_op (fun v -> v#put_image ~x ~y ?xsrc ?ysrc ?width ?height img)
    method put_pixmap ~x ~y ?xsrc ?ysrc ?width ?height pix =
      self#gr_op (fun v -> v#put_pixmap ~x ~y ?xsrc ?ysrc ?width ?height pix)
    method put_rgb_data ~width ~height ?x ?y ?dither ?row_stride region =
      self#gr_op (fun v -> v#put_rgb_data 
	  ~width ~height ?x ?y ?dither ?row_stride region)
    method rectangle ~x ~y ~width ~height ?filled () =
      self#gr_op (fun v -> v#rectangle ~x ~y ~width ~height ?filled ())
    method segments segs = self#gr_op (fun v -> v#segments segs)
    method string s ~font ~x ~y = 
      self#gr_op (fun v -> v#string s ~font ~x ~y)

    method color = super#color
    method set_background = super#set_background
    method set_clip_mask = super#set_clip_mask
    method set_clip_origin = super#set_clip_origin
    method set_clip_rectangle = super#set_clip_rectangle
    method set_clip_region = super#set_clip_region
    method set_foreground = super#set_foreground
    method set_line_attributes = super#set_line_attributes
    method gc_values = super#gc_values

    method gc = super#gc
    method set_gc gc = super#set_gc gc; store#set_gc gc

    val mutable pushed_gcs = []

    method push_gc gc =
      pushed_gcs <- gc :: pushed_gcs;
      self#set_gc gc

    method push_new_gc () =
      let new_gc = GC.create root in
      pushed_gcs <- self#gc :: pushed_gcs;
      self#set_gc new_gc

    method pop_gc () =
      try
	let old_gc = List.hd pushed_gcs in
	pushed_gcs <- List.tl pushed_gcs;
	self#set_gc old_gc
      with
      |	_ -> raise (Error "gc stack empty")

    method put_ximage ~x ~y ?xsrc ?ysrc ?width ?height 
	(ximg : OXimage.ximage) =
      self#gr_op (fun v -> 
	v#put_image ~x ~y ?xsrc ?ysrc ?width ?height ximg#data)
    method get_ximage = OXimage.get_image store#drawable

    method get_color ~x ~y =
      if x < 0 || y < 0 || x >= width || y >= width then raise Out;
      let ximg = self#get_ximage ~x ~y ~width:1 ~height: 1 in
      let c = Colour.gdraw_of_system (ximg#unsafe_get 0 0) in
      ximg#destroy;
      c

    method drawable = 
      (raise (Error "#drawable not accessible") : Gdk.window)
  end
;;
