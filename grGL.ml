class t (glarea : GlGtk.area) = object (self)

  val mutable width = 0
  val mutable height = 0

  method synchronize () = 
    prerr_endline "glarea sync";
    glarea#swap_buffers ()

  method clear () = 
    GlClear.clear [`color; `depth];

    GlDraw.color (1.0, 0.8, 0.8);
    GlDraw.begins `quads;
    GlDraw.vertex2 (-10.0, -10.0);
    GlDraw.vertex2 (-10.0, 10.0);
    GlDraw.vertex2 (10.0, 10.0);
    GlDraw.vertex2 (10.0, -10.0);
    GlDraw.ends ()


  method arc ~x ~y ~width ~height ?(filled=false) ?(start=0.0) ?(angle=360.0) () = 
    prerr_endline (Printf.sprintf "%d %d %d %d %B %f %f"
		     x y width height filled start angle);
    ()

  method set_line_attributes ?width: pensize () =
    match pensize with
    | Some ps -> GlDraw.line_width ps
    | None -> ()

  method line ~x ~y ~x:x2 ~y:y2 =
    GlDraw.begins `line_strip;
    GlDraw.vertex2 (float x, float y);
    GlDraw.vertex2 (float x2, float y2);
    GlDraw.ends ()

  method lines pts = 
    GlDraw.begins `line_strip;
    List.iter (fun (x,y) -> GlDraw.vertex2 (float x, float y)) pts;
    GlDraw.ends ()

  method points pts =
    GlDraw.begins `points;
    List.iter (fun (x,y) -> GlDraw.vertex2 (float x, float y)) pts;
    GlDraw.ends ()

(*
  method point ~x ~y = self#points [x,y]
*)

  method polygon ?(filled=false) pts = 
    GlDraw.begins (if filled then `polygon else `line_loop);
    List.iter (fun (x,y) -> GlDraw.vertex2 (float x, float y)) pts;
    GlDraw.ends ()

    method put_image ~x ~y img =
      GlPix.raster_pos ~x:(float x) ~y:(float y) ();
      GlPix.draw (img : ([`rgba], [`ubyte]) GlPix.t)
 
    method rectangle ~x ~y ~width ~height ?(filled=false) () =
      if filled then begin
	let x1 = float x 
	and y1 = float y
	and x2 = float (x + width - 1) 
	and y2 = float (y + height - 1)
	in
	GlDraw.begins `quads;
	GlDraw.vertex2 (x1, y1);
	GlDraw.vertex2 (x2, y1);
	GlDraw.vertex2 (x2, y2);
	GlDraw.vertex2 (x1, y2);
	GlDraw.ends ()
      end else
	GlDraw.rect (float x, float y) (float (x+width-1), float (y+height-1))

(*
    method segments segs = 
      GlDraw.begins `lines;
      List.iter (fun self#gr_op (fun v -> v#segments segs)
      GlDraw.ends ()

    method string s ~font ~x ~y = 
      self#gr_op (fun v -> v#string s ~font ~x ~y)
*)

    val mutable foreground = (0.0, 0.0, 0.0)
    method foreground = foreground	
    method set_foreground c = 
      foreground <- c;
      GlDraw.color ~alpha:1.0 c

    method image ~x ~y (spec : GrImage.spec) = 
      prerr_endline (Printf.sprintf "#image %d+%d" x y)

    method push = GlMisc.push_attrib 
    method pop = GlMisc.pop_attrib

  val mutable stored_texture = None

    method glyph ~color ~x ~y glyph =
      let color = GrMisc.Colour.gl_of_dvi color in
      let graymap = glyph.GlGlyph.graymap in

      self#push [`color_buffer];

      GlMat.push ();
      GlMat.translate ~x:(float x) ~y:(float (height-y)) ();
      
      let x = -glyph.GlGlyph.hoffset in
      let y = glyph.GlGlyph.voffset - glyph.GlGlyph.height in
      GlMat.translate ~x:(float x) ~y:(float y) ();

(*
      GlMat.scale ~x: 3.0 ~y:3.0 ();
*)

(*
      Gl.disable `alpha_test;
      Gl.disable `blend;
      Gl.disable `texture_2d;
      self#set_foreground (0.5, 1.0, 0.5);
      GlDraw.begins `quads;
      GlDraw.vertex2 (0.0, 0.0);
      GlDraw.vertex2 (float glyph.GlGlyph.width, 0.0);
      GlDraw.vertex2 (float glyph.GlGlyph.width, float glyph.GlGlyph.height);
      GlDraw.vertex2 (0.0, float glyph.GlGlyph.height);
      GlDraw.ends ();
*)

      Gl.enable `alpha_test;
      GlFunc.alpha_func `gequal 0.02;
      Gl.enable `blend;
      GlFunc.blend_func ~src:`src_alpha ~dst:`one_minus_src_alpha;
      Gl.enable `texture_2d;
      GlTex.parameter ~target: `texture_2d (`min_filter `linear);
      GlTex.parameter ~target: `texture_2d (`mag_filter `linear);

      begin match stored_texture with
      |	Some t when t == graymap.GlFont.texture -> ()
      |	_ ->
	  prerr_endline "upload texture";
	  stored_texture <- Some graymap.GlFont.texture;
(*
	  GlTex.image2d ~internal: 0x8043 graymap.GlFont.texture;
*)
	  begin try
	    GluMisc.build_2d_mipmaps ~internal: 0x8043 graymap.GlFont.texture;
	  with
	  | e -> prerr_endline (Printf.sprintf "upload failed (%s)"
				  (Printexc.to_string e));
	      raise e
	  end;
	  prerr_endline "upload texture done";
      end;

      Gl.enable `blend;
      GlFunc.blend_func ~src:`src_alpha ~dst:`one_minus_src_alpha;

      self#set_foreground color;
      GlDraw.begins `quads;
      GlTex.coord2 (graymap.GlFont.x1, graymap.GlFont.y1);
      GlDraw.vertex2 (0.0, 0.0);

      GlTex.coord2 (graymap.GlFont.x2, graymap.GlFont.y1);
      GlDraw.vertex2 (float glyph.GlGlyph.width, 0.0);

      GlTex.coord2 (graymap.GlFont.x2, graymap.GlFont.y2);
      GlDraw.vertex2 (float glyph.GlGlyph.width, float glyph.GlGlyph.height);

      GlTex.coord2 (graymap.GlFont.x1, graymap.GlFont.y2);
      GlDraw.vertex2 (0.0, float glyph.GlGlyph.height);
      GlDraw.ends ();
      GlMat.pop ();

      Gl.disable `blend;
      Gl.disable `texture_2d;

      self#pop ();

  method initialize () =
    prerr_endline "glarea initialize";
    GlClear.color ~alpha:0.0 (1.0, 1.0, 1.0);
    GlMat.mode `modelview;
    GlMat.load_identity ();
(*
    GluMat.look_at ~eye: (0.0, 0.0, 100.0)
      ~center: (0.0, 0.0, 0.0)
      ~up: (0.0, 1.0, 0.0)
*)

  method reshape ~width:w ~height:h = 
    prerr_endline (Printf.sprintf "glarea reshape %dx%d" w h);
    width <- w;
    height <-  h;
    GlDraw.viewport ~x:0 ~y:0 ~w:w ~h:h;
    GlMat.mode `projection;
    GlMat.load_identity ();
(*
    GluMat.perspective ~fovy: 45.0 ~aspect: 1.0 ~z: (1.0, 1000.0); 
*)
    GluMat.ortho2d ~x:(0.0, float w) ~y:(0.0, float h);
    GlMat.mode `modelview

  initializer
    ignore (glarea#connect#realize ~callback: self#initialize);
    ignore (glarea#connect#reshape ~callback: self#reshape)

  end
;;
