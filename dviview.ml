(*
 * advi - A DVI previewer
 * Copyright (C) 2000  Alexandre Miquel
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
 *
 * See the GNU Lesser General Public License version 2.1 for more
 * details (enclosed in the file LGPL).
 *)


let pauses = Misc.option_flag true "-nopauses" "Switch pauses off";;
let fullwidth = Misc.option_flag false "-fullwidth" "Adjust size to width";;
let bounding_box = Misc.option_flag false "-bbox" "Show the bounding box";;

let start_page = ref 0
let _ = Misc.set_option
    "-page"
    (Arg.Int (fun i -> start_page := i))
    "INT\tMake advi start at page INT";;
let start_html = ref None
let _ = Misc.set_option
    "-html"
    (Arg.String (fun s -> start_html := Some s))
    "STRING\tMake advi start at html reference of name STRING";;
let debug_pages = Misc.debug_option
    "--debug_pages"
    "Debug page motion"
let page_stack_to_string page stack =
  let stack = String.concat " " (List.map string_of_int stack) in
  Printf.sprintf "Page no: %d Page_stack: %s"
    page
    stack

let scale_step = ref (sqrt (sqrt 2.));;
let set_scale x =
  if x > 1.0 && x <= 2. then scale_step := x
  else
    Misc.warning
      (Printf.sprintf "out of bounds scale_step %f ignored" x);;
let _ = 
  Misc.set_option
    "-scalestep"
    (Arg.Float set_scale)
    "REAL\tScale step for '<' and '>' (default sqrt(sqrt(2.0)))"
;;

let autoresize = ref true;;
let _ = Misc.set_option
    "-noautoresize"
    (Arg.Clear autoresize)
    "\tPrevents scaling from resizing the window (done if geometry is provided)"
;;

(****************)
(*  Signatures  *)
(****************)

module type DEVICE = sig
  type glyph

  val make_glyph : Glyph.t -> glyph
  val get_glyph  : glyph -> Glyph.t

  val open_dev : string -> unit
  val close_dev : unit -> unit
  val clear_dev : unit -> unit
  val set_bbox : (int * int * int * int) option -> unit

  type color = int

  val set_color : int -> unit
  val draw_glyph : glyph -> int -> int -> unit
  val fill_rect : int -> int -> int -> int -> unit

  val draw_path: (int * int) array -> pensize:int -> unit
  val fill_path: (int * int) array -> shade:float -> unit
  val draw_arc: x:int -> y:int -> rx:int -> ry:int -> 
                start:int -> stop:int -> pensize:int -> unit
  val fill_arc: x:int -> y:int -> rx:int -> ry:int -> 
                start:int -> stop:int -> shade:float -> unit

  val set_epstransparent : bool -> unit
  val set_alpha : float -> unit
  type blend =
    | Normal | Multiply | Screen | Overlay (* | SoftLight | HardLight *)
    | ColorDodge | ColorBurn | Darken | Lighten | Difference 
    | Exclusion (* | Luminosity | Color | Saturation | Hue *)
  val set_blend : blend -> unit
  val draw_ps : string -> (int * int * int * int) -> (int * int) -> int -> int -> unit
  val clean_ps_cache : unit -> unit
  val sleep : float -> unit

  type app_type = Sticky | Persistent | Embedded
  val embed_app : string -> app_type -> int -> int -> int -> int -> unit
  val kill_embedded_apps : unit -> unit 

  type event =
      Resized of int * int
    | Refreshed
    | Key of char
    | Region of int * int * int * int
    | Href of string
    | Advi of string * (unit -> unit)

  val wait_event : unit -> event

  module H :
      sig 
        type tag =
          | Name of string 
          | Href of string
          | Advi of string * (unit -> unit)

        type anchor = {
            tag : tag;
            draw : (int * int * glyph) list
          } 

        val add : anchor -> unit
        val flashlight : tag -> unit
      end

  exception Stop
  exception GS
  val continue : unit -> unit
  val current_pos : unit -> int * int
  val newpage : string list -> int -> float -> int -> int -> unit
  val exec_ps : string -> int -> int -> unit
  val add_headers : string list -> unit
  val synchronize : unit -> unit

  type busy = Free | Busy | Pause | Disk
  val set_busy : busy -> unit;;
  val set_title : string -> unit
end ;;

module type DVIVIEW = sig
  open Dimension
  val set_autoresize : bool -> unit
  val set_geometry : string -> unit
  val set_crop : bool -> unit
  val set_hmargin : dimen -> unit
  val set_vmargin : dimen -> unit
  exception Error of string

  val main_loop : string -> unit
end ;;

module Make(Dev : DEVICE) = struct
  
  module Drv = Driver.Make(Dev)
      
  open Dimension
    
  exception Error of string
      
  (*** View attributes ***)
      
  (* things we can set before initialization *)
      
  type offset =
    | No_offset
    | Plus of int
    | Minus of int
          
  let int_of_offset = function 
    | No_offset -> 0
    | Plus n -> n
    | Minus n -> -n 
          
  type geometry = {
      mutable width : int ;
      mutable height : int ;
      mutable xoffset : offset ;
      mutable yoffset : offset
    }
        
  type attr = {
      mutable geom : geometry ;
      mutable crop : bool ;
      mutable hmargin : dimen ;
      mutable vmargin : dimen
    }
        
  let string_of_geometry geom =
    let w = geom.width
    and h = geom.height
    and xoff = geom.xoffset
    and yoff = geom.yoffset in
    match (xoff, yoff) with
    | (No_offset, No_offset) -> Printf.sprintf "%dx%d" w h
    | (Plus x, No_offset) -> Printf.sprintf "%dx%d+%d" w h x
    | (Minus x, No_offset) -> Printf.sprintf "%dx%d-%d" w h x
    | (No_offset, Plus y) -> Printf.sprintf "%dx%d++%d" w h y
    | (No_offset, Minus y) -> Printf.sprintf "%dx%d+-%d" w h y
    | (Plus x, Plus y) -> Printf.sprintf "%dx%d+%d+%d" w h x y
    | (Plus x, Minus y) -> Printf.sprintf "%dx%d+%d-%d" w h x y
    | (Minus x, Plus y) -> Printf.sprintf "%dx%d-%d+%d" w h x y
    | (Minus x, Minus y) -> Printf.sprintf "%dx%d-%d-%d" w h x y
          
  (*** The view state ***)
          
  type state = {
      (* DVI attributes *)
      filename : string ;
      mutable dvi : Dvi.t ;
      mutable cdvi : Drv.cooked_dvi ;
      mutable num_pages : int ;
      (* Page layout *)
      mutable base_dpi : float ;
      mutable dvi_width : int ;   (* in pixels *)
      mutable dvi_height : int ;  (* in pixels *)
      (* Window size *)
      mutable size_x : int ;
      mutable size_y : int ;
      (* Current parameters *)
      mutable orig_x : int ;
      mutable orig_y : int ;
      mutable ratio : float ;
      mutable page_no : int;
      mutable page_stack : int list;
      mutable last_modified : float ;
      mutable button : (int * int) option ;
      
      mutable pause_no : int;
      (* Attributes for Embedded postscript *)
      mutable aborted : bool;
      mutable frozen : bool;
      (* If the page was not completed *)
    }
        
  let state = ref None
      
  (*** Setting the geometry ***)
      
  let is_digit c = (c >= '0' && c <= '9')
      
  let parse_geometry str =
    try
    let len = String.length str
    and i = ref 0 in
    let parse_int () =
      if !i = len || not (is_digit str.[!i] || str.[!i] == '-') then
	invalid_arg "set_geometry" ;
      let start = !i in
      if str.[!i] = '-' && !i < len+1 then incr i;
      while !i < len && is_digit str.[!i] do incr i done ;
      let stop = !i in
      int_of_string (String.sub str start (stop - start)) in
    let parse_offset () =
      if !i = len || (str.[!i] <> '+' && str.[!i] <> '-') then
	No_offset
      else begin
	let sgn = str.[!i] in
	incr i ;
	if !i = len || not (is_digit str.[!i] || str.[!i] == '-') then
	  No_offset
	else
	  match sgn with
	  | '+' -> Plus (parse_int ())
	  | '-' -> Minus (parse_int ())
	  | _ -> assert false
      end in
    while !i < len && str.[!i] = ' ' do incr i done ;
    let width = parse_int () in
    if !i = len || (str.[!i] <> 'x' && str.[!i] <> 'X') then
      invalid_arg "set_geometry" ;
    incr i ;
    let height = parse_int () in
    let xoffset = parse_offset () in
    let yoffset = parse_offset () in
    { width = width; height = height; xoffset = xoffset; yoffset = yoffset }
    with Failure _ -> invalid_arg "parse_geometry"
      
  (*** Setting other parameters ***)
        
  let attr =
    { geom = 
      { width = 0;
        height = 0;
        xoffset = No_offset;
        yoffset = No_offset;
      };
      crop = false ;
      hmargin = Px 0 ;
      vmargin = Px 0
    }

  let set_autoresize b = autoresize := b
  let set_geometry geom =
    attr.geom <- parse_geometry geom

  let set_crop b =
    attr.crop <- b
        
  let set_hmargin d =
    attr.hmargin <- normalize d
        
  let set_vmargin d =
    attr.vmargin <- normalize d

      

        
  (*** Initialization ***)
        
  let init filename =
    let dvi =
      try Dvi.load filename
      with
      |	Sys_error _ -> raise (Error ("cannot open `" ^ filename ^ "'"))
      |	Dvi.Error s -> raise (Error (filename ^ ": " ^ s))
      |	_ -> raise (Error ("error while loading `" ^ filename ^ "'")) in
    let cdvi = Drv.cook_dvi dvi
    and dvi_res = 72.27
    and mag = float dvi.Dvi.preamble.Dvi.pre_mag /. 1000.0 in
    let w_sp = dvi.Dvi.postamble.Dvi.post_width
    and h_sp = dvi.Dvi.postamble.Dvi.post_height in
    let w_in = mag *. ldexp (float w_sp /. dvi_res) (-16)
    and h_in = mag *. ldexp (float h_sp /. dvi_res) (-16) in
    let wdpi =
      match attr.hmargin with
      | Px n -> float (attr.geom.width - 2 * n) /. w_in
      | In f -> float attr.geom.width /. (w_in +. 2.0 *. f)
      | _ -> assert false
    and hdpi =
      match attr.vmargin with
      | Px n -> float (attr.geom.height - 2 * n) /. h_in
      | In f -> float attr.geom.height /. (h_in +. 2.0 *. f)
      | _ -> assert false in
    let base_dpi = min wdpi hdpi in
    let width = int_of_float (base_dpi *. w_in)
    and height = int_of_float (base_dpi *. h_in) in
    let (size_x, size_y) =
      if attr.crop then begin
	let sx = match attr.hmargin with
	| Px n -> width + 2 * n
	| In f -> width + int_of_float (base_dpi *. 2.0 *. f)
	| _ -> assert false
	and sy = match attr.vmargin with
	| Px n -> height + 2 * n
	| In f -> height + int_of_float (base_dpi *. 2.0 *. f)
	| _ -> assert false in
	(min attr.geom.width sx, min attr.geom.height sy)
      end else
	(attr.geom.width, attr.geom.height) in
    attr.geom.width <- size_x ;
    attr.geom.height <- size_y ;
    let orig_x = (size_x - width)/2
    and orig_y = (size_y - height)/2 in
    let last_modified =
      try (Unix.stat filename).Unix.st_mtime
      with _ -> 0.0 in
    let pages = Array.length dvi.Dvi.pages in
    Misc.dops := !Misc.pson;
    { filename = filename ;
      dvi = dvi ;
      cdvi = cdvi ;
      num_pages = pages ;
      base_dpi = base_dpi ;
      dvi_width = width ;
      dvi_height = height ;
      size_x = size_x ;
      size_y = size_y ;
      orig_x = orig_x ;
      orig_y = orig_y ;
      ratio = 1.0 ;
      page_stack = [];
      page_no = if !start_page > 0 then min !start_page pages -1 else 0;
      last_modified = last_modified ;
      button = None;
      
      pause_no = 0;
      frozen = true;
      aborted = false;
    }
      
  let update_dvi_size init st =
    let dvi_res = 72.27
    and mag = float st.dvi.Dvi.preamble.Dvi.pre_mag /. 1000.0
    and w_sp = st.dvi.Dvi.postamble.Dvi.post_width
    and h_sp = st.dvi.Dvi.postamble.Dvi.post_height in
    let w_in = mag *. ldexp (float w_sp /. dvi_res) (-16)
    and h_in = mag *. ldexp (float h_sp /. dvi_res) (-16) in
    
    if init then
      begin
        let wdpi =
          match attr.hmargin with
          | Px n -> float (attr.geom.width - 2 * n) /. w_in
          | In f -> float attr.geom.width /. (w_in +. 2.0 *. f)
          | _ -> assert false
        and hdpi =
          match attr.vmargin with
          | Px n -> float (attr.geom.height - 2 * n) /. h_in
          | In f -> float attr.geom.height /. (h_in +. 2.0 *. f)
          | _ -> assert false in
        let base_dpi = (if !fullwidth then wdpi else min wdpi hdpi) in
        let width = int_of_float (base_dpi *. w_in)
        and height = int_of_float (base_dpi *. h_in) in
        let (size_x, size_y) =
          if attr.crop then begin
	    let sx = match attr.hmargin with
	    | Px n -> width + 2 * n
	    | In f -> width + int_of_float (base_dpi *. 2.0 *. f)
	    | _ -> assert false
	    and sy = match attr.vmargin with
	    | Px n -> height + 2 * n
	    | In f -> height + int_of_float (base_dpi *. 2.0 *. f)
	    | _ -> assert false in
	    (min attr.geom.width sx, min attr.geom.height sy)
          end else
	    (attr.geom.width, attr.geom.height) in
        attr.geom.width <- size_x ;
        attr.geom.height <- size_y ;
        st.base_dpi <- base_dpi;
        st.size_x <- size_x;
        st.size_y <- size_y;
        let orig_x, orig_y = (size_x - width)/2,  (size_y - height)/2 in
        st.orig_x <- orig_x; 
        st.orig_y <- orig_y; 
      end;
    st.dvi_width <- int_of_float (st.base_dpi *. w_in *. st.ratio);
    st.dvi_height <- int_of_float (st.base_dpi *. h_in *. st.ratio)
    
      
  let goto_next_pause st f =
    try 
      let cont =
        try 
	  while f () do () done; 
	  st.pause_no <- st.pause_no + 1; 
	  None
        with Drv.Pause -> 
	  st.pause_no <- st.pause_no + 1; 
	  Some f 
      in
      Dev.set_busy (if cont = None then Dev.Free else Dev.Pause);
      Dev.synchronize();
      cont
    with Dev.Stop ->
      st.aborted <- true;
      None
        
  let draw_bounding_box st = 
    Dev.set_color 0xcccccc ;
    Dev.fill_rect st.orig_x st.orig_y st.dvi_width 1 ;
    Dev.fill_rect st.orig_x st.orig_y 1 st.dvi_height ;
    Dev.fill_rect st.orig_x (st.orig_y + st.dvi_height) st.dvi_width 1 ;
    Dev.fill_rect (st.orig_x + st.dvi_width) st.orig_y 1 st.dvi_height 

  (* Input : a point in window coordinates, relative to lower-left corner. *)
  (* Output : a point in document coordinates, relative to upper-right corner. *)
  (* The output depends on the ratio st.ratio. *)
  let document_xy st x y =
    (* x and y are relative to lower-left corner, *)
    (* but orig_x and orig_y are relative to upper-right corner !!! *)
    (* So we change y to comply to orig_y coordinates. *)
    let y = st.size_y - y in
    let docx = int_of_float (float (x - st.orig_x))
    and docy = int_of_float (float (y - st.orig_y))
    in docx, docy

  (* User has selected a region with the mouse. We dump characters. *)
  let selection st x y dx dy =
    let docx , docy  = document_xy st x y
    and docx2, docy2 = document_xy st (x+dx) (y+dy) in
    (* Printf.printf "Zone de %d, %d à %d, %d" docx docy docx2 docy2 ; *)
    let symbols = Drv.give_symbols() in
    let zone = (docx, docy, docx2, docy2) in
    let output = Symbol.to_ascii zone symbols in
    print_string output;
    print_newline ();
    flush stdout;
    ()
      
  let redraw st =
    (* draws until the current pause_no or page end *)
    Dev.set_busy Dev.Busy;
    let cont = 
      try
        Dev.continue(); 
        Dev.clear_dev () ;
	(* Size of a blank ? *)
	let blank_w = 20
	and blank_h = 20 in
	Drv.clear_symbols st.size_x st.size_y blank_w blank_h ;
        let cont = 
          if !pauses then
            let f = Drv.render_step st.cdvi st.page_no
	        (st.base_dpi *. st.ratio) st.orig_x st.orig_y in
            let current_pause = ref 0 in
            begin
              try
                while 
	          try f () with
      	          | Drv.Pause ->
	              if !current_pause = st.pause_no then raise Drv.Pause
	              else begin incr current_pause; true end
(*
   | e -> prerr_endline (Printexc.to_string e); false
*)
                do () done;
                None
              with
              | Drv.Pause ->
                  Some f
            end
          else
            begin
              Drv.render_page  st.cdvi st.page_no
                (st.base_dpi *. st.ratio) st.orig_x st.orig_y;
              None
            end in
        st.aborted <- false; 
        cont
      with Dev.Stop ->
        st.aborted <- true;
        None
    in
    if (!bounding_box) then draw_bounding_box st;
    Dev.set_busy (if cont = None then Dev.Free else Dev.Pause);
    Dev.synchronize();
    cont
      
  let center st =
    st.ratio <- 1.0 ;
    update_dvi_size false st ;
    st.orig_x <- (st.size_x - st.dvi_width)/2 ;
    st.orig_y <- (st.size_y - st.dvi_height)/2 ;
    redraw st
      
      
  let find_xref st tag default =
      try Hashtbl.find st.dvi.Dvi.xrefs tag
      with Not_found ->
        if st.frozen then
          begin
            Drv.scan_specials st.cdvi max_int;
            st.frozen <- false;
            try Hashtbl.find st.dvi.Dvi.xrefs tag
            with Not_found ->
              default
          end
        else default

  exception Link
  let exec_xref link = 
      (* -- check that files exist; use mine and mailcap; etc. *)
      if Misc.has_prefix "file:" link then
        try
          let filename, arguments = 
            match Misc.split_string (Misc.get_suffix "file:" link) '#' 0 with
            | [ filename ; tag ] -> filename, ["-html"; tag ]
            | [ filename ] -> filename, []
            | _ ->
                Misc.warning ("Invalid link "^link);
                raise Link
          in
          let call command =
            Dev.embed_app command Dev.Sticky
              attr.geom.width
              attr.geom.height
              (int_of_offset attr.geom.xoffset)
              (int_of_offset attr.geom.yoffset); 
            if Graphics.button_down() then
              ignore (Graphics.wait_next_event [ Graphics.Button_up ]) in
          if Sys.file_exists filename then
            begin
              if Misc.has_suffix ".dvi"  filename then
              call  (String.concat " " ("advi" :: arguments @ [ filename ]))
              else if (Misc.has_suffix ".txt"  filename
                    || Misc.has_suffix ".tex"  filename) then
                call ("xterm -e more "^ filename)
              else
                Misc.warning 
                  (Printf.sprintf
                     "Don't know what to do with file %s"
                     filename);
            end
          else
            Misc.warning
              (Printf.sprintf "File %s non-existent or not readable"
                 filename)
        with
          Misc.Match -> assert false
        | Link ->
            ()
          
  let page_start st default =   
    match !start_html with None -> default
    | Some html ->
        Drv.scan_specials st.cdvi max_int;
        find_xref st html default
          
  let rec clear_page_stack max stack =
    let pages = Array.create max false in
    let rec clear = function
        p::stack ->
          let s = clear stack in
          let pa = if p < 0 then -1 - p else p in
          if pa < max && not pages.(pa) then
            begin
              pages.(pa) <- true;
              p :: s
            end
          else
            s
      | _ -> [] in
    clear stack

  let reload_time st =
      try let s = Unix.stat st.filename in s.Unix.st_mtime
      with _ -> st.last_modified
      
  let reload st =
    try
      st.last_modified <- reload_time st;
      let dvi = Dvi.load st.filename in
      let cdvi = Drv.cook_dvi dvi in
      let dvi_res = 72.27
      and mag = float dvi.Dvi.preamble.Dvi.pre_mag /. 1000.0 in
      let w_sp = dvi.Dvi.postamble.Dvi.post_width
      and h_sp = dvi.Dvi.postamble.Dvi.post_height in
      let w_in = mag *. ldexp (float w_sp /. dvi_res) (-16)
      and h_in = mag *. ldexp (float h_sp /. dvi_res) (-16) in
      let width = int_of_float (w_in *. st.base_dpi *. st.ratio)
      and height = int_of_float (h_in *. st.base_dpi *. st.ratio) in
      let pages =  Array.length dvi.Dvi.pages in
      st.dvi <- dvi ;
      st.cdvi <- cdvi ;
      st.num_pages <- pages ;
      st.page_stack <- clear_page_stack pages st.page_stack; 
      st.page_no <- page_start st (min st.page_no (st.num_pages - 1));
      st.pause_no <- 0 ;
      st.frozen <- true;
      st.aborted <- true;
      update_dvi_size false st ;
      Misc.dops := !Misc.pson;
      redraw st
    with x ->
      assert (Misc.debug (Printexc.to_string x));      
      None
        
  let changed st = reload_time st > st.last_modified
        
  let goto_page st cont n = (* go to the begining of the page *) 
    let new_page_no = max 0 (min n (st.num_pages - 1)) in
    if st.page_no <> new_page_no || st.aborted then
      begin
        st.page_no <- new_page_no ;
        st.pause_no <- 0 ;
        redraw st
      end
    else cont (* return the current cont: do nothing *)

  let push_stack b st n =
    match st.page_stack with
    | p :: rest when p = n -> if b then st.page_stack <- ( -1 - n )::rest
    | p :: rest when p = -1 - n -> ()
    | all -> st.page_stack <- (if b then -1 - n else n) :: all
      
  let push_page b st cont n =
    let new_page_no = max 0 (min n (st.num_pages - 1)) in
    if st.page_no <> new_page_no || st.aborted then
      begin
        push_stack b st st.page_no;
        goto_page st cont n
      end
    else cont
        
  let pop_page b st cont n =
    assert (debug_pages
              (Printf.sprintf "%s\n => popping %s page %d "
                 (page_stack_to_string st.page_no st.page_stack)
                 (if b then "true" else "false")
                 n));    
    let rec pop n return_page return_stack stack =
      match n, stack with
      | n, _ when n <= 0 ->
          return_page, return_stack
      | _, [] ->
          return_page, return_stack
      | n, h::t ->
          if b && h >= 0
          then pop n return_page return_stack t
          else pop (pred n) h t t in
    let page, stack = pop n st.page_no st.page_stack st.page_stack in
    st.page_stack <- stack;
    goto_page st cont (if page > 0 then page else -1 - page)
      
  let goto_href st cont link = (* goto page of hyperref h *)
    let page = 
      if Misc.has_prefix "#" link then
        let tag = Misc.get_suffix "#" link in
        find_xref st tag st.page_no
      else 
        begin
          exec_xref link;
          st.page_no
        end in
    let cont = push_page true st cont page in
    Dev.H.flashlight (Dev.H.Name link);
    cont
      
  let goto_next_page st cont = (* go to the begining of the page *)
    if st.page_no = st.num_pages - 1 then
      cont (* return the current cont. *)
    else goto_page st cont (st.page_no + 1) 
        
  let resize st x y =
    attr.geom <-
      { width = x;
        height = y;
        xoffset = No_offset;
        yoffset = No_offset;
      };
    update_dvi_size true st;
    redraw st

  let scale_by st factor =
    if !autoresize then
      begin
        let scale x = int_of_float (float x *. factor) in
        attr.geom.width <- scale attr.geom.width;
        attr.geom.height <- scale attr.geom.height;
        Dev.close_dev();
        Dev.open_dev (Printf.sprintf " " ^ string_of_geometry attr.geom);
      end
    else
      begin
        let new_ratio = factor *. st.ratio in
        if new_ratio >= 0.1 && new_ratio < 10.0 then
          begin
            st.ratio <- new_ratio ;
            let (cx, cy) = (st.size_x/2, st.size_y/2) in
            st.orig_x <- int_of_float (float (st.orig_x - cx) *. factor) + cx ;
            st.orig_y <- int_of_float (float (st.orig_y - cy) *. factor) + cy ;
          end;
      end;
    update_dvi_size true st;
    redraw st
      

  let nomargin st =
    attr.hmargin <- Px 0; attr.vmargin <- Px 0; 
    update_dvi_size true st;
    redraw st
      
  let main_loop filename =
    let st = init filename in
    let cont = ref None in (* drawing continuation *)
    Dev.open_dev (Printf.sprintf " " ^ string_of_geometry attr.geom) ;
    Dev.set_title ("Advi: " ^ Filename.basename filename);
    if st.page_no > 0 && !Misc.dops then
      Drv.scan_specials st.cdvi st.page_no
    else
      st.page_no <- page_start st 0;
    cont := redraw st ;

    (* num is the the current number entered by keyboard *)
    let num = ref 0 in

    (* selection_mode says if a mouse click should move the window or select text. *)
    let selection_mode = ref false in

    try while true do
      let ev = if changed st then Dev.Refreshed else Dev.wait_event() in
      let num_val = !num in
      match ev with
      | Dev.Refreshed -> 
          cont := reload st
      | Dev.Resized (x,y) -> 
          cont := resize st x y
      | Dev.Key ' ' -> 
          begin match !cont with
          | None -> 
              (* If you want to go to the next page by space,
                 use () instead of the following instead  *)
              num := 0; 
              cont := push_page false st !cont (st.page_no + max 1 num_val)
          | Some f -> cont := goto_next_pause st f
          end
      | Dev.Key ('0'..'9' as c) ->
          num := !num * 10 + Char.code c - Char.code '0'
      | Dev.Key 'n' ->
          num := 0;
          cont := goto_page st !cont (st.page_no + max 1 num_val) ; 
      | Dev.Key '\r' ->
          num := 0;
          cont := push_page false st !cont (st.page_no + max 1 num_val) 
      | Dev.Key '\t' -> 
          push_stack true st st.page_no
      | Dev.Key 'p' ->
          num := 0; 
          cont := goto_page st !cont (st.page_no - max 1 num_val) 
      | Dev.Key '\b' ->
          num := 0;
          cont := pop_page false st !cont (max 1 num_val) 
      | Dev.Key '\027' ->
          num := 0; 
          cont := pop_page true st !cont 1; 
      | Dev.Key ',' -> cont := goto_page st !cont 0;
      | Dev.Key '.' -> cont := goto_page st !cont max_int;
      | Dev.Key 'f' -> Drv.unfreeze_fonts st.cdvi
      | Dev.Key 'F' -> Drv.unfreeze_glyphs st.cdvi (st.base_dpi *. st.ratio)
      | Dev.Key 'r' -> cont := redraw st
      | Dev.Key 'R' -> cont := reload st
      |	Dev.Key 's' -> selection_mode := not !selection_mode
      | Dev.Key 'c' -> cont := center st
      | Dev.Key '<' -> cont := scale_by st (1.0 /. !scale_step)
      | Dev.Key '>' -> cont := scale_by st !scale_step
      | Dev.Key 'g' ->
          num := 0;
          cont :=
            push_page true st !cont
              (if num_val > 0 then num_val - 1 else st.num_pages)
      | Dev.Key '#' ->
          num := 0;
          cont := nomargin st
          
      | Dev.Key 'q' -> raise Exit
      | Dev.Key 'C' -> (* clear image cache *)
          Dev.clean_ps_cache ()
      | Dev.Key '?' ->
          let int = function
              No_offset -> 0
            | Plus x -> x
            | Minus y -> 0 - y in
          Dev.embed_app ("advi "^ Config.splash_screen) Dev.Embedded
            attr.geom.width
            attr.geom.height
            (int_of_offset attr.geom.xoffset)
            (int_of_offset attr.geom.yoffset)
      | Dev.Key _ -> ()
      | Dev.Href h ->
          cont := goto_href st !cont h
      | Dev.Advi (s,a) -> a();
      | Dev.Region (x, y, w, h) ->

	  if !selection_mode then selection st x y w (-h)
	  else
	    begin
              st.orig_x <- st.orig_x + w ;
              st.orig_y <- st.orig_y + h ; 
              cont := redraw st
	    end
	      
    done with Exit -> Dev.close_dev ()
        
end ;;
