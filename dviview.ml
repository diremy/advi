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

type select_mode = Interval | Rectangle;;

let pauses = Misc.option_flag true "-nopauses" "Switch pauses off";;
let fullwidth = Misc.option_flag false "-fullwidth" "Adjust size to width";;
let bounding_box = Misc.option_flag false "-bbox" "Show the bounding box";;

let start_page = ref 0;;
Misc.set_option
  "-page"
  (Arg.Int (fun i -> start_page := i))
  "INT\tMake advi start at page INT";;

let start_html = ref None;;
Misc.set_option
  "-html"
  (Arg.String (fun s -> start_html := Some s))
  "STRING\tMake advi start at html reference of name STRING";;
let debug_pages =
  Misc.debug_option
    "--debug_pages"
    "Debug page motion";;

let browser = ref "netscape-communicator";;
Misc.set_option
  "-browser"
  (Arg.String (fun s -> browser := s))
  "STRING\tCommand to call the browser";;

let click_turn_page =
  Misc.option_flag false
    "-click_turn"
    "Turn pages with mouse clicks (see the doc)";;

let page_stack_to_string page stack =
  let stack = String.concat " " (List.map string_of_int stack) in
  Printf.sprintf "Page no: %d Page_stack: %s"
    page
    stack;;

let scale_step = ref (sqrt (sqrt 2.));;
let set_scale x =
  if x > 1.0 && x <= 2. then scale_step := x
  else
    Misc.warning
      (Printf.sprintf "out of bounds scale_step %f ignored" x);;
Misc.set_option
  "-scalestep"
  (Arg.Float set_scale)
  "REAL\tScale step for '<' and '>' (default sqrt(sqrt(2.0)))";;

let autoresize = ref true;;
Misc.set_option
  "-noautoresize"
  (Arg.Clear autoresize)
  "\tPrevents scaling from resizing the window (done if geometry is provided)"
;;

module Symbol = Grdev.Symbol;;

open Dimension;;

exception Error of string;;

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
type mode = Selection | Control;;
type state = {
      (* DVI attributes *)
    filename : string ;
    mutable dvi : Dvi.t ;
    mutable cdvi : Driver.cooked_dvi ;
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
    mutable exchange_page : int;
    mutable last_modified : float ;
    mutable button : (int * int) option ;
    mutable fullscreen : (int * int * int * int) option; 

    mutable pause_no : int;
      (* Attributes for Embedded postscript *)

      (* true when page was not completed: may need to redraw *)
    mutable aborted : bool;
      (* true when hrefs have not been processed *)
    mutable frozen : bool;
      (* numeric value for keyboard interaction *)
    mutable num : int;
      (* next numeric value *)
    mutable next_num : int;
      (* control the action of the mouse *)
    mutable mode : mode;
      (* Some of f when on a pause *)
    mutable cont : (unit -> bool) option;
  };;

(*** Setting the geometry ***)
let is_digit c = c >= '0' && c <= '9';;

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
    with Failure _ -> invalid_arg "parse_geometry";;

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
  };;

let set_autoresize b = autoresize := b
let set_geometry geom = attr.geom <- parse_geometry geom

let set_crop b = attr.crop <- b

let set_hmargin d = attr.hmargin <- normalize d

let set_vmargin d = attr.vmargin <- normalize d

(*** Initialization ***)
let init filename =
  let dvi =
    try Dvi.load filename
    with
    | Sys_error _ -> raise (Error ("cannot open `" ^ filename ^ "'"))
    | Dvi.Error s -> raise (Error (filename ^ ": " ^ s))
    | _ -> raise (Error ("error while loading `" ^ filename ^ "'")) in
  let cdvi = Driver.cook_dvi dvi
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
  let st =
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
      exchange_page = 0;
      page_no = if !start_page > 0 then min !start_page pages - 1 else 0;
      last_modified = last_modified ;
      button = None;
      fullscreen = None;

      pause_no = 0;
      frozen = true;
      aborted = false;
      cont = None;
      mode = Control;
      num = 0;
      next_num = 0;
    } in
  st;;

let set_bbox st =
  Grdev.set_bbox (Some (st.orig_x, st.orig_y, st.dvi_width, st.dvi_height));;

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
      let base_dpi = if !fullwidth then wdpi else min wdpi hdpi in
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
          (sx, sy)
        end else
          (attr.geom.width, attr.geom.height) in
(*
   attr.geom.width <- size_x ;
   attr.geom.height <- size_y ;
*)
      st.base_dpi <- base_dpi;
      st.size_x <- size_x;
      st.size_y <- size_y;
      let orig_x, orig_y = (size_x - width)/2,  (size_y - height)/2 in
      st.orig_x <- orig_x;
      st.orig_y <- orig_y;
    end;
  st.dvi_width <- int_of_float (st.base_dpi *. w_in *. st.ratio);
  st.dvi_height <- int_of_float (st.base_dpi *. h_in *. st.ratio);
  set_bbox st;;

let rec goto_next_pause n st =
  if n > 0 then
    begin match st.cont with
    | None -> ()
    | Some f ->
        st.cont <- None;
        try
          let () =
            try
              while f () do () done;
              st.pause_no <- st.pause_no + 1;
            with Driver.Pause ->
              st.pause_no <- st.pause_no + 1;
              st.cont <- Some f in
          goto_next_pause (pred n) st
        with Grdev.Stop -> st.aborted <- true;
    end;
  Grdev.synchronize(); 
  Grdev.set_busy (if st.cont = None then Grdev.Free else Grdev.Pause);;

let draw_bounding_box st =
  Grdev.set_color 0xcccccc ;
  Grdev.fill_rect st.orig_x st.orig_y st.dvi_width 1 ;
  Grdev.fill_rect st.orig_x st.orig_y 1 st.dvi_height ;
  Grdev.fill_rect st.orig_x (st.orig_y + st.dvi_height) st.dvi_width 1 ;
  Grdev.fill_rect (st.orig_x + st.dvi_width) st.orig_y 1 st.dvi_height;;

(* Input : a point in window coordinates, relative to lower-left corner. *)
(* Output : a point in document coordinates, relative to upper-right corner. *)
(* The output depends on the ratio st.ratio. *)
let document_xy st x y =
  (* x and y are relative to lower-left corner, *)
  let y = st.size_y - y in
  x, y;;

let position st x y =
  match Symbol.lines x y with
  | Some (s, line, bound, before, after) ->
      if bound > 0 then
        let line = max 0 line in
        begin
          Printf.printf "#line %d, %d <<%s>><<%s>>"
            line bound before after;
          print_newline()
        end
  | None -> ();;

(* User has selected a region with the mouse. We dump characters. *)
let selection s = Grdev.cut s;;

let get_size_in_pix st = function
  | Px n -> n
  | In f -> int_of_float (st.base_dpi *. f)
  | _ -> assert false;;

(* The next four functions returns the position that correspond to the top,
   the bottom, the left and right of the page *)
let top_of_page st =
  let vmargin_size = get_size_in_pix st attr.vmargin in
  vmargin_size;;

let bottom_of_page st =
  let vmargin_size = get_size_in_pix st attr.vmargin in
  attr.geom.height - st.dvi_height - vmargin_size;;

let left_of_page st =
  let hmargin_size = get_size_in_pix st attr.hmargin in
  hmargin_size;;

let right_of_page st =
  let hmargin_size = get_size_in_pix st attr.hmargin in
  attr.geom.width - st.dvi_width - hmargin_size;;

(* the two following functions move the displayed part of the page while
   staying inside the margins *)
let move_within_margins_y st movey =
  let vmargin_size = get_size_in_pix st attr.vmargin in
  let tmp_orig_y = st.orig_y + movey in
  let new_orig_y =
    if movey < 0 then begin
      if tmp_orig_y + st.dvi_height + vmargin_size < attr.geom.height
      then attr.geom.height - st.dvi_height - vmargin_size
      else tmp_orig_y
    end else begin
      if tmp_orig_y - vmargin_size > 0
      then vmargin_size
      else tmp_orig_y
    end
  in
  if st.orig_y <> new_orig_y then Some new_orig_y
  else None;;

let move_within_margins_x st movex =
  let hmargin_size = get_size_in_pix st attr.hmargin in
  let tmp_orig_x = st.orig_x + movex in
  let new_orig_x =
    if movex < 0 then begin
      if tmp_orig_x + st.dvi_width + hmargin_size < attr.geom.width
      then attr.geom.width - st.dvi_width - hmargin_size
      else tmp_orig_x
    end else begin
      if tmp_orig_x - hmargin_size > 0
      then hmargin_size
      else tmp_orig_x
    end
  in
  if st.orig_x <> new_orig_x then Some new_orig_x
  else None;;

let redraw st =
    (* draws until the current pause_no or page end *)
  Grdev.set_busy Grdev.Busy;
  st.cont <- None;
  st.aborted <- false;
  let () =
    try
      Grdev.continue ();
(*      Grdev.clear_dev () ;
        (* RDC: moved inside render_step to properly handle backgrounds *) *)
(*
   let blank_w = 20
   and blank_h = 20 in
*)
      Driver.clear_symbols ();
      if !bounding_box then draw_bounding_box st;
      if !pauses then
        let f = Driver.render_step st.cdvi st.page_no
            (st.base_dpi *. st.ratio) st.orig_x st.orig_y in
        let current_pause = ref 0 in
        begin
          try
            while
              try f () with
                | Driver.Pause ->
                    if !current_pause = st.pause_no then raise Driver.Pause
                    else begin incr current_pause; true end
            do () done;
            if !current_pause < st.pause_no then
              st.pause_no <- !current_pause
          with
          | Driver.Pause ->
              st.cont <- Some f;
        end
      else
        begin
          Driver.render_page  st.cdvi st.page_no
            (st.base_dpi *. st.ratio) st.orig_x st.orig_y;
        end;
    with Grdev.Stop ->
      st.aborted <- true in
  Grdev.synchronize();
  Grdev.set_busy (if st.cont = None then Grdev.Free else Grdev.Pause);;

let goto_previous_pause n st =
  if n > 0 then
    begin
      st.pause_no <- max 0 (st.pause_no -n);
      redraw st
    end;;

let find_xref tag default st =
  try Hashtbl.find st.dvi.Dvi.xrefs tag
  with Not_found ->
    if st.frozen then
      begin
        Driver.scan_special_pages st.cdvi max_int;
        st.frozen <- false;
        try Hashtbl.find st.dvi.Dvi.xrefs tag
        with Not_found -> default
      end
    else default;;

exception Link;;

let exec_xref link =
  let call command =
    let pid = Misc.fork_process command in
    (* I don't understand this change -Didier: it was 
    Grdev.wait_button_down() in *)
    (* only to launch embeded apps *)
    if Graphics.button_down () then
      ignore (Graphics.wait_next_event [ Graphics.Button_up ]) in
  if Misc.has_prefix "file:" link then
    begin
      try
        let filename, arguments =
          match Misc.split_string (Misc.get_suffix "file:" link) 
	         (function '#' -> true | _ -> false) 0 with
          | [ filename ; tag ] -> filename, ["-html"; tag ]
          | [ filename ] -> filename, []
          | _ ->
              Misc.warning ("Invalid link "^link);
              raise Link
        in
        if Sys.file_exists filename then
          begin
            if Misc.has_suffix ".dvi"  filename then
              call (String.concat " " (Sys.argv.(0) :: arguments @ [ filename ]))
            else if Misc.has_suffix ".txt" filename ||
                    Misc.has_suffix ".tex" filename then
              call ("xterm -e less " ^ filename)
            else if Misc.has_suffix ".html"  filename ||
                    Misc.has_suffix ".htm"  filename then
              call (!browser ^ " " ^ link)
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
      | Misc.Match -> assert false
      | Link -> ()
    end
  else if Misc.has_prefix "http:" link then
    call (!browser ^ " " ^ link);;

let page_start default st =
  match !start_html with
  | None -> default
  | Some html ->
      Driver.scan_special_pages st.cdvi max_int;
      find_xref html default st;;

let rec clear_page_stack max stack =
  let pages = Array.create max false in
  let rec clear = function
    | p :: stack ->
        let s = clear stack in
        let pa = if p < 0 then -1 - p else p in
        if pa < max && not pages.(pa) then
          begin
            pages.(pa) <- true;
            p :: s
          end
        else s
    | _ -> [] in
  clear stack;;

let reload_time st =
  try let s = Unix.stat st.filename in s.Unix.st_mtime
  with _ -> st.last_modified;;

let reload st =
  try
    st.last_modified <- reload_time st;
    let dvi = Dvi.load st.filename in
    let cdvi = Driver.cook_dvi dvi in
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
    let page = page_start (min st.page_no (st.num_pages - 1)) st in
    if page <> st.page_no then st.pause_no <- 0;
    st.page_no <- page;
    st.frozen <- true;
    st.aborted <- true;
    update_dvi_size false st ;
    Misc.dops := !Misc.pson;
    redraw st
  with x ->
    assert (Misc.debug (Printexc.to_string x));
    st.cont <- None;;

let changed st = reload_time st > st.last_modified;;

let goto_page n st = (* go to the begining of the page *)
  let new_page_no = max 0 (min n (st.num_pages - 1)) in
  if st.page_no <> new_page_no || st.aborted then
    begin
      if st.page_no <> new_page_no then
        st.exchange_page <- st.page_no;
      st.page_no <- new_page_no ;
      st.pause_no <- 0 ;
      redraw st
    end;;

let push_stack b n st =
  match st.page_stack with
  | p :: rest when p = n -> if b then st.page_stack <- ( -1 - n ) :: rest
  | p :: rest when p = -1 - n -> ()
  | all -> st.page_stack <- (if b then -1 - n else n) :: all;;

let push_page b n st =
  let new_page_no = max 0 (min n (st.num_pages - 1)) in
  if st.page_no <> new_page_no || st.aborted then
    begin
      push_stack b st.page_no st;
      goto_page n st
    end;;

let pop_page b n st =
  assert
    (debug_pages
       (Printf.sprintf "%s\n => popping %s page %d "
          (page_stack_to_string st.page_no st.page_stack)
          (string_of_bool b)
          n));
  let rec pop n return_page return_stack stack =
    match n, stack with
    | n, _ when n <= 0 ->
        return_page, return_stack
    | _, [] ->
        return_page, return_stack
    | n, h :: t ->
        if b && h >= 0
        then pop n return_page return_stack t
        else pop (pred n) h t t in
  let page, stack = pop n st.page_no st.page_stack st.page_stack in
  st.page_stack <- stack;
  goto_page (if page > 0 then page else -1 - page) st;;

let goto_href link st = (* goto page of hyperref h *)
  let page =
    if Misc.has_prefix "#" link then
      let tag = Misc.get_suffix "#" link in
      find_xref tag st.page_no st
    else
      begin
        exec_xref link;
        st.page_no
      end in
  push_page true page st;
  Grdev.H.flashlight (Grdev.H.Name link);;

let goto_next_page st =
  if st.page_no <> st.num_pages - 1 then goto_page (st.page_no + 1) st;;

let resize st x y =
  attr.geom <-
    { width = x;
      height = y;
      xoffset = No_offset;
      yoffset = No_offset;
    };
  update_dvi_size true st;
  redraw st;;

let scale n st =
  let factor =
    if n > 0 then !scale_step ** float n
    else  (1. /. !scale_step) ** float (0 - n) in
  if !autoresize then
    begin
      let scale x = int_of_float (float x *. factor) in
      attr.geom.width <- scale st.size_x;
      attr.geom.height <- scale st.size_y;
      Grdev.close_dev();
      Grdev.open_dev (Printf.sprintf " " ^ string_of_geometry attr.geom);
    end
  else
    begin
      let new_ratio = factor *. st.ratio in
      if new_ratio >= 0.02 && new_ratio < 50.0 then
        begin
          st.ratio <- new_ratio ;
          let (cx, cy) = (st.size_x / 2, st.size_y / 2) in
          st.orig_x <- int_of_float (float (st.orig_x - cx) *. factor) + cx ;
          st.orig_y <- int_of_float (float (st.orig_y - cy) *. factor) + cy ;
        end;
    end;
  update_dvi_size true st;
  redraw st;;

  module B =
    struct
      let nop st = ()
      let push_next_page st =
        push_page false (st.page_no + max 1 st.num) st
      let next_pause st =
        if st.cont = None then push_next_page st
        else goto_next_pause (max 1 st.num) st
      let digit k st =
        st.next_num <- st.num * 10 + k
      let next_page st =
        goto_page (st.page_no + max 1 st.num) st
      let goto st =
        push_page true (if st.num > 0 then st.num - 1 else st.num_pages) st

      let push_page st =
        push_stack true st.page_no st
      let previous_page st =
        goto_page (st.page_no - max 1 st.num) st
      let pop_previous_page st =
        pop_page false (max 1 st.num) st
      let previous_pause st =
        if st.pause_no > 0
        then goto_previous_pause (max 1 st.num) st
        else pop_previous_page st
      let pop_page st =
        pop_page true 1 st
      let first_page st =
        goto_page 0 st
      let last_page st =
        goto_page max_int st
      let exchange_page st =
        goto_page st.exchange_page st
      let unfreeze_fonts st =
        Driver.unfreeze_fonts st.cdvi
      let unfreeze_glyphs st =
        Driver.unfreeze_glyphs st.cdvi (st.base_dpi *. st.ratio)
      let center st =
        st.ratio <- 1.0 ;
        st.orig_x <- (st.size_x - st.dvi_width) / 2 ;
        st.orig_y <- (st.size_y - st.dvi_height) / 2 ;
        update_dvi_size false st ;
        redraw st

      let scale_up st = scale (max 1 st.num) st
      let scale_down st = scale (0 - max 1 st.num) st

      let nomargin st =
        attr.hmargin <- Px 0; attr.vmargin <- Px 0;
        update_dvi_size true st;
        redraw st

      let page_left st =
        match (move_within_margins_x st (attr.geom.width - 10)) with
        | Some n ->
            if n > st.orig_x  then begin
              st.orig_x <- n;
              set_bbox st;
              redraw st
            end
        | None -> ()

      let page_down st =
        let none () =
          if st.page_no < st.num_pages - 1 then begin
              (* the following test is necessary because of some
               * floating point rounding problem
               *)
            if attr.geom.height <
              st.dvi_height + 2 * (get_size_in_pix st attr.vmargin) then
              begin
                st.orig_y <- top_of_page st;
                set_bbox st;
              end;
            goto_page (st.page_no + 1) st
          end
        in
        begin
          match (move_within_margins_y st (10 - attr.geom.height)) with
          | Some n ->
                (* this test is necessary because of rounding errors *)
              if n > st.orig_y then none ()
              else begin
                st.orig_y <- n;
                set_bbox st;
                redraw st
              end
          | None -> none ()
        end

      let page_up st =
        let none () =
          if st.page_no > 0 then begin
            if attr.geom.height <
              st.dvi_height + 2 * (get_size_in_pix st attr.vmargin) then
              begin
                st.orig_y <- bottom_of_page st;
                set_bbox st;
              end;
            goto_page (st.page_no -1) st
          end
        in
        begin
          match (move_within_margins_y st (attr.geom.height - 10)) with
          | Some n ->
              if n < st.orig_y then none ()
              else begin
                st.orig_y <- n;
                set_bbox st;
                redraw st
              end
          | None -> none ()
        end

      let page_right st =
        match (move_within_margins_x st (10 - attr.geom.width)) with
        | Some n ->
            if n < st.orig_x then begin
              st.orig_x <- n;
              set_bbox st;
              redraw st
            end
        | None -> ()


      let redraw = redraw
      let reload = reload

      let fullscreen st =
        let x, y = 
          match st.fullscreen with
            None ->
              let x = GraphicsY11.origin_x() in
              let y = GraphicsY11.origin_y() in
              st.fullscreen <- Some (x, y, st.size_x, st.size_y);
              Grdev.reposition ~x:0 ~y:0 ~w:(-1) ~h:(-1);
          | Some (x, y, w, h) -> 
              st.fullscreen <- None;
              Grdev.reposition ~x ~y ~w ~h in
        resize st x y
        
      let exit st = raise Exit
      let clear_image_cash st = (* clear image cache *)
        Grdev.clean_ps_cache ()
      let help st =
        let int = function
          | No_offset -> 0
          | Plus x -> x
          | Minus y -> - y in
        let pid = Misc.fork_process
            (Printf.sprintf "%s -g %dx%d %s"
	       Sys.argv.(0)
               attr.geom.width
               attr.geom.height
               Config.splash_screen) in
        ()
    end;;

let bindings = Array.create 256 B.nop;;
for i = 0 to 9 do
  bindings.(Char.code '0' + i) <- B.digit i
done;
let bind (key, action) = bindings.(Char.code key) <- action  in
List.iter bind [
    (* For instance *)
    (* Alan: I modified the bindings for hjkl to move the page around *)
  'h'   , B.page_left;
  'i' 	, B.pop_page;
  'j' 	, B.page_down;
  'k' 	, B.page_up;
  'l' 	, B.page_right;
  'm' 	, B.push_page;
  
  ' ' 	, B.next_pause;
  'n' 	, B.next_page;
  '\r'	, B.push_next_page;
  '\t'	, B.push_page;
  'p' 	, B.previous_page;
  'P' 	, B.previous_pause;
  '\b'	, B.pop_previous_page;
  '\027'	, B.pop_page; 
  ',' 	, B.first_page; 
  '.' 	, B.last_page; 
  'f' 	, B.unfreeze_fonts; 
  'F' 	, B.unfreeze_glyphs; 
  ''   , B.fullscreen;
  'r' 	, B.redraw; 
  'R' 	, B.reload; 
  'c' 	, B.center; 
  '<' 	, B.scale_down; 
  '>' 	, B.scale_up; 
  'g' 	, B.goto; 
  'x' 	, B.exchange_page; 
  '#' 	, B.nomargin; 
  'q' 	, B.exit; 
  'C' 	, B.clear_image_cash; 
  '?' 	, B.help; 
];; 
    
let main_loop filename =
  let st = init filename in
  let cont = ref None in (* drawing continuation *)
  Grdev.set_title ("Advi: " ^ Filename.basename filename);
  Grdev.open_dev (" " ^ string_of_geometry attr.geom) ;
  set_bbox st;
  if st.page_no > 0 && !Misc.dops then
    Driver.scan_special_pages st.cdvi st.page_no
  else
    st.page_no <- page_start 0 st;
  redraw st;
  (* num is the current number entered by keyboard *)
  try while true do
    let ev = if changed st then Grdev.Refreshed else Grdev.wait_event () in
    st.num <- st.next_num;
    st.next_num <- 0;
    match ev with
    | Grdev.Refreshed -> reload st
    | Grdev.Resized (x, y) -> resize st x y
    | Grdev.Key c -> bindings.(Char.code c) st
    | Grdev.Href h -> goto_href h st
    | Grdev.Advi (s, a) -> a ()
    | Grdev.Move (w, h) ->
        st.orig_x <- st.orig_x + w ;
        st.orig_y <- st.orig_y + h ;
        set_bbox st;
        redraw st
    | Grdev.Region (x, y, w, h) -> ()
    | Grdev.Selection s -> selection s
    | Grdev.Position (x, y) ->
        position st x y
    | Grdev.Click (Grdev.Top_left, _,_,_) ->
        if !click_turn_page then B.pop_page st
    | Grdev.Click (_, Grdev.Button1, _, _) ->
        if !click_turn_page then B.previous_pause st
    | Grdev.Click (_, Grdev.Button2, _, _) ->
        if !click_turn_page then B.pop_previous_page st
    | Grdev.Click (_, Grdev.Button3, _, _) ->
        if !click_turn_page then B.next_pause st

(*
   | Grdev.Click (Grdev.Bottom_right, _) -> B.next_pause st
   | Grdev.Click (Grdev.Bottom_left, _) -> B.pop_previous_page st
   | Grdev.Click (Grdev.Top_right, _) -> B.push_page st
*)
    | _ -> ()

  done with Exit -> Grdev.close_dev ();;
