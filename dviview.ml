(***********************************************************************)
(*                                                                     *)
(*                             Active-DVI                              *)
(*                                                                     *)
(*                   Projet Cristal, INRIA Rocquencourt                *)
(*                                                                     *)
(*  Copyright 2002 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the GNU Lesser General Public License.          *)
(*                                                                     *)
(*  Jun Furuse, Didier Rémy and Pierre Weis.                           *)
(*  Contributions by Roberto Di Cosmo, Didier Le Botlan,               *)
(*  Xavier Leroy, and Alan Schmitt.                                    *)
(*                                                                     *)
(*  Based on Mldvi by Alexandre Miquel.                                *)
(***********************************************************************)

let pauses = Options.flag true "-nopauses" "Switch pauses off";;
let fullwidth = Options.flag false "-fullwidth" "Adjust size to width";;
let bounding_box = Options.flag false "-bbox" "Show the bounding box";;

let start_page = ref 0;;
Options.add
  "-page"
  (Arg.Int (fun i -> start_page := i))
  "INT\tMake advi start at page INT";;

let starting_page npages =
 if !start_page > 0 then min !start_page npages - 1 else 0;;

let start_html = ref None;;
Options.add
  "-html"
  (Arg.String (fun s -> start_html := Some s))
  "STRING\tMake advi start at html reference of name STRING";;
let debug_pages =
  Options.debug
    "--debug_pages"
    "Debug page motion";;

let browser = ref "netscape-communicator";;
Options.add
  "-browser"
  (Arg.String (fun s -> browser := s))
  "STRING\tCommand to call the browser";;

let pager = ref "xterm -e less";;
Options.add
  "-pager"
  (Arg.String (fun s -> pager := s))
  "STRING\tCommand to call the pager";;

let click_turn_page =
  Options.flag false
    "-click_turn"
    "Turn pages with mouse clicks (see the doc)";;

let page_stack_to_string page stack =
  let stack = String.concat " " (List.map string_of_int stack) in
  Printf.sprintf "Page no: %d Page_stack: %s" page stack;;

let scale_step = ref (sqrt (sqrt (sqrt 2.)));;

let set_scale x =
  if x > 1.0 && x <= 2. then scale_step := x
  else
    Misc.warning
      (Printf.sprintf "out of bounds scale_step %f ignored" x);;
Options.add
  "-scalestep"
  (Arg.Float set_scale)
  "REAL\tScale step for '<' and '>' (default sqrt (sqrt (sqrt 2.0)))";;

let autoresize = ref true;;
Options.add
  "-noautoresize"
  (Arg.Clear autoresize)
  "\tPrevents scaling from resizing the window (done if geometry is provided)"
;;

let dpi_resolution = ref 72.27;;
let set_dpi_resolution r = dpi_resolution := max r 72.27;;

Options.add
  "-resolution"
  (Arg.Float set_dpi_resolution)
  "REAL\tDpi resolution of the screen (min 72.27)))";;

module Symbol = Grdev.Symbol;;

open Dimension;;

exception Error of string;;

(*** View attributes ***)
(* things we can set before initialization *)

type attr = {
    mutable geom : Ageometry.t;
    mutable crop : bool;
    mutable hmargin : dimen;
    mutable vmargin : dimen
  };;

(*** The view state ***)
type mode = Selection | Control;;
type state = {
    (* DVI attributes *)
    filename : string;
    mutable dvi : Dvi.t;
    mutable cdvi : Driver.cooked_dvi;
    mutable num_pages : int;
    (* Page layout *)
    mutable base_dpi : float;
    mutable dvi_width : int;   (* in pixels *)
    mutable dvi_height : int;  (* in pixels *)
    (* Window size *)
    mutable size_x : int;
    mutable size_y : int;
    (* Current parameters *)
    mutable orig_x : int;
    mutable orig_y : int;
    mutable ratio : float;
    mutable page_number : int;
    mutable page_stack : int list;
    mutable exchange_page : int;
    mutable last_modified : float;
    mutable button : (int * int) option;
    mutable fullscreen : (int * int * int * int) option;

    mutable pause_number : int;
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

let set_page_number st n =
 Userfile.save_page_number n;
 Thumbnails.save n;
 st.page_number <- n;;

(*** Setting the geometry ***)

(*** Setting other parameters ***)

let attr =
  { geom = {
      Ageometry.width = 0;
      Ageometry.height = 0;
      Ageometry.xoffset = Ageometry.No_offset;
      Ageometry.yoffset = Ageometry.No_offset;
    };
    crop = false;
    hmargin = Px 0;
    vmargin = Px 0
  };;

let set_autoresize b = autoresize := b
let set_geometry g = attr.geom <- Ageometry.parse g;;

let set_crop b = attr.crop <- b;;

let set_hmargin d = attr.hmargin <- normalize d;;

let set_vmargin d = attr.vmargin <- normalize d;;

(*** Initialization ***)
let init filename =
  let dvi =
    try Dvi.load filename
    with
    | Sys_error _ -> raise (Error ("cannot open `" ^ filename ^ "'"))
    | Dvi.Error s -> raise (Error (filename ^ ": " ^ s))
    | _ -> raise (Error ("error while loading `" ^ filename ^ "'")) in
  let cdvi = Driver.cook_dvi dvi
  and dvi_res = !dpi_resolution
  and mag = float dvi.Dvi.preamble.Dvi.pre_mag /. 1000.0 in
  let w_sp = dvi.Dvi.postamble.Dvi.post_width
  and h_sp = dvi.Dvi.postamble.Dvi.post_height in
  let w_in = mag *. ldexp (float w_sp /. dvi_res) (-16)
  and h_in = mag *. ldexp (float h_sp /. dvi_res) (-16) in
  let wdpi =
    match attr.hmargin with
    | Px n -> float (attr.geom.Ageometry.width - 2 * n) /. w_in
    | In f -> float attr.geom.Ageometry.width /. (w_in +. 2.0 *. f)
    | _ -> assert false
  and hdpi =
    match attr.vmargin with
    | Px n -> float (attr.geom.Ageometry.height - 2 * n) /. h_in
    | In f -> float attr.geom.Ageometry.height /. (h_in +. 2.0 *. f)
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
      (min attr.geom.Ageometry.width sx, min attr.geom.Ageometry.height sy)
    end else
      (attr.geom.Ageometry.width, attr.geom.Ageometry.height) in
  attr.geom.Ageometry.width <- size_x;
  attr.geom.Ageometry.height <- size_y;
  let orig_x = (size_x - width) / 2
  and orig_y = (size_y - height) / 2 in
  let last_modified =
    try (Unix.stat filename).Unix.st_mtime
    with _ -> 0.0 in
  Options.dops := !Options.pson;
  let st =
    let npages = Array.length dvi.Dvi.pages in
    { filename = filename;
      dvi = dvi;
      cdvi = cdvi;
      num_pages = npages;
      base_dpi = base_dpi;
      dvi_width = width;
      dvi_height = height;
      size_x = size_x;
      size_y = size_y;
      orig_x = orig_x;
      orig_y = orig_y;
      ratio = 1.0;
      page_stack = [];
      exchange_page = 0;
      page_number = starting_page npages;
      last_modified = last_modified;
      button = None;
      fullscreen = None;

      pause_number = 0;
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
  let dvi_res = !dpi_resolution
  and mag = float st.dvi.Dvi.preamble.Dvi.pre_mag /. 1000.0
  and w_sp = st.dvi.Dvi.postamble.Dvi.post_width
  and h_sp = st.dvi.Dvi.postamble.Dvi.post_height in
  let w_in = mag *. ldexp (float w_sp /. dvi_res) (-16)
  and h_in = mag *. ldexp (float h_sp /. dvi_res) (-16) in

  if init then
    begin
      let wdpi =
        match attr.hmargin with
        | Px n -> float (attr.geom.Ageometry.width - 2 * n) /. w_in
        | In f -> float attr.geom.Ageometry.width /. (w_in +. 2.0 *. f)
        | _ -> assert false
      and hdpi =
        match attr.vmargin with
        | Px n -> float (attr.geom.Ageometry.height - 2 * n) /. h_in
        | In f -> float attr.geom.Ageometry.height /. (h_in +. 2.0 *. f)
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
          (attr.geom.Ageometry.width, attr.geom.Ageometry.height) in
      (*
      attr.geom.width <- size_x;
      attr.geom.height <- size_y;
      *)
      st.base_dpi <- base_dpi;
      st.size_x <- size_x;
      st.size_y <- size_y;
      let orig_x, orig_y = (size_x - width) / 2,  (size_y - height) / 2 in
      st.orig_x <- orig_x;
      st.orig_y <- orig_y;
    end;
  st.dvi_width <- int_of_float (st.base_dpi *. w_in *. st.ratio);
  st.dvi_height <- int_of_float (st.base_dpi *. h_in *. st.ratio);
  set_bbox st;;

(* incremental drawing *)
let goto_next_pause n st =
  let rec aux n st =
    if n > 0 then
      begin match st.cont with
      | None -> ()
      | Some f ->
          st.cont <- None;
          try
            begin try
              while f () do () done;
              st.pause_number <- st.pause_number + 1;
            with
            | Driver.Wait sec ->
                ignore (Grdev.sleep sec);
                st.cont <- Some f;
                aux n st
            | Driver.Pause ->
                st.pause_number <- st.pause_number + 1;
                st.cont <- Some f;
                aux (pred n) st
            end;
          with Grdev.Stop -> st.aborted <- true;
      end
  in
  aux n st;
  Grdev.synchronize();
  Grdev.set_busy (if st.cont = None then Grdev.Free else Grdev.Pause);;

let draw_bounding_box st =
  Grdev.set_color 0xcccccc;
  Grdev.fill_rect st.orig_x st.orig_y st.dvi_width 1;
  Grdev.fill_rect st.orig_x st.orig_y 1 st.dvi_height;
  Grdev.fill_rect st.orig_x (st.orig_y + st.dvi_height) st.dvi_width 1;
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
  | Some (s, line, bound, left, before, after, right, file) ->
      let line = max 0 line in
      let bound = max 0 bound in
      let file = match file with Some f ->  f | _ -> "" in
      Printf.printf "#line %d, %d <<%s<<%s>><<%s>>%s>> %s\n"
        line bound left before after right file;
      flush stdout
  | None -> ();;

(* User has selected a region with the mouse. We dump characters. *)
let selection s = Grdev.cut s;;

let get_size_in_pix st = function
  | Px n -> n
  | In f -> int_of_float (st.base_dpi *. f)
  | _ -> assert false;;

let vmargin_size st = get_size_in_pix st attr.vmargin;;
let hmargin_size st = get_size_in_pix st attr.hmargin;;

(* The next four functions returns the position that correspond to the top,
   the bottom, the left and right of the page *)
let top_of_page = vmargin_size;;

let bottom_of_page st =
  attr.geom.Ageometry.height - st.dvi_height - vmargin_size st;;

let left_of_page = hmargin_size;;

let right_of_page st =
  attr.geom.Ageometry.width - st.dvi_width - hmargin_size st;;

(* the two following functions move the displayed part of the page while
   staying inside the margins *)
let move_within_margins_y st movey =
  let tmp_orig_y = st.orig_y + movey in
  let new_orig_y =
    let vmargin_size = vmargin_size st in
    if movey < 0 then begin
      if tmp_orig_y + st.dvi_height + vmargin_size < attr.geom.Ageometry.height
      then attr.geom.Ageometry.height - st.dvi_height - vmargin_size
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
  let tmp_orig_x = st.orig_x + movex in
  let new_orig_x =
    let hmargin_size = hmargin_size st in
    if movex < 0 then begin
      if tmp_orig_x + st.dvi_width + hmargin_size < attr.geom.Ageometry.width
      then attr.geom.Ageometry.width - st.dvi_width - hmargin_size
      else tmp_orig_x
    end else begin
      if tmp_orig_x - hmargin_size > 0
      then hmargin_size
      else tmp_orig_x
    end
  in
  if st.orig_x <> new_orig_x then Some new_orig_x
  else None;;

let redraw ?trans st =
  (* draws until the current pause_number or page end *)
  (* the pauses and waits appears before are ignored *)
  Grdev.set_busy Grdev.Busy;
  st.cont <- None;
  st.aborted <- false;
  begin
    try
      Grdev.continue ();
      Driver.clear_symbols ();
      if !bounding_box then draw_bounding_box st;
      let f =
        Driver.render_step st.cdvi st.page_number ?trans
          (st.base_dpi *. st.ratio) st.orig_x st.orig_y in
      if !pauses then begin
        let current_pause = ref 0 in
        try
          while
            try f () with
            | Driver.Wait _ -> true
            | Driver.Pause ->
                if !current_pause = st.pause_number then raise Driver.Pause
                else begin incr current_pause; true end
          do () done;
          if !current_pause < st.pause_number
          then st.pause_number <- !current_pause
        with
        | Driver.Pause -> st.cont <- Some f
      end else begin
        Transimpl.sleep := (fun _ -> true); (* always breaks *)
        while try f () with Driver.Wait _ | Driver.Pause -> true
        do () done
      end
    with
    | Grdev.Stop -> st.aborted <- true
  end;
  Grdev.synchronize ();
  Grdev.set_busy (if st.cont = None then Grdev.Free else Grdev.Pause);
Misc.debug_stop "Page has been drawn\n";
;;

let redisplay st =
  st.pause_number <- 0;
  redraw st;;

let goto_previous_pause n st =
  if n > 0 then begin
    st.pause_number <- max 0 (st.pause_number - n);
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
    Grdev.wait_button_up ();
    ignore (Launch.fork_process command) in
  if Misc.has_prefix "file:" link then
    try
      let filename, arguments =
        match Misc.split_string (Misc.get_suffix "file:" link)
               (function '#' -> true | _ -> false) 0 with
        | [ filename; tag ] -> filename, ["-html"; tag ]
        | [ filename ] -> filename, []
        | _ -> Misc.warning ("Invalid link " ^ link); raise Link in
      if not (Sys.file_exists filename) then
        Misc.warning
          (Printf.sprintf "File %s non-existent or not readable" filename) else
      if Misc.has_suffix ".dvi"  filename then
        let command =
          String.concat " " (Sys.argv.(0) :: arguments @ [filename]) in
        call command else
      if Misc.has_suffix ".txt" filename ||
         Misc.has_suffix ".tex" filename then
        call (!pager ^ " " ^ filename) else
      if Misc.has_suffix ".html" filename ||
         Misc.has_suffix ".htm" filename then
        call (!browser ^ " " ^ link) else
      Misc.warning
        (Printf.sprintf "Don't know what to do with file %s" filename)
    with
    | Misc.Match -> assert false
    | Link -> () else
  if Misc.has_prefix "http:" link then call (!browser ^ " " ^ link) else
  Misc.warning (Printf.sprintf "Don't know what to do with link %s" link);;

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
    let dvi_res = !dpi_resolution
    and mag = float dvi.Dvi.preamble.Dvi.pre_mag /. 1000.0 in
    let w_sp = dvi.Dvi.postamble.Dvi.post_width
    and h_sp = dvi.Dvi.postamble.Dvi.post_height in
    let w_in = mag *. ldexp (float w_sp /. dvi_res) (-16)
    and h_in = mag *. ldexp (float h_sp /. dvi_res) (-16) in
    let width = int_of_float (w_in *. st.base_dpi *. st.ratio)
    and height = int_of_float (h_in *. st.base_dpi *. st.ratio) in
    let npages =  Array.length dvi.Dvi.pages in
    st.dvi <- dvi;
    st.cdvi <- cdvi;
    st.num_pages <- npages;
    st.page_stack <- clear_page_stack npages st.page_stack;
    let npage = page_start (min st.page_number (st.num_pages - 1)) st in
    if npage <> st.page_number then st.pause_number <- 0;
    set_page_number st npage;
    st.frozen <- true;
    st.aborted <- true;
    update_dvi_size false st;
    Options.dops := !Options.pson;
    redraw ?trans:(Some Transitions.DirTop) st
  with x ->
    (* To be revisited (should assert Options.debug) *)
    assert (Misc.debug_endline (Printexc.to_string x); true);
    st.cont <- None;;

let changed st = reload_time st > st.last_modified;;

let goto_page n st = (* go to the begining of the page *)
  let new_page_number = max 0 (min n (st.num_pages - 1)) in
  if st.page_number <> new_page_number || st.aborted then
    begin
      if st.page_number <> new_page_number
      then st.exchange_page <- st.page_number;
      let trans =
        if new_page_number = succ st.page_number
           then Some Transitions.DirRight else
        if new_page_number = pred st.page_number
           then Some Transitions.DirLeft else
        if new_page_number = st.page_number
           then Some Transitions.DirTop else
        None in
      set_page_number st new_page_number;
      st.pause_number <- 0;
      redraw ?trans st
    end;;

let push_stack b n st =
  match st.page_stack with
  | p :: rest when p = n -> if b then st.page_stack <- ( -1 - n ) :: rest
  | p :: rest when p = -1 - n -> ()
  | all -> st.page_stack <- (if b then -1 - n else n) :: all;;

let push_page b n st =
  let new_page_number = max 0 (min n (st.num_pages - 1)) in
  if st.page_number <> new_page_number || st.aborted then
    begin
      push_stack b st.page_number st;
      goto_page n st
    end;;

let pop_page b n st =
  assert
    (debug_pages
       (Printf.sprintf "%s\n => popping %s page %d "
          (page_stack_to_string st.page_number st.page_stack)
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
  let npage, stack = pop n st.page_number st.page_stack st.page_stack in
  st.page_stack <- stack;
  goto_page (if npage > 0 then npage else -1 - npage) st;;

let goto_href link st = (* goto page of hyperref h *)
  let npage =
    if Misc.has_prefix "#" link then
      let tag = Misc.get_suffix "#" link in
      find_xref tag st.page_number st
    else
      begin
        exec_xref link;
        st.page_number
      end in
  push_page true npage st;
  Grdev.H.flashlight (Grdev.H.Name link);;

let goto_next_page st =
  if st.page_number <> st.num_pages - 1 then goto_page (st.page_number + 1) st;;

let resize st x y =
  attr.geom <-
    { Ageometry.width = x;
      Ageometry.height = y;
      Ageometry.xoffset = Ageometry.No_offset;
      Ageometry.yoffset = Ageometry.No_offset;
    };
  update_dvi_size true st;
  redraw st;;

let scale n st =
  let factor =
    if n > 0 then !scale_step ** float n
    else  (1. /. !scale_step) ** float (0 - n) in
  if !autoresize then begin
     let scale x = int_of_float (float x *. factor) in
     attr.geom.Ageometry.width <- scale st.size_x;
     attr.geom.Ageometry.height <- scale st.size_y;
     Grdev.close_dev ();
     Grdev.open_dev (Printf.sprintf " " ^ Ageometry.to_string attr.geom);
  end else begin
     let new_ratio = factor *. st.ratio in
     if new_ratio >= 0.02 && new_ratio < 50.0 then begin
        st.ratio <- new_ratio;
        let (cx, cy) = (st.size_x / 2, st.size_y / 2) in
        st.orig_x <- int_of_float (float (st.orig_x - cx) *. factor) + cx;
        st.orig_y <- int_of_float (float (st.orig_y - cy) *. factor) + cy;
     end;
  end;
  update_dvi_size true st;
  redraw st;;

module B =
  struct
    let nop st = ()
    let push_next_page st =
      push_page false (st.page_number + max 1 st.num) st
    let next_pause st =
      if st.cont = None then push_next_page st
      else goto_next_pause (max 1 st.num) st
    let digit k st =
      st.next_num <- st.num * 10 + k
    let next_page st =
      goto_page (st.page_number + max 1 st.num) st
    let goto st =
      push_page true (if st.num > 0 then st.num - 1 else st.num_pages) st

    let push_page st =
      push_stack true st.page_number st
    let previous_page st =
      goto_page (st.page_number - max 1 st.num) st
    let pop_previous_page st =
      pop_page false (max 1 st.num) st
    let previous_pause st =
      if st.pause_number > 0
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
      st.ratio <- 1.0;
      st.orig_x <- (st.size_x - st.dvi_width) / 2;
      st.orig_y <- (st.size_y - st.dvi_height) / 2;
      update_dvi_size false st;
      redraw st

    let scale_up st = scale (max 1 st.num) st
    let scale_down st = scale (0 - max 1 st.num) st

    let nomargin st =
      attr.hmargin <- Px 0; attr.vmargin <- Px 0;
      update_dvi_size true st;
      redraw st

    let page_left st =
      match move_within_margins_x st (attr.geom.Ageometry.width - 10) with
      | Some n ->
          if n > st.orig_x  then begin
            st.orig_x <- n;
            set_bbox st;
            redraw st
          end
      | None -> ()

    let page_right st =
      match (move_within_margins_x st (10 - attr.geom.Ageometry.width)) with
      | Some n ->
          if n < st.orig_x then begin
            st.orig_x <- n;
            set_bbox st;
            redraw st
          end
      | None -> ()

    let page_down st =
      let none () =
        if st.page_number < st.num_pages - 1 then begin
            (* the following test is necessary because of some
             * floating point rounding problem
             *)
          if attr.geom.Ageometry.height <
            st.dvi_height + 2 * vmargin_size st then begin
           st.orig_y <- top_of_page st;
           set_bbox st;
          end;
          goto_page (st.page_number + 1) st
        end in
      match move_within_margins_y st (10 - attr.geom.Ageometry.height) with
      | Some n ->
          (* this test is necessary because of rounding errors *)
          if n > st.orig_y then none () else begin
            st.orig_y <- n;
            set_bbox st;
            redraw st
          end
      | None -> none ()

    let page_up st =
      let none () =
        if st.page_number > 0 then begin
          if attr.geom.Ageometry.height <
             st.dvi_height + 2 * vmargin_size st then begin
            st.orig_y <- bottom_of_page st;
            set_bbox st;
          end;
          goto_page (st.page_number - 1) st
        end in
      match move_within_margins_y st (attr.geom.Ageometry.height - 10) with
      | Some n ->
          if n < st.orig_y then none () else begin
            st.orig_y <- n;
            set_bbox st;
            redraw st
          end
      | None -> none ()

    let redraw = redraw ?trans:(Some Transitions.DirNone)
    let reload = reload
    let redisplay = redisplay

    let fullscreen st =
      let x, y =
        match st.fullscreen with
        | None ->
            let x = GraphicsY11.origin_x () in
            let y = GraphicsY11.origin_y () in
            st.fullscreen <- Some (x, y, st.size_x, st.size_y);
            Grdev.reposition ~x:0 ~y:0 ~w:(-1) ~h:(-1);
        | Some (x, y, w, h) ->
            st.fullscreen <- None;
            Grdev.reposition ~x ~y ~w ~h in
      resize st x y
    let exit st = raise Exit
    let clear_image_cache st = (* clear image cache *)
      Grdev.clean_ps_cache ()
    let help st =
      let pid =
        Launch.fork_process
          (Printf.sprintf "%s -g %dx%d %s"
             Sys.argv.(0)
             attr.geom.Ageometry.width
             attr.geom.Ageometry.height
             Config.splash_screen) in
      ()

     let scratch st =
       Scratch.draw ()
     let scratch_write st =
       Scratch.write ()
  end;;

let bindings = Array.create 256 B.nop;;

let bind (key, action) = bindings.(int_of_char key) <- action;;

let bind_keys () =
  for i = 0 to 9 do
    bind (char_of_int (int_of_char '0' + i), B.digit i)
  done;
  List.iter bind [
   (* Default key bindings. *)

   (* General purpose keys. *)
   'q', B.exit;
   '?', B.help;

   (* hjkl to move the page around *)
   'h', B.page_left;
   'j', B.page_down;
   'k', B.page_up;
   'l', B.page_right;
   'c', B.center;

   (* m, i are reserved for scrolling
     'm', B.scroll_one_line_down;
     'i', B.scroll_one_line_up;
    *)

   (* return, tab, backspace, escape, and x
      to handle the page marks stack. *)
   '\t' (* tab *), B.push_page;
   '' (* Escape *), B.pop_page;

   '\b' (* backspace *), B.pop_previous_page;
   '\r' (* return *), B.push_next_page;

   'x', B.exchange_page;

   (* space, n, p, P, N to go on, or go back to next pause or page. *)
   ' ', B.next_pause;
   'n', B.next_page;
   'p', B.previous_page;
   'P', B.previous_pause;
   'N', B.next_pause;

   (* comma, ., g, to go to a page. *)
   ',', B.first_page;
   '.', B.last_page;
   'g', B.goto;

   (* r, Control-l, R, to redraw or reload. *)
   'r', B.redraw;
   'R', B.reload;
   '' (* CONTROL-l *), B.redisplay;

   (* Control-f, c, To handle the advi window. *)
   '' (* CONTROL-f *) , B.fullscreen;

   (* Scaling the page. *)
   '<', B.scale_down;
   '-', B.scale_down;
   '_', B.scale_down;
   '>', B.scale_up;
   '+', B.scale_up;
   '=', B.scale_up;
   '#', B.nomargin;

   (* Efficiency related keys. *)
   'f', B.unfreeze_fonts;
   'F', B.unfreeze_glyphs;
   'C', B.clear_image_cache;

   (* Scratching. *)
   's', B.scratch_write;
   'S', B.scratch;
  ];;

bind_keys ();;

let main_loop filename =
  let st = init filename in
  (* Check if whiterun *)
  if Launch.whiterun () then
    begin
      Driver.scan_special_pages st.cdvi (st.num_pages -1);
      Launch.dump_whiterun_commands ()
    end
  else
    begin
      Grdev.set_title ("Advi: " ^ Filename.basename filename);
      Grdev.open_dev (" " ^ Ageometry.to_string attr.geom);
      set_bbox st;
      if st.page_number > 0 && !Options.dops then
        Driver.scan_special_pages st.cdvi st.page_number
      else set_page_number st (page_start 0 st);
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
            st.orig_x <- st.orig_x + w;
            st.orig_y <- st.orig_y + h;
            set_bbox st;
            redraw st
        | Grdev.Edit (p, a) ->
            print_endline (Grdev.E.tostring p a);
            flush stdout;
            redraw st
        | Grdev.Region (x, y, w, h) -> ()
        | Grdev.Selection s -> selection s
        | Grdev.Position (x, y) ->
            position st x y
        | Grdev.Click (Grdev.Top_left, _, _, _) ->
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
      done with Exit -> Grdev.close_dev ()
    end;;
