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
(*  Jun Furuse, Didier R�my and Pierre Weis.                           *)
(*  Contributions by Roberto Di Cosmo, Didier Le Botlan,               *)
(*  Xavier Leroy, and Alan Schmitt.                                    *)
(*                                                                     *)
(*  Based on Mldvi by Alexandre Miquel.                                *)
(***********************************************************************)

(* $Id$ *)

let pauses =
  Options.flag true "-nopauses"
  "  switch pauses off,\
  \n\t (the default is to wait for specified pauses).";;
let fullwidth =
  Options.flag false "-fullwidth"
  "  adjust size to full width,\
   \n\t (the default is not to adjust to full width).";;
let bounding_box =
  Options.flag false "-bbox"
  "  show the bounding box,\
  \n\t (the default is to hide the bounding box).";;

let start_page = ref 0;;
Options.add
  "-page"
  (Arg.Int (fun i -> start_page := i))
  "<num>: start preview to page number <num>,\
  \n\t (the default starting page is page number 0).";;

let starting_page npages =
 if !start_page > 0 then min !start_page npages - 1 else 0;;

let start_html = ref None;;
Options.add
"-html"
(Arg.String (fun s -> start_html := Some s))
  "<anchor>: ask Active-DVI to start at HTML reference named <anchor>.";;
let debug_pages =
  Options.debug
    "--debug_pages"
    "  debug page motion.";;

let browser = ref "netscape-communicator";;
Options.add
  "-browser"
  (Arg.String (fun s -> browser := s))
  "<com>: set the HTML files viewer command to <com>,\
  \n\t (the default is \"netscape-communicator\").";;

let pager = ref "xterm -e less";;
Options.add
  "-pager"
  (Arg.String (fun s -> pager := s))
  "<com>: set the text files viewer command to <com>,\
  \n\t (the default is \"xterm -e less\").";;

let pdf_viewer = ref "xpdf";;
Options.add
  "-pdf-viewer"
  (Arg.String (fun s -> pdf_viewer := s))
  "<com>: set the PDF files viewer command to <com>,\
  \n\t (the default is \"xpdf\").";;

let ps_viewer = ref "gv";;
Options.add
  "-ps-viewer"
  (Arg.String (fun s -> ps_viewer := s))
  "<com>: set the PostScript files viewer command to <com>,\
  \n\t (the default is \"gv\").";;

let image_viewer = ref "xv";;
Options.add
  "-image-viewer"
  (Arg.String (fun s -> image_viewer := s))
  "<com>: set the image files viewer command to <com>,\
  \n\t (the default is \"xv\").";;

let film_viewer = ref "mplayer";;
Options.add
  "-film-viewer"
  (Arg.String (fun s -> film_viewer := s))
  "<com>: set the movie files player command to <com>,\
  \n\t (the default is \"mplayer\").";;

let click_turn_page =
  Options.flag false
    "-click-turn"
    "  turn pages with mouse clicks (see the doc).";;

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
  "<float>: set the step used by '<' and '>' for scaling the page,\
  \n\t (the default is \"sqrt (sqrt (sqrt 2.0))\").";;

let autoresize = ref true;;
Options.add
  "-noautoresize"
  (Arg.Clear autoresize)
  "  prevents scaling the page from resizing the window,\
  \n\t (automatically set when the geometry is specified)."
;;

let autoscale = ref true;;
Options.add
  "-noautoscale"
  (Arg.Clear autoscale)
  "  prevents resizing the window from scaling the page,\
  \n\t (automatically set when the geometry is specified)."
;;

let dpi_resolution = ref 72.27;;
let set_dpi_resolution r = dpi_resolution := max r 72.27;;

Options.add
  "-resolution"
  (Arg.Float set_dpi_resolution)
  "<float>: set the dpi resolution of the screen,\
  \n\t (the default (and minimum value) is 72.27))).";;

module Symbol = Grdev.Symbol;;

open Dimension;;

exception Error of string;;

(*** View attributes ***)
(* Things we can set before initialization *)

type attr = {
    mutable geom : Ageometry.t;
    mutable crop : bool;
    mutable hmargin : dimen;
    mutable vmargin : dimen
  };;

(*** The view state ***)
type mode =
   | Selection
   | Control;;
type toc =
   | Page of int
   | Thumbnails of int * (int * Graphics.image) array;;
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
    mutable page_marks : int list;
    mutable exchange_page : int;
    mutable last_modified : float;
    mutable button : (int * int) option;
    mutable fullscreen : (int * int * int * int * (int * int)) option;

    mutable pause_number : int;
    (* Attributes for Embedded postscript *)

    (* True when page was not completed: may need to redraw *)
    mutable aborted : bool;
    (* True when hrefs have not been processed *)
    mutable frozen : bool;
    (* Numeric value for keyboard interaction *)
    mutable num : int;
    (* Next numeric value *)
    mutable next_num : int;
    (* Control the action of the mouse *)
    mutable mode : mode;
    (* Some of f when on a pause *)
    mutable cont : (unit -> bool) option;
    mutable toc : toc array option;
    synchronize : bool;
};;

let set_page_number st n =
 Userfile.save_page_number n;
 Userfile.save_page_timing n;
 Thumbnails.save n;
 st.page_number <- n;;

(*** Setting the geometry ***)

(*****************************************************************************

  The `st' record contains the geometry for advi drawing, not the real
  geometry, which is stored in the attr record.
  Fields st.size_x st.size_y orig_x orig_y of the window are in
  advi coordinates, i.e.

       ----->
      |
      |
      \/

  Fields st.orig_x et st.orig_y indicates the advi "margins".
  Fields st.size_x et st.size_y the advi dimensions according to the
  resolution:

   - these are not the dimension of the window !Graphics.size_x() and
  Graphics.size_y() unless the window is self-adjusted to the advi dimensions;

   - they can be lower in one dimension when the window is forced a
  different shape;

   - they can be both higher when the window size is fixed and when drawing at
  a  higher scale.

  We always have st.orig_x + st.width + st.orig_x = st.size_x
  and the same for y coordinates

*****************************************************************************)

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
let set_autoscale b = autoscale := b
let set_geometry g = attr.geom <- Ageometry.parse g;;

let set_crop b = attr.crop <- b;;

let set_hmargin d = attr.hmargin <- normalize d;;

let set_vmargin d = attr.vmargin <- normalize d;;

(*** Initialization ***)
let init_geometry all st =
  let dvi = st.dvi in
  let dvi_res = !dpi_resolution
  and mag = float dvi.Dvi.preamble.Dvicommands.pre_mag /. 1000.0 in
  let w_sp = dvi.Dvi.postamble.Dvicommands.post_width
  and h_sp = dvi.Dvi.postamble.Dvicommands.post_height in
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
  let width = Misc.round (base_dpi *. w_in)
  and height = Misc.round (base_dpi *. h_in)
  and real_width = Misc.round (base_dpi *. w_in *. st.ratio)
  and real_height = Misc.round (base_dpi *. h_in *. st.ratio) in
  let fwidth = base_dpi *. w_in
  and fheight = base_dpi *. h_in in
  let (size_x, size_y) =
    if attr.crop then begin
      let sx = match attr.hmargin with
      | Px n -> Misc.round (fwidth +. 2.0 *. float_of_int n)
      | In f -> Misc.round (fwidth +. 2.0 *. base_dpi *. f)
      | _ -> assert false
      and sy = match attr.vmargin with
      | Px n -> Misc.round (fheight +. 2.0 *. float_of_int n)
      | In f -> Misc.round (fheight +. 2.0 *. base_dpi *. f)
      | _ -> assert false in
      (min attr.geom.Ageometry.width sx, min attr.geom.Ageometry.height sy)
    end else
      (attr.geom.Ageometry.width, attr.geom.Ageometry.height) in
  if all then
    begin
      let orig_x = Misc.round ((float size_x -. fwidth) *. 0.5)
      and orig_y = Misc.round ((float size_y -. fheight) *. 0.5) in
      st.base_dpi <- base_dpi;
      st.size_x <- size_x;
      st.size_y <- size_y;
      st.orig_x <- orig_x;
      st.orig_y <- orig_y;
    end;
  st.dvi_width <- real_width;
  st.dvi_height <- real_height;
  st.toc <- None;
;;

let init filename =
  let dvi =
    try Dvi.load filename
    with
    | Sys_error _ -> raise (Error (Printf.sprintf "cannot open `%s'" filename))
    | Dvi.Error s -> raise (Error (Printf.sprintf "%s: (Dvi) %s" filename s))
    | e ->
       raise (Error
                (Printf.sprintf "error while loading `%s': %s"
                   filename (Printexc.to_string e))) in
  let cdvi = Driver.cook_dvi dvi in
  let int = 0 in
  let float = 0. in
  let last_modified =
    try (Unix.stat filename).Unix.st_mtime
    with _ -> 0.0 in
  Gs.init_do_ps ();
  let npages =  Array.length dvi.Dvi.pages in
  let st =
    let npages = Array.length dvi.Dvi.pages in
    { filename = filename;
      dvi = dvi;
      cdvi = cdvi;
      num_pages =  npages;
      base_dpi = float;
      dvi_width = int;
      dvi_height = int;
      size_x = int;
      size_y = int;
      orig_x = int;
      orig_y = int;
      ratio = 1.0;
      page_stack = [];
      page_marks = [];
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
      toc = None;
      synchronize = true;
    } in
  init_geometry true st;
  attr.geom.Ageometry.width <- st.size_x;
  attr.geom.Ageometry.height <- st.size_y;
  st;;

let set_bbox st =
  Grdev.set_bbox (Some (st.orig_x, st.orig_y, st.dvi_width, st.dvi_height));;

let update_dvi_size all ?dx ?dy st =
  init_geometry all st;
  begin match dx with None -> () | Some z -> st.orig_x <- z end;
  begin match dy with None -> () | Some z -> st.orig_y <- z end;
  set_bbox st;;

(* Incremental drawing *)
let synchronize st =
  if st.synchronize then Grdev.synchronize()

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
  synchronize st;
  Busy.set (if st.cont = None then Busy.Free else Busy.Pause);;

let draw_bounding_box st =
  Misc.warning "Draw_bounding box";
  Grdev.set_color 0xcccccc;
  Grdev.fill_rect st.orig_x st.orig_y st.dvi_width 1;
  Grdev.fill_rect st.orig_x st.orig_y 1 st.dvi_height;
  Grdev.fill_rect st.orig_x (st.orig_y + st.dvi_height) st.dvi_width 1;
  Grdev.fill_rect (st.orig_x + st.dvi_width) st.orig_y 1 st.dvi_height;;

(* Input : a point in window coordinates, relative to the lower-left corner.
   Output : a point in document coordinates, relative to the upper-right corner.
   The output depends on the ratio st.ratio. *)
let document_xy st x y =
  (* x and y are relative to the lower-left corner. *)
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

(* User has selected a region with the mouse.
   We dump the corresponding characters. *)
let selection s = Grdev.cut s;;

let get_size_in_pix st = function
  | Px n -> n
  | In f -> Misc.round (st.base_dpi *. f)
  | _ -> assert false;;

let vmargin_size st = get_size_in_pix st attr.vmargin;;
let hmargin_size st = get_size_in_pix st attr.hmargin;;

(* The next four functions returns the position that correspond to the top,
   the bottom, the left, and right of the page. *)
let top_of_page = vmargin_size;;

let bottom_of_page st =
  attr.geom.Ageometry.height - st.dvi_height - vmargin_size st;;

let left_of_page = hmargin_size;;

let right_of_page st =
  attr.geom.Ageometry.width - st.dvi_width - hmargin_size st;;

(* The two following functions move the displayed part of the page while
   staying inside the margins. *)
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
    end in
  if st.orig_y <> new_orig_y then Some new_orig_y else None;;

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
    end in
  if st.orig_x <> new_orig_x then Some new_orig_x else None;;

let redraw ?trans ?chst st =
  (* Draws until the current pause_number or page end. *)
  (* The pauses and waits that appear before are ignored. *)
  Busy.set Busy.Busy;
  st.cont <- None;
  st.aborted <- false;
  begin
    try
      Grdev.continue ();
      Driver.clear_symbols ();
      if !bounding_box then draw_bounding_box st;
      let f =
        Driver.render_step st.cdvi st.page_number ?trans ?chst
          (st.base_dpi *. st.ratio) st.orig_x st.orig_y in
      if !pauses then begin
        let current_pause = ref 0 in
        try
          while
            try f () with
            | Driver.Wait sec ->
                if !current_pause = st.pause_number then
                  ignore (Grdev.sleep sec);
                true
            | Driver.Pause ->
                if !current_pause = st.pause_number then raise Driver.Pause
                else begin incr current_pause; true end
          do () done;
          if !current_pause < st.pause_number
          then st.pause_number <- !current_pause
        with
        | Driver.Pause -> st.cont <- Some f
      end else begin
        Misc.debug_endline("Pauses are disabled: overriding transitions!");
        Transimpl.sleep := (fun _ -> true); (* always breaks *)
        while try f () with Driver.Wait _ | Driver.Pause -> true
        do () done
      end
    with
    | Grdev.Stop -> st.aborted <- true
  end;
  synchronize st;
  Busy.set (if st.cont = None then Busy.Free else Busy.Pause);
  Misc.debug_stop "Page has been drawn\n";
;;

let thumbnail_limit = ref 5;;

Options.add
  "-thumbnail-scale"
  (Arg.Int (fun i -> thumbnail_limit := i))
  (Printf.sprintf
     "<int>: set the number of thumbnails per line\
     \n\t and column to <int>,\
     \n\t (the default number is %d)." !thumbnail_limit);;

let xrefs st =
  if st.frozen then
    begin
      Driver.scan_special_pages st.cdvi max_int;
      st.frozen <- false;
    end;
  st.dvi.Dvi.xrefs;;

let make_thumbnails st =
  let xnails =
    Hashtbl.fold
      (fun x p all -> if Misc.has_prefix "/page." x then p :: all else all)
      (xrefs st)
      [] in
  let page_nails =
    if xnails = [] then Array.init st.num_pages (fun p -> p) else
    let ucons x l =
      match l with
      | y :: _ when x = y -> l
      | _ -> x :: l in
    let rec unique = function
      | [] -> []
      | x :: l -> ucons x (unique l) in
    Array.of_list (unique (List.sort compare xnails)) in
  let num_nails = Array.length page_nails in
  let r_fit = int_of_float (ceil (sqrt (float_of_int num_nails))) in
  let r = min r_fit !thumbnail_limit in
  let pages = num_nails - 1 / r / r in
  let ist =
    { st with
      size_x = st.size_x / r;
      size_y = st.size_y / r;
      synchronize = false;
      orig_x = st.orig_x / r;
      orig_y = st.orig_y / r;
      dvi_width = st.dvi_width / r;
      base_dpi = st.base_dpi /. float r;
      dvi_height = st.dvi_height / r;
    } in
  let size_x = Graphics.size_x () in
  let size_y = Graphics.size_y () in
  let dx = size_x / r
  and dy = size_y / r in
  let all =
    Driver.with_active false
      (Array.map
         (fun p ->
           let chgvp s =
             {s with
	      Dvi.bkgd_prefs =
               {s.Dvi.bkgd_prefs with
	        Grdev.bgviewport =
                  Some {
                   Grdev.vx = 0;
                   Grdev.vy = size_y - dy;
                   Grdev.vw = dx;
                   Grdev.vh = dy;
                  }
               }
             } in
	   let without_pauses f x =
	     let p = !pauses in
	     try pauses := false; let v = f x in pauses := p; v
	     with x -> pauses := p; raise x in
	   without_pauses(redraw ?chst:(Some chgvp))
             {ist with page_number = p};
           (* Interrupt thumbnail computation in case of user interaction. *)
           begin try Grdev.continue() with
           | Grdev.Stop ->
               let gray = Graphics.rgb 200 200 200 in
               Grdev.with_color gray
                 (Graphics.fill_rect 0 (size_y - dy) (dx - 1)) (dy - 1)
           end;
           p, Graphics.get_image 0 (size_y - dy) dx dy;
         ))
         page_nails in
  let rolls = (Array.length all + r * r - 1) / r / r in
  let split =
    Array.init rolls
      (fun roll ->
        let first = roll * r * r in
        Thumbnails
          (r, Array.sub all first (min (r * r) (Array.length all - first)))) in
  st.toc <- Some split
;;

let make_toc st =
  try
    let refs = xrefs st in
    let first = Hashtbl.find refs "/toc.first" in
    let last = Hashtbl.find refs "/toc.last" in
    st.toc <- Some (Array.init (last - first + 1) (fun p -> Page (p + first)))
  with
  | Not_found -> ()
;;

let show_thumbnails st r page =
  let size_x = Graphics.size_x () in
  let size_y = Graphics.size_y () in
  let dx = size_x / r
  and dy = size_y / r in
  let pages = Array.length page / r / r in
  Array.iteri
    (fun p' (p, img) ->
       let x = size_x * (p' mod r) / r in
       let y = size_y * (p' / r) / r in
       Graphics.draw_image img x (size_y - y -dy);
       Grdev.H.area
         (Grdev.H.Href ("#/page." ^ string_of_int (p + 1))) x
         (size_y - y - dy) dx dy)
    page;
  (* To force the page under thumbnails to be redrawn *)
  st.aborted <- true
;;

let show_toc st =
  if st.toc = None then make_toc st;
  Grdev.clear_dev();
  Driver.clear_symbols();
  match st.toc with
  | None -> ()
  | Some rolls ->
      let n = Array.length rolls in
      if st.num = n then redraw st
      else
        let roll = st.num mod n in
        st.next_num <- succ st.num;
        begin match rolls.(roll) with
        | Page p -> redraw { st with page_number = p }
        | Thumbnails (r, page) -> show_thumbnails st r page
        end;
        synchronize st;
        st.aborted <- true
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
  try
    let p = int_of_string (Misc.get_suffix "/page." tag) in
    if p > 0 && p <= st.num_pages then p - 1 else default
  with Misc.Match ->
    try Hashtbl.find (xrefs st) tag
    with Not_found -> default;;

exception Link;;

(* View a link: call the appropriate viewer helping command. *)
let exec_xref link =
  let call command arg =
    Grdev.wait_button_up ();
    ignore (Launch.fork_process (Printf.sprintf "%s %s" command arg)) in
  match Misc.string_prefix ':' link with
  | "file:" ->
      if Misc.has_prefix "file://" link then call !browser link else
      begin try
        let fname, arguments =
          match Misc.split_string (Misc.get_suffix "file:" link)
                  (function '#' -> true | _ -> false) 0 with
          | [ fname; tag ] -> fname, ["-html"; tag ]
          | [ fname ] -> fname, []
          | _ -> Misc.warning ("Invalid link " ^ link); raise Link in
        if not (Sys.file_exists fname) then
          Misc.warning
            (Printf.sprintf
               "File %s is non-existent or not readable" fname) else
        match Misc.filename_extension fname with
        | ".dvi" ->
            let command = String.concat " " (Sys.argv.(0) :: arguments) in
            call command fname
        | ".html" | ".shtml" | ".htm" | ".shtm" ->
            call !browser link
        | ".pdf" -> call !pdf_viewer fname
        | ".ps" | ".eps" -> call !ps_viewer fname
        | ".bmp" | ".gif" | ".png" | ".jpg" | ".jpeg"
        | ".pbm" | ".pgm" | ".pnm" | ".tiff" | ".xpm" ->
            call !image_viewer fname
        | ".mpg" -> call !film_viewer fname
        (* In any other cases we call the pager program. *)
        | ".txt" | ".tex" | ".ftex" | ".sty" | ".pic"
        | ".ml" | ".scm" | ".c" | ".el" ->
            call !pager fname
        | _ ->
            Misc.warning
              (Printf.sprintf "Don't know what to do with link %s" link)
      with
      | Misc.Match -> assert false
      | Link -> () end
  | "http:" | "https:" | "ftp:" | "telnet:" ->
      call !browser link
  | _ ->
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
  Misc.warning "reloading DVI file";
  try
    Grdev.clear_usr1 ();
    st.last_modified <- reload_time st;
    let dvi = Dvi.load st.filename in
    let cdvi = Driver.cook_dvi dvi in
    let dvi_res = !dpi_resolution
    and mag = float dvi.Dvi.preamble.Dvicommands.pre_mag /. 1000.0 in
    let w_sp = dvi.Dvi.postamble.Dvicommands.post_width
    and h_sp = dvi.Dvi.postamble.Dvicommands.post_height in
    let w_in = mag *. ldexp (float w_sp /. dvi_res) (-16)
    and h_in = mag *. ldexp (float h_sp /. dvi_res) (-16) in
    let width = Misc.round (w_in *. st.base_dpi *. st.ratio)
    and height = Misc.round (h_in *. st.base_dpi *. st.ratio) in
    let npages =  Array.length dvi.Dvi.pages in
    st.dvi <- dvi;
    st.cdvi <- cdvi;
    st.num_pages <- npages;
    st.toc <- None;
    st.page_stack <- clear_page_stack npages st.page_stack;
    let npage = page_start (min st.page_number (st.num_pages - 1)) st in
    if npage <> st.page_number then
      begin
        st.pause_number <- 0;
        st.exchange_page <- st.page_number;
      end;
    set_page_number st npage;
    st.frozen <- true;
    st.aborted <- true;
    update_dvi_size false st;
    Gs.init_do_ps ();
    redraw ?trans:(Some Transitions.DirTop) st
  with x ->
    Misc.warning
      (Printf.sprintf "exception while reloading %s" (Printexc.to_string x));
    st.cont <- None;;

let changed st =
  reload_time st > st.last_modified;;

(* Go to the begining of the page. *)
let goto_page n st =
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
          n);
     true);
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
  let new_page = if npage > 0 then npage else -1 - npage in
  if new_page = st.page_number then redraw st
  else goto_page new_page st;;

let mark_page st =
  let marks =
    if List.length st.page_marks > 9
    then List.rev (List.tl (List.rev  st.page_marks))
    else st.page_marks in
  st.page_marks <- st.page_number :: marks

let goto_mark n st =
  try goto_page (List.nth st.page_marks n) st
  with Failure _ | Invalid_argument _ -> ()

let previous_slice st =
  print_string "#line 0, 0 <<<<>><<>>Next-Slice>> ";
  print_newline ()

let next_slice st =
  print_string "#line 0, 0 <<Previous-Slice<<>><<>>>> ";
  print_newline ()

(* goto page of hyperref h *)
let goto_href link st =
  let p =
    if Misc.has_prefix "#" link then
      let tag = Misc.get_suffix "#" link in
      find_xref tag st.page_number st
    else
      begin
        exec_xref link;
        st.page_number
      end in
  push_page true p st;
  Grdev.H.flashlight (Grdev.H.Name link);;

(* Go to hyperpage n if possible or page n otherwise *)
let goto_pageref n st =
  let tag = Printf.sprintf "#page.%d" n in
  let alt = if st.num > 0 then st.num - 1 else st.num_pages in
  let p = find_xref tag alt st in
  push_page true p st

let goto_next_page st =
  if st.page_number <> st.num_pages - 1 then goto_page (st.page_number + 1) st;;

let resize st ?dx ?dy x y =
  attr.geom <-
    { Ageometry.width = x;
      Ageometry.height = y;
      Ageometry.xoffset = Ageometry.No_offset;
      Ageometry.yoffset = Ageometry.No_offset;
    };
  if !autoscale then update_dvi_size true ?dx ?dy st;
  redraw st;;

let scale n st =
  let factor =
    if n > 0 then !scale_step ** float n
    else  (1. /. !scale_step) ** float (0 - n) in
    if !autoresize then
      begin
        let scale x = Misc.round (float x *. factor) in
        attr.geom.Ageometry.width <- scale st.size_x;
        attr.geom.Ageometry.height <- scale st.size_y;
        Grdev.close_dev ();
        let x, y =
          Grdev.open_dev (Printf.sprintf " " ^ Ageometry.to_string attr.geom)
        in
        attr.geom.Ageometry.width <- x;
        attr.geom.Ageometry.height <- y;
      end
    else
      begin
        let new_ratio = factor *. st.ratio in
        if new_ratio >= 0.02 && new_ratio < 50.0 then
          begin
            st.ratio <- new_ratio;
            let (cx, cy) = (st.size_x / 2, st.size_y / 2) in
            st.orig_x <- Misc.round (float (st.orig_x - cx) *. factor) + cx;
            st.orig_y <- Misc.round (float (st.orig_y - cy) *. factor) + cy;
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
    let goto_pageref st =
      goto_pageref st.num st
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
           * floating point rounding problem *)
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

    let redraw = redraw ?trans:(Some Transitions.DirNone) ?chst:None

    let toggle_active st =
      Driver.toggle_active(); redraw st

    let reload = reload
    let redisplay = redisplay

    let fullscreen st =
      let b = (st.fullscreen = None) in
      let (size_x, size_y), (dx, dy) =
        match st.fullscreen with
        | None ->
            let x = GraphicsY11.origin_x () in
            let y = GraphicsY11.origin_y () in
            st.fullscreen <-
              Some (x, y, st.size_x, st.size_y, (st.orig_x, st.orig_y));
            (* negative width and height mean fullscreen *)
            Grdev.reposition ~x:0 ~y:0 ~w:(-1) ~h:(-1),
            (0, 0);
        | Some (x, y, w, h, dxy) ->
            st.fullscreen <- None;
            Grdev.reposition ~x ~y ~w ~h,
            dxy in
      resize st ~dx ~dy size_x size_y;
      if b then center st

    let exit st = raise Exit
    let switch_edit_mode st =
      Grdev.E.switch_edit_mode ();
      redraw st
    let clear_image_cache st = (* clear image cache *)
      Grdev.clean_ps_cache ()
    let help st =
      ignore (
        Launch.fork_me
          (Printf.sprintf "-g %dx%d"
             attr.geom.Ageometry.width
             attr.geom.Ageometry.height)
        Config.splash_screen)

    let toggle_antialiasing st =
      Gs.toggle_antialiasing ()

    let scratch st =
      Scratch.draw ()
    let scratch_write st =
      Scratch.write ()

    let previous_slice = previous_slice
    let next_slice = next_slice
    let mark_page = mark_page
    let goto_mark st = goto_mark st.num st

    let make_thumbnails st =
       Launch.without_launching
         (fun () ->
            Busy.set Busy.Busy;
            make_thumbnails st;
            Busy.stop ();
            if not st.aborted then show_toc st)
         ()

    let show_toc st =
       Launch.without_launching show_toc st

    let ask_to_search =
      let prefill = ref "" in
      (fun message ->
         let minibuff =
           let nlines = 1 in
           let xc, yc = 2, 2 in
           let sx, sy = Graphics.text_size "X" in
           let wt, ht = Graphics.size_x () - 2 * xc, sy * nlines in
           let ncol = wt / sx in
           let bw = 0 in
           Gterm.make_term_gen
             Graphics.black (Graphics.rgb 180 220 220) (* Grounds colors *)
             bw Graphics.black (* Border *)
             (Graphics.rgb 220 150 120) (* Title color *)
             (Graphics.black) (* Cursor color *)
             xc yc (* Where on the screen *)
             ncol nlines (* Size in columns and lines *) in
         Gterm.draw_term minibuff;
         let re = Gterm.ask_prefill minibuff message !prefill in
         !Misc.forward_push_back_key_event '' GraphicsY11.control;
         prefill := re;
         re)

    let search_forward st =
      let re_string = ask_to_search "Search Forward (re): " in
      Misc.warning (Printf.sprintf "Search forward %s" re_string);
      ()

    let search_backward st =
      let re_string = ask_to_search "Search Backward (re): " in
      Misc.warning (Printf.sprintf "Search backward %s" re_string);
      ()
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
   'a', B.toggle_active;
   'A', B.toggle_antialiasing;
   'q', B.exit;
   '?', B.help;

   (* hjkl to move the page around *)
   'h', B.page_left;
   'j', B.page_down;
   'k', B.page_up;
   'l', B.page_right;

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
   'M', B.mark_page;
   'm', B.goto_mark;

   (* space, n, p, P, N to go on, or go back to next pause or page. *)
   ' ', B.next_pause;
   'n', B.next_page;
   '', B.next_page;
   'p', B.previous_page;
   '�', B.previous_page;
   'P', B.previous_pause;
   'N', B.next_pause;

   (* ^P, ^N in edit mode means previous- or next-slice *)
   '', B.next_slice;
   '', B.previous_slice;

   (* comma, ., g, to go to a page. *)
   ',', B.first_page;
   '.', B.last_page;
   'g', B.goto;

   (* r, Control-l, R, to redraw or reload. *)
   'r', B.redraw;
   'R', B.reload;
   '', B.redisplay;

   (* Control-f, c, to handle the advi window. *)
   '', B.fullscreen;
   'c', B.center;

   (* Searching: Control-s search forward, Control-r search backward. *)
   '', B.search_forward;
   '', B.search_backward;

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

   (* Edit mode. *)
   'e', B.switch_edit_mode;

   (* Scratching. *)
   's', B.scratch_write;
   'S', B.scratch;

   (* Thumbnails. *)
   'T', B.make_thumbnails;
   't', B.show_toc;
  ];;

bind_keys ();;

let main_loop filename =
  let st = init filename in
  (* Check if whiterun *)
  if Launch.whiterun () then begin
    Driver.scan_special_pages st.cdvi (st.num_pages - 1);
    Launch.dump_whiterun_commands ()
  end else begin
    Grdev.set_title ("Advi: " ^ Filename.basename filename);
    let x, y = Grdev.open_dev (" " ^ Ageometry.to_string attr.geom) in
    attr.geom.Ageometry.width <- x;
    attr.geom.Ageometry.height <- y;
    update_dvi_size true st;
    set_bbox st;
    if st.page_number > 0 && Gs.get_do_ps () then
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
      | Grdev.Click (pos, but, _, _) when Grdev.E.editing () ->
          begin match pos, but with
          | Grdev.Top_left, Grdev.Button1 -> B.previous_slice st
          | Grdev.Top_left, Grdev.Button2 -> B.reload st
          | Grdev.Top_left, Grdev.Button3 -> B.next_slice st
          | Grdev.Top_right, Grdev.Button1 -> B.previous_page st
          | Grdev.Top_right, Grdev.Button2 -> B.last_page st
          | Grdev.Top_right, Grdev.Button3 -> B.next_page st
          | _, _ -> ()
          end
      | Grdev.Click (Grdev.Top_left, _, _, _) ->
          if !click_turn_page then B.pop_page st
      | Grdev.Click (_, Grdev.Button1, _, _) ->
          if !click_turn_page then B.previous_pause st
      | Grdev.Click (_, Grdev.Button2, _, _) ->
          if !click_turn_page then B.pop_previous_page st
      | Grdev.Click (_, Grdev.Button3, _, _) ->
          if !click_turn_page then B.next_pause st
      | Grdev.Nil -> ()
    done with Exit -> Grdev.close_dev ()
  end;;
