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


let antialias =
  Options.flag false
    "-A"
    "Set Postscript antialiasing (default is unset)";;

let pstricks =
  Options.flag false
    "-pstricks"
    "Show moveto";;

let showps = ref false;;

Options.add
  "--showps" (Arg.Set showps)
  "\tPrint a copy of Postscript sent to gs to stdout";;

let pspage = ref 0;;

(* constants *)
(** ack_string is an arbitrary string that would not be ``naturally''
   generated by gs. Xdvi used characters \347\310\376 in octet which is
   \231\200\254 in decimal.
   I don't know why... to test whether gs is new or old *)
let ack_string = "\231\200\254\n";;
let pos_string = "dvi";;
let err_string = "Error:";;
let current_x = ref 0;;
let current_y = ref 0;;

let parse_pos s =
  let c = String.index s ',' in
  let bc = s.[3] in
  let b =
    if bc = 'a' then true else if bc = 'r' then false
    else assert false in
  let left = String.sub s 4 (c-4) in
  let right = String.sub s (c+1) (String.length s -c-1) in
  let int_of_floatstring f = int_of_float (float_of_string f +. 0.5) in
  b, int_of_floatstring left, int_of_floatstring right;;

let ack_request =
  String.concat ""
    [ "flushpage ("; ack_string; ") print flush "; ];;

let timeout = 3.;;

exception Timeout;;
exception Error;;

exception Terminated;;
exception Retry;;

let x11alpha = "x11alpha";;
let x11 = "x11";;
type graphical =
    { display : int;
      window  : int32 (* GraphicsY11.window_id *) ; (* Window identifier. *)
      pixmap  : int32 (* GraphicsY11.window_id *) ; (* Pixmap identifier. *)
      width  : int ; (* geometry of the window. *)
      height : int ;
      bwidth  : int ; (* geometry of the backing store. *)
      bheight : int ;
      xdpi : float ; (* x resolution *)
      ydpi : float ; (* y resolution *)
      x    : int   ; (* x offset in pixels
                          (coordinates of the upper left corner) *)
      y    : int   ; (* y offset *)
    };;

exception Killed of string;;

let rec select fd_in fd_out fd_exn timeout =
  (* dirty hack: Graphics uses itimer internally! *)
  let start = Unix.gettimeofday () in
  try
    Unix.select fd_in fd_out fd_exn timeout
  with
    Unix.Unix_error (Unix.EINTR, _, _) as exn ->
      let now = Unix.gettimeofday () in
      let remaining = start +. timeout -. now in
      if remaining > 0.0 then select fd_in fd_out fd_exn timeout
      else [], [], []
;;

class gs () =
  let gr =
    { display = 0;
      window = GraphicsY11.window_id ();
      pixmap = GraphicsY11.bstore_id ();
      width = Graphics.size_x ();
      height = Graphics.size_y ();
      bwidth = GraphicsY11.bsize_x ();
      bheight = GraphicsY11.bsize_y ();
      xdpi = 72.0;
      ydpi = 72.0;
      x = 0;
      y = 0;
    }  in
  let dpi = 72 (* unite utilise par dvi? *) in
  let command = Config.gs_path in
  let command_args =
    [|
      "-dNOPLATFONTS"; "-dNOPAUSE";
      "-sDEVICE="^(if !antialias then x11alpha else x11);
      "-q";
      "-dSAFER";
      "-";
    |] in

  (* Set environment so that ghostscript writes in our window. *)
  let int32_string k x =
    let mask = Int32.of_int 0x11 in
    let s = String.create k in
    for i = 0 to k-1 do
      let b = Int32.to_int (Int32.logand (Int32.shift_left x (i * 8)) mask) in
      s.[k -1 - i] <- Char.chr b;
    done;
    s in
  let int32_string x = Int32.format "%u" x in

  let _ =
    Unix.putenv "GHOSTVIEW"
      (Printf.sprintf "%s %s "
         (int32_string gr.window)
         (if !Options.global_display_mode then "" else int32_string gr.pixmap)
      ) in

  let iof = int_of_float and foi = float_of_int in
  let lx = iof ( (foi (gr.x * dpi)) /. gr.xdpi)
  and ly = iof ( (foi (gr.y * dpi)) /. gr.ydpi)
  and ux = iof ( (foi ((gr.x + gr.bwidth)  * dpi)) /. gr.xdpi )
  and uy = iof ( (foi ((gr.y + gr.bheight) * dpi)) /. gr.ydpi)

  in

  (* Set ghostscript property. *)
  let content = Printf.sprintf "%s %d %d %d %d %d %f %f %d %d %d %d"
      "0" (* no backing pixmap for the window *)
      0   (* Rotation : 0 90 180 270 *)
      lx ly ux uy
      (* lower-left x y , upper-right x y :
         Bounding box in default user coordinates. *)
      gr.xdpi gr.ydpi (* Resolution x y. *)
      0 0 0 0 (* Margins left, bottom, top, right. *)
  in

  let _ =
    begin
      try GraphicsY11.set_named_atom_property  "GHOSTVIEW"  content;
      with x -> Misc.fatal_error "Cannot set ``GHOSTVIEW'' property"
    end;
  in

  (* Ignore signal SIGPIPE. *)
  let _ =  Unix.sigprocmask Unix.SIG_BLOCK [13] in

  let lpd_in, lpd_out = Unix.pipe () in
  let rpd_in, rpd_out = Unix.pipe () in
  let leftout = Unix.out_channel_of_descr lpd_out in
  let rightin = Unix.in_channel_of_descr rpd_in in
  let close_all () =
    let tryc f x = try f x with _ -> () in
    tryc close_out leftout;
    tryc close_in rightin;
    tryc Unix.close lpd_in;
    tryc Unix.close rpd_in
  in
  let pid =
    Unix.create_process command command_args lpd_in rpd_out
      (* Unix.stdout *) Unix.stderr
  in
  object (self)
    val pid = pid
    val mutable ack = 0
    method gr = gr

    method ack_request =
      try
        ack <- ack + 1;
        self # line ack_request;
        flush leftout;
        self # ack;
      with Killed s ->
        self # kill;
        Misc.warning s;
        raise Terminated
      | exn ->
          Misc.warning (Printexc.to_string exn);
          self # kill; 
          raise Terminated

    method ack =
      if ack > 0 then
        begin
          let s =
            try input_line rightin
            with End_of_file ->
              match select [ rpd_in ] [] [] 1.0 with
              | [], _, _ ->
                  begin match Unix.waitpid [ Unix.WNOHANG ] pid with
                  | x, Unix.WEXITED y when x > 0 ->
                      raise (Killed "gs exited")
                  | 0, _ ->
                      raise (Killed "gs alive but not responding")
                  | _, _ ->
                      raise (Killed "gs in strange state")
                  end
              | _, _, _ ->
                  input_line rightin  in
          if Misc.has_prefix s ack_string
          then ack <- ack -1
          else if Misc.has_prefix pos_string s then
            begin
              try
                let b, y, x = parse_pos s in
                let y = (if b then gr.height else 0) + y in
                if !pstricks then
                  begin
                    Printf.fprintf stderr "<-- %s %d %d"
                      (if b then "-" else "+") x y;
                    prerr_newline();
                  end;
                current_x := x; current_y := y
              with
              | Not_found | Failure _ -> prerr_endline s
            end
          else if Misc.has_prefix err_string s then
            begin
              prerr_endline s;
              raise (Killed "Error in Postscript");
            end
          else prerr_endline s;
          self # ack
        end;

    method kill =
      try
        Unix.kill pid Sys.sighup; 
        let _, _ = Unix.waitpid [] pid in
        close_all ();
      with Unix.Unix_error (_, _, _) -> ()

    method line l =
      try
        output_string leftout l;
        output_char leftout '\n';
        if !showps then print_endline l;
      with x ->
        prerr_endline  (Printexc.to_string x);
        self # kill;
        prerr_endline "GS Terminated";
        flush stderr

    method sync =
      try self # ack_request;
      with
        Killed s ->
          self # kill;
          Options.dops := false;
          prerr_endline s;
          prerr_endline "Continuing without gs\n";
          flush stderr

    method ps b =
      List.iter self#line b;

    method load_header (b, h) =
      self # line
        (if b then h else String.concat "" [ "("; h; ") run"; ])

  end;;

let texbegin = "TeXDict begin";;
let texend = "flushpage end";;
let moveto x y = Printf.sprintf "%d %d moveto" x y;;

let texc_special_pro gv =
  let l = [ "texc.pro"; "special.pro" ] in
  try
    let l' = Search.true_file_names [] l in
    if List.length l = List.length l' then List.map (fun s -> false, s) l'
    else raise Not_found
  with
    Not_found ->
      Misc.warning "Cannot find Postscript prologues: texc.pro or special.pro";
      Misc.warning "Continuing without Postscript specials";
      gv # kill;
      Options.dops := false;
      []
;;

class gv =
  object (self)
    val mutable dirtypage = true
    val mutable process : gs option = None
    val mutable dpi = 72
    val mutable mag = 1.
    val mutable xorig = 0
    val mutable yorig = 0
    val mutable sync = true
    val mutable headers = []
    method line =
      try
        self # process # line
      with Terminated ->
        process <- None;
        raise Terminated
    method moveto x y =
      let x' = xorig + x in
      let y' = y - yorig  in
      if !pstricks then
        begin
          Printf.fprintf stderr "--> %d %d" x' y';
          prerr_newline();
        end;
      moveto x' y'
    method check_size =
      begin
        match process with
          None -> ()
        | Some gs ->
            let gr = gs # gr in
            let size_x = GraphicsY11.bsize_x () in
            let size_y = GraphicsY11.bsize_y () in
            if size_x <> gr.bwidth || size_y <> gr.bheight
            then self # kill;
      end;

    method sync =
      if not sync then
        begin
          match process with
            Some p -> p # sync; sync <- true
          | None -> ()
        end

    method add_headers l =
      if headers = [] then headers <- texc_special_pro self;
      match
        List.filter
          (function _, s -> List.for_all (function _, s' -> s <> s') headers)
          l
      with
      | [] -> ()
      | l ->
          headers <- headers @ l;
          match process with
          | Some gs ->
              gs # line "grestore SI restore";
              List.iter gs # load_header l;
              gs # line "/SI save def gsave";
          | None -> ()

    method newpage l sdpi m x y =
      self # check_size;
      let l' = List.filter (fun s -> not (List.mem s headers)) l in
      let gs = self # process in
      if l' <> [] then headers <- headers @ l';
      dpi <- sdpi;
      mag <- m;
      xorig <- x;
      yorig <- (gs # gr).height - y;
      if !pspage > 0 then if !showps then print_endline "showpage";
      incr pspage;
      if !showps then
        print_endline (Printf.sprintf "%%%%Page: %d %d\n" !pspage !pspage);
      gs # line "\n%% Newpage\n";
      gs # line "grestore";
      if l' <> [] then gs # line "SI restore";
      gs # line
        (Printf.sprintf
           "TeXDict begin %d %d div dup /Resolution X /VResolution X end"
           dpi dpi);
      gs # line
        (Printf.sprintf
           "TeXDict begin /DVImag %f def end"
           mag);
      List.iter gs # load_header l';
      if l' <> [] then gs # line  "/SI save def";
      gs # line "gsave";
      gs # sync


    method process =
      match process with
      | None ->
          if not !Options.dops then raise Terminated;
          let gs = new gs () in
          if headers = [] then headers <-  texc_special_pro self;
          (* should take matrix as a parameter ? *)
          if !showps then print_endline "%!PS-Adobe-2.0\n%%Creator: advi\n%!";
          gs # line "[1 0 0 -1 0 0] concat";
          List.iter (gs # load_header) headers;
          gs # line "/floatstring 20 string def";
          gs # line "{ floatstring cvs print } /printfloat def";
          gs # line "{ floatstring cvs print } /printcp def";
          gs # line "TeXDict begin @landscape end";
          gs # line "/SI save def gsave";
          process <- Some gs;
          gs
      | Some gs -> gs

    method send b  =
      self # check_size;
      self # process # ps b;
      sync <- false

    method def b =
      self # send [ b ]

    method ps b (x:int) (y:int) =
      self # send [ texbegin; self # moveto x y; b; texend ];
      sync <- false

    method special b (x:int) (y:int) =
      self # send [ texbegin; self # moveto x y; 
                    "@beginspecial @setspecial";
                    b;
                    "@endspecial"; texend;
                  ] ;
      sync <- false

    method kill =
      if !showps then print_endline "showpage";
      match process with
      | None -> ()
      | Some gs ->
          gs # kill;
          process <- None
  end
;;

let gv = new gv;;

(* exported functions *)

let kill () = gv # kill;;
let draw s x y =
  if !Options.dops then
    try gv#ps (Misc.get_suffix  "ps: " s) x y  with Misc.Match ->
      try gv#special (Misc.get_suffix  "\" " s) x y  with Misc.Match ->
        try gv#def (Misc.get_suffix  "! " s) with Misc.Match -> ()
;;

let add_headers = gv#add_headers;;
let newpage = gv#newpage;;
let flush () =
  if !Options.dops then
    try  gv#sync
    with Terminated ->
      Misc.warning "Continuing without Postscript";
      gv#kill;
      Options.dops := false;;

let toggle_antialias () =
  antialias := not !antialias;
  kill ();;
