let debug = Options.debug ~label: "grembed" "-debug-grembed" "Debug GrEmbed module";;

exception Error of string

type app_mode = 
  | Sticky      (* always on screen *)
  | Persistent  (* only on the page which created it, but kept alive *)
  | Respawn     (* respawn apps for each page visit *)
;;

type geom = { width: int; height: int; x: int; y: int }

type process = {
    name : string;
    mode : app_mode;
    pid : int;
    ebox : GBin.event_box option;
    geom : geom
  }
;;

class t (subwin : GrSubwindow.t) = object (self)
  val embeds = Hashtbl.create 17

  method private launch_wm_managed ~name ~mode ~geom command ~cont =
    if mode = Persistent then raise (Error "wm managed embed cannot be persistent");
    let x = geom.x in
    let y = geom.y - geom.height in
    let width = geom.width in
    let height = geom.height in
    let window = subwin#parent#misc#window in
    let opt_geometry, opt_x, opt_y =
      let px, py =
  	let (ww, wh, wx, wy) = GrMisc.get_root_geometry window in
  	wx + x, wy + y
      in
      Printf.sprintf "%dx%d+%d+%d" width height px py,
      string_of_int px,
      string_of_int py 
    in
    let command =
      List.fold_right (fun (p,rep) st ->
	Misc.string_replace p rep st) 
	[ "!w", string_of_int width;
	  "!h", string_of_int height;
	  "!x", opt_x;
	  "!y", opt_y;
	  "!g", opt_geometry ]
	command
    in
    debug (Printf.sprintf "launching %s: %s" name command);
    let proc = 
      { name= name; 
	mode= mode;
	pid= Launch.fork_process command;
	ebox= None;
	geom= geom }
    in
    cont proc

  method private launch_embedded ~name ~mode ~geom command ~cont =
    let x = geom.x in
    let y = geom.y - geom.height in
    let width = geom.width in
    let height = geom.height in
    let map ebox =
      let window = ebox#misc#window in
      try
	let px = 0 and py = 0 in
	let opt_geometry = Printf.sprintf "%dx%d+%d+%d" width height px py in
	let xid_string = Int32.to_string (Gdk.Window.get_xwindow window) in
	let command =
	  List.fold_right (fun (p,rep) st ->
	    Misc.string_replace p rep st) 
	    [ "!p", xid_string;
	      "!w", string_of_int width;
  	      "!h", string_of_int height;
  	      "!x", string_of_int px;
  	      "!y", string_of_int py;
  	      "!g", opt_geometry ]
  	    command
  	in
	debug (Printf.sprintf "launching %s: %s" name command);
  	let proc = 
  	  { name= name;
  	    mode= mode;
  	    pid= Launch.fork_process command;
  	    ebox= Some ebox;
  	    geom= geom }
  	in
  	cont proc
      with e -> subwin#destroy ebox; raise e
    in
    ignore (subwin#create ~init: map ~x ~y ~width ~height)

  method launch ~name ~mode ~x ~y ~width ~height command =
    (*** !x commands
      !p : embedding target window id (in digit)

        If !p is not specified, the applications will be treated by WM.
        (If they are X apps, of course...)

      !g : geometry like 100x100+20+30
      !w : width  of the target window in pixel
      !h : height of the target window in pixel
      !x : x of the application against the root
      !y : y of the application against the root
      
      Why "!"?  '\' is for TeX. "%" is for TeX. "$" is for TeX...
    ***)
    let geom = {x=x; y=y; width=width; height=height} in
    try
      
      (* look whether it is already running *)
      let proc = Hashtbl.find embeds name in

      (* we cannot change the mode *)
      if proc.mode <> mode then
        raise (Error (Printf.sprintf 
	      "#launch: embed %s is already running in different mode" name));
      match proc.ebox with
      |	None -> 
          (* if it is wm managed embed, we cannot resize and move *)
	  if proc.geom <> geom then 
	    raise (Error (Printf.sprintf 
               "#launch: running wm managed embed %s cannot be moved/resized" name));
      |	Some ebox ->
          (* we cannot resize it *)
	  if (width,height) <> (proc.geom.width, proc.geom.height) then
            raise (Error (Printf.sprintf 
	      "#launch: running embed %s cannot be resized" name));
          (* if it exists and in the same mode, we just map it *)
	  debug (Printf.sprintf "moving %s: %s" name command);
	  subwin#move ~x ~y:(y-height) ebox

    with Not_found ->
      (* check we can really embed it or not *)
      (* if !p does not present, we cannot embed it! *)
      (* If there is no !p, the application geometry will be treated
  	 by the WM. In such cases, we try to fix the geometry
  	 so that it is against the root. *)
      let embedable = 
  	let command' = Misc.string_replace "!p" "dummy" command in
  	command <> command'
      in
      (if embedable then self#launch_embedded 
      else self#launch_wm_managed) 
	~name ~mode ~geom command 
	~cont: (fun proc -> Hashtbl.add embeds name proc)

  method private lookup m name =
    try
      Hashtbl.find embeds name
    with
    | Not_found -> raise (Error (Printf.sprintf "#%s: no such embed: %s"
				   m name))

  method kill name = 
    let proc = self#lookup "kill" name in
    begin match proc.ebox with
    | Some ebox -> subwin#destroy ebox
    | None -> ()
    end;
    Launch.kill ~signal: 9 proc.pid;
    Hashtbl.remove embeds name

  method unmap name =
    let proc = self#lookup "unmap" name in
    match proc.ebox with
    | Some w -> subwin#unmap w
    | None -> raise (Error "#unmap: wm managed embed is not unmappable")

  method map name =
    let proc = self#lookup "map" name in
    match proc.ebox with
    | Some w -> subwin#map w
    | None -> raise (Error "#map: wm managed embed is not mappable")

  method move name ~x ~y =
    let proc = self#lookup "move" name in
    begin match proc.ebox with
    | Some ebox -> subwin#move ebox ~x ~y
    | None -> raise (Error "#move: wm managed embed is not movable")
    end

  method iter f =
    Hashtbl.iter (fun name proc -> f proc) embeds

  method destroy () = self#iter (fun proc -> self#kill proc.name)
end
