let debug = Options.debug ~label: "grsubwindow" "-debug-grsubwindow" "Debug GrSubwindow module";;

exception Error of string

class type subwindow = object
  method parent : GPack.fixed
  method create : init: (GBin.event_box -> unit) ->
      x:int -> y:int -> width:int -> height:int -> GBin.event_box
  method destroy : GBin.event_box -> unit
  method move : GBin.event_box -> x:int -> y:int -> unit
  method resize : GBin.event_box -> width:int -> height:int -> unit
  method map : GBin.event_box -> unit
  method unmap : GBin.event_box -> unit
end
;;

type mapped_state = {
    x : int;
    y : int;
    mutable mapped : bool
  } 

class t (fix : GPack.fixed) = object
  val children = Hashtbl.create 17

  method parent = fix
  method create ~init ~x ~y ~width ~height =
    if width = 0 && height = 0 then 
      raise (Error "subwindow#create: zero size?")
    else
      let w = GBin.event_box ~width ~height ~show: true () in
      let id = ref None in
      id := Some (w#misc#connect#draw ~callback: (fun _ ->
	(init w : unit);
	begin match !id with
	| Some id -> w#misc#disconnect id
	| None -> ()
	end));
      fix#put (w :> GObj.widget) ~x ~y;
      Hashtbl.add children w {x=x; y=y; mapped=true};
      w

  method destroy (emb : GBin.event_box) =
    try
      Hashtbl.remove children emb;
      fix#remove (emb :> GObj.widget);
      emb#destroy ()
    with
    | Not_found -> raise (Error "#destroy: argument is not a subwindow")

  method move (emb : GBin.event_box) ~x ~y = 
    try
      let state = Hashtbl.find children emb in
      if state.mapped then begin (* already mapped *)
	debug "move mapped window";
	let ostate = Hashtbl.find children emb in
	if (ostate.x,ostate.y) <> (x,y) then begin
	  Hashtbl.replace children emb {x=x;y=y;mapped=true};
	  fix#move (emb :> GObj.widget) ~x ~y
	end
      end else begin (* if it is not mapped, we map! *)
	debug "put unmapped window";
	fix#move (emb :> GObj.widget) ~x ~y;
	emb#misc#map ();
	Hashtbl.replace children emb {x=x; y=y; mapped=true}
      end
    with Not_found -> raise (Error "#move: argument is not a subwindow")

  method resize (ebox : GBin.event_box) ~width ~height =
    if Hashtbl.mem children ebox then 
      ebox#misc#set_geometry ~width ~height ()
    else raise (Error "#resize: argument is not registered")

  method map (ebox : GBin.event_box) = 
    try
      let state = Hashtbl.find children ebox in
      state.mapped <- true;
      fix#move (ebox :> GObj.widget) ~x: state.x ~y: state.y;
      ebox#misc#map ()
    with
    | Not_found -> raise (Error "#map: argument is not registered")

  method unmap (ebox : GBin.event_box) = 
    try
      let state = Hashtbl.find children ebox in
      if state.mapped then begin
	state.mapped <- false;
	ebox#misc#unmap ()
      end
    with Not_found -> raise (Error "#unmap: argument is not registered")
end

class dummy = (object
  method parent = raise Exit
  method create ~init ~x ~y ~width ~height = raise Exit
  method destroy (emb : GBin.event_box) = raise Exit
  method move (emb : GBin.event_box) = raise Exit
  method resize (emb : GBin.event_box) = raise Exit
  method map (ebox : GBin.event_box) = raise Exit
  method unmap (ebox : GBin.event_box) = raise Exit
end : subwindow)

