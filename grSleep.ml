type state = FINISHED | INTERRUPTED;;

(* returns false if sleep is fully performed. returns true if interrupted *)

let timeouts = Hashtbl.create 13;;

let end_timeout state id =
  let cont = Hashtbl.find timeouts id in
  Hashtbl.remove timeouts id;
  GMain.Timeout.remove id;
  cont state;
;;

let break_all_timeouts () =
  Hashtbl.iter (fun k v -> end_timeout INTERRUPTED k) timeouts
;;

let f ~sec:n ~cont =
  let idref = ref None in
  let cbk () = 
    end_timeout FINISHED 
      (match !idref with Some id -> id | None -> raise (Failure "GrSleep.f"));
    true
  in
  let id = GMain.Timeout.add ~ms: (truncate (n *. 1000.0)) ~callback: cbk in
  idref := Some id;
  Hashtbl.add timeouts id cont;
;;
