(* simple timeout handler *)

module Timeout = struct
  type t = { at : float;
	     callback : (unit -> unit);
	     stamp : int}
  let compare t t' = compare t.at t'.at
end

module TimeoutSet = Set.Make(Timeout);;
open Timeout

type t = Timeout.t

let set = ref TimeoutSet.empty
let stamp = ref 0

let rec callback () =
  try while 
      let min = TimeoutSet.min_elt !set in
      let now = Unix.gettimeofday () in
      let wait = min.at -. now in
      if wait <= 0.0 then begin
  	set := TimeoutSet.remove min !set;
  	min.callback ();
  	true
      end else begin
  	ignore (Unix.setitimer Unix.ITIMER_REAL {Unix.it_interval= 0.0;
  						 Unix.it_value= wait});
  	false
      end
    do () done
  with
  | Not_found -> ()
;;
  
let init () =
  (* Already exists itimer's handle is disabled! *)
  ignore (Sys.signal Sys.sigalrm 
	    (Sys.Signal_handle (fun _ -> callback ())));
  set := TimeoutSet.empty;
  stamp := 0
;;

let add sec cbk =
  if sec < 0.0 then raise (Invalid_argument "Internval.add_timeout");
  let start = Unix.gettimeofday () in
  let at = start +. sec in
  let mystamp = !stamp in
  incr stamp;
  let timeout = { at= at; callback= cbk; stamp= mystamp} in
  set := TimeoutSet.add timeout !set;
  callback ();
  timeout
;;  

let remove timeout =
  if not (TimeoutSet.mem timeout !set) then raise Not_found;
  set := TimeoutSet.remove timeout !set;
  callback ()
;;