(* Wait special *)
open Special;;
open Texstack;;
open DrProc;;

exception Pause;;
exception Wait of float;;

let special st s =
  let records = get_records s in
  let second =
    try parse_float (List.assoc "sec" records)
    with
    | Not_found -> raise Pause
    | Failure _ -> raise (Failure "wait: invalid special") in
  (* Wait is treated like Pause, as an exception *)
  if st#proc.visible then raise (Wait second);;
(*
  st.checkpoint <- 0;;
*)
