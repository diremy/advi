(* bit modified version of Stack *)
type 'a t = { mutable top: 'a; mutable stack: 'a list }

let create bottom = { top= bottom; stack= [] };;

let push v st = 
  st.stack <- st.top :: st.stack;
  st.top <- v
;;

let pop st = 
  match st.stack with
  | [] -> ()
  | v::vs -> st.top <- v; st.stack <- vs
;;
