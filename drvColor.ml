open Stack;;
open Special;;

class state = object
  val color_state = 
    let st = Stack.create () in
    Stack.push 0 st;
    st

  method push_color c = Stack.push c color_state
  method pop_color = try ignore (Stack.pop color_state) with Empty -> ()
  method color = Stack.top color_state
end

let special st s =
  match split_string s 0 with
  | "color" :: "push" :: args ->
      st#push_color (Dvicolor.parse_args args)
  | "color" :: "pop" :: [] ->
      st#pop_color
  | _ -> ();;

