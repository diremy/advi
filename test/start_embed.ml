open Tk;;

let opencamltk () =
 Arg.parse keywords (fun _ -> ()) "";
 opentk_with_parsed_args ();; 

let main_window = opencamltk ();;

let button =
  Button.create main_window [Text "Hello World!"];;

let start () =
  pack [button] [];
  mainLoop ();;

start ();;
