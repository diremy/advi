open Tk;;

let opencamltk () =
 Arg.parse keywords (fun _ -> ()) "";
 opentk_with_parsed_args ();; 

let main_window = opencamltk ();;

let button =
  Button.create main_window [Text "Hello World!"];;

let rec hello_cb () =
  Button.configure button [Command bonjour_cb; Text "Hello World!"]

and bonjour_cb () =
  Button.configure button [Command hello_cb; Text "Bonjour!"];;

let start () =
  Button.configure button [Command bonjour_cb];
  pack [button] [];
  mainLoop ();;

start ();;
