let ask_to_launch command command_invocation =
  let title = Printf.sprintf "Active-DVI alert for %s" command in
  let window = GWindow.dialog ~title
      ~position: `CENTER
      ~modal: true
      () 
  in
  let l1 = GMisc.label ~text: "Attempt to launch the following command"
      ~packing: window#vbox#add ()
  in
  let l2 = GMisc.label ~text: "Do you want to execute it?"
      ~packing: window#vbox#add ()
  in
  ()
;;
  
  
      
