(* USR1 signal as Gtk signal *)

let usr1 = Sys.sigusr1;;

let signal = 
  { GtkSignal.name = "usr1"; 
    GtkSignal.classe= `widget; 
    GtkSignal.marshaller = GtkSignal.marshal_unit }
;;

let set w =
  Sys.set_signal usr1
    (Sys.Signal_handle
       (fun _ -> prerr_endline "usr1";
	 GtkSignal.emit_unit w ~sgn: signal))
;;
