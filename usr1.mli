val signal : ([> `widget], unit -> unit) GtkSignal.t;;

(* set the destination window of usr1 signal *)
val set : [> `widget] Gtk.obj -> unit;;
