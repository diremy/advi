class type state = object
  method dvi : DrDvi.state
  method color : DrColor.state
  method proc : DrProc.state
end
;;

val fnt : #state -> int -> unit
val put : #state -> int -> unit
val set : #state -> int -> unit
val put_rule : #state -> int -> int -> unit
val set_rule : #state -> int -> int -> unit

