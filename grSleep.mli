type state = FINISHED | INTERRUPTED;;

val break_all_timeouts : unit -> unit

val f : sec: float -> cont: (state -> unit) -> unit
