class state : DrDvi.state -> DrColor.state -> DrProc.state -> DrTpic.state ->
object
  method dvi : DrDvi.state
  method color : DrColor.state
  method proc : DrProc.state
  method tpic : DrTpic.state
end
;;

(*
val clear_symbols : state -> unit
*)
(*
val add_char : state -> int -> int -> int -> DrDvi.Symbol.glyph -> unit
val add_line : state -> int * string option -> unit
val add_blank : int -> state -> int -> unit
val add_rule : state -> int -> int -> int -> int -> unit
*)

val add_special : string -> (state -> string -> unit) -> unit
val special : state -> string -> unit

val eval_dvi_command : state -> Dvi.command -> unit
val eval_command : state -> Dvi.command -> unit
val render_step :
  GrDev.dvidevice ->
  DrDvi.cooked_dvi -> int -> float -> int -> int -> unit -> bool

val scan_special_pages : 'a -> 'b -> unit
val toggle_active : unit -> unit
