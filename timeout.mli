(* simple timeout handler *)

type t

val init : unit -> unit
val add : float -> (unit -> unit) -> t
val remove : t -> unit
