exception Terminated
val kill : unit -> unit
val draw : string -> int -> int -> unit
val add_headers : string list -> unit
val newpage : string list -> int -> float -> int -> int -> unit
val flush : unit -> unit
val toggle_antialias : unit -> unit
val current_x : int ref
val current_y : int ref
