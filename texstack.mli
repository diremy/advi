type 'a t = { mutable top: 'a; mutable stack: 'a list };;

val create : 'a -> 'a t;;

val push : 'a -> 'a t -> unit;;
val pop : 'a t -> unit;;
