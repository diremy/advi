(* Symbols information. *)
type 'g symbol =
    { color   : int ; 
      locx    : int ; 
      locy    : int ;
      voffset : int ;
      hoffset : int ;
      width   : int ;
      height  : int ;
      code    : int ;
      fontname  : string ;
      fontratio : float ;
      glyph   : 'g option }

(* Sets of symbol. *)
type 'g set

(* Empty set. *)
val empty_set : int -> int -> 'g set

(* Rules, spaces and positions are symbols with special fontnames *)
val rulename  : string
val spacename : string
val linename : string

(* Add an element, imperative. *)
val add : 'g symbol -> 'g set -> unit

(* to_ascii returns a string representing the symbols that are in the set. *)
(* Could be done with a pretty printer... *)
val to_ascii   : 'g set -> string
val to_escaped : 'g set -> string

(* Gives a copy of set where only symbols in zone x1 y1 x2 y2 are kept. *)
val inzone : int -> int -> int -> int -> 'g set -> 'g set

(* Idem where but the resulting set is time-convex (intermediate symbols are also kept). *)
val intime : int -> int -> int -> int -> 'g set -> 'g set

(* Iterates function ff over the set of symbols. *)
val iter : ('g symbol -> unit) -> 'g set -> unit

val lines : int -> int -> 'g set -> int * int * string * string
