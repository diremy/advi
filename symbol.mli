(* Symbols information. *)
type symbol = { color   : int ; 
	       locx    : int ; 
	       locy    : int ;
	       voffset : int ;
	       hoffset : int ;
	       width   : int ;
	       height  : int ;
	       code    : int ;
	       fontname : string ;
	       fontratio : float }

(* Sets of symbol. *)
type set

(* Empty set. *)
val empty_set : pagewidth:int -> pageheight:int -> set

(* Add an element. *)
val add : symbol -> set -> unit

(* to_ascii returns a string representing the symbols that are in the set. *)
val to_ascii   : set -> string
val to_escaped : set -> string

(* Gives a copy of set where only symbol in zone x1 y1 x2 y2 are kept. *)
val inzone : int -> int -> int -> int -> set -> set

(* Idem where but the resulting set is time-convex . *)
val intime : int -> int -> int -> int -> set -> set
