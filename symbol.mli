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

(* to_ascii returns a string representing the symbols that are in zone 'zone'. *)
(* Elements are filtered according to the first argument. *)
val to_ascii   : (int*int*int*int) -> set -> string
val to_escaped : (int*int*int*int) -> set -> string

(* Says if the symbol is in zone x1,y1 -> x2,y2. *)
val inzone : int -> int -> int -> int -> symbol -> bool
