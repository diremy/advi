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

(* Sets of symbol and sets of symbol names. *)
type set
type named_set

(* Empty set. *)
val empty_set : set

(* Add an element. *)
val add : symbol -> set -> set

val symbol_name : symbol -> string  
val find_names : set -> named_set

(* Filters a symbol_set *)
val filters : (symbol -> bool) -> set -> set

(* to_ascii takes a named_set and returns a string representing the named symbols. *)
val to_ascii : named_set -> string
val to_escaped : named_set -> string

(* Says if the symbol is in zone x1-x2 y1-y2. *)
val inzone : int -> int -> int -> int -> symbol -> bool
