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

(* A better structure is welcome. *)
type set = symbol list
type named_set = string list

(* Empty set. *)
let empty_set = []

(* Add an element. *)
let add el set = el :: set

(* Says if the character code is probably the ascii code. *)
let is_pure code =
  match Char.chr code with
  | '!'..'~' -> true
  | _ -> false
  
(* Returns the name of the symbol. *)
let symbol_name s =
  match s.fontname,s.code with
  | _, code when (is_pure code) -> String.make 1 (Char.chr code)
  | font, code -> Printf.sprintf "[[%s:%d]]]" font code

(* Converts a symbol_set to a named_set. *)
let find_names sym_set =
  List.map symbol_name sym_set

(* Filters a symbol_set with function ff. *)
let filters ff symbol_set =
  Misc.reverse_filter ff symbol_set

(* So far we only dump lists of symbols. *)
(* In the future we will have to preserve spacing and formatting. *)

(* to_ascii takes a named_set and returns a string representing the named symbols. *)
let to_ascii named_set =
  let rec append s = function
    | [] -> s
    | n :: l -> append (s^n) l
  in
  append "" named_set
  
(* to_escaped does the same, but the final string is 'escaped'. *)
let to_escaped name_list = String.escaped (to_ascii name_list)

(* Says if the symbol is in zone x1-x2 y1-y2. *)
(* Size of the symbol should be used...we'll see later. *)
let inzone x1 y1 x2 y2 s =
  let (x1,x2) = if x1 <= x2 then x1,x2 else x2,x1
  and (y1,y2) = if y1 <= y2 then y1,y2 else y2,y1 in

  (s.locx - s.hoffset <= x2) && (s.locx - s.hoffset + s.width >= x1) &&
  (s.locy - s.voffset <= y2) && (s.locy - s.voffset + s.height >= y1)

