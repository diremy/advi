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

let dummy_symbol =
  { color = 0 ;
    locx  =  0 ;
    locy  =  0 ;
    voffset = 0 ;
    hoffset = 0 ;
    width  = 0 ;
    height = 0 ;
    code   = 0 ;
    fontname = "" ;
    fontratio = 1.0 }

type set = (symbol list) ref

let draw_symbol s c =
  Graphics.set_color c;
  Graphics.draw_rect (s.locx-s.hoffset) (s.locy-s.voffset) s.width s.height;
  Graphics.synchronize()

(* Empty set, we probably do not need to optimize according to page dimensions. *)
let empty_set ~pagewidth:p_width ~pageheight:p_height = ref []
  
(* Add an element. *)
let add el set = set := el :: !set

(* Says if the character code is probably the ascii code. *)
let is_pure code =
  match Char.chr code with
  | '!'..'~' -> true
  | _ -> false
  
(* Symbols that are surrounded with big spaces. *)
let big_space s = 
  match Char.chr s.code with
  | '!' | '.' | ':' | ',' -> true
  | _ -> false

(* Returns the name of the symbol. *)
let symbol_name s =
  match s.fontname,s.code with
  | _, code when (is_pure code) -> String.make 1 (Char.chr code)
  | font, code -> Printf.sprintf "[[%s:%d]]]" font code

let name_to_ascii n = n

(* Says if the symbol is in zone x1-x2 y1-y2. *)
(* Size of the symbol should be used...we'll see later. *)
let symbol_inzone x1 y1 x2 y2 =
  let (x1,x2) = if x1 <= x2 then x1,x2 else x2,x1
  and (y1,y2) = if y1 <= y2 then y1,y2 else y2,y1 in
  function s -> 
    (s.locx - s.hoffset <= x2) && (s.locx - s.hoffset + s.width >= x1) &&
    (s.locy - s.voffset <= y2) && (s.locy - s.voffset + s.height >= y1)

let inzone x1 y1 x2 y2 set =
   ref (Misc.reverse_filter (symbol_inzone x1 y1 x2 y2) !set)
      
let intime x1 y1 x2 y2 set =
  let inz = symbol_inzone x1 y1 x2 y2 in
  let rec discard sl =
    match sl with
    | [] -> []
    | s :: l -> if inz s then sl else discard l
  in
  ref (discard (List.rev (discard !set)))
    

(** FORMATING **)

exception Break of int*int

let empty_document = "[empty_document]"

(* Says if symbol s1 is on a line above(-1) or below (1) s2. Should return 0 for ² and x in x². *)
(* I think that voffset _must_ be ignored. *)
let above s1 s2 =
  if s1.locy < s2.locy - s2.height then -1
  else if s2.locy < s1.locy - s1.height then 1
  else 0

(* Provided symbols s1 and s2 are on the same line, -1 means that s1 is at the left of s2. *)
let at_right s1 s2 =
  if s1.locx < s2.locx then -1 else 1

(* Thresholds to detect a blank, see below. *)
let threshold1 = 25 (* For normal symbols. *)
let threshold2 = 60 (* For some punctuation signs. *)

(* Says if there is a blank between s1 and s2. Here hoffset must be taken into account. *)
let blank () =

  (* We make stats over the width of nb previous symbols. *)
  let nb = 5 in

  let total_width = ref 0  (* Total width of all elements in the array. *)
  and stat_nb = ref 0    (* Number of elements in the array. *)
  and stat_index = ref 0 (* Index of the next element to be put in the array. *)
  and stat_syms = Array.create nb dummy_symbol in

  fun s1 s2 ->
  if (s1 == dummy_symbol) || (s2 == dummy_symbol) then false
  else
    begin      
      (* Stats. *)
      let tot = !total_width in
      let tot = if !stat_nb = nb then tot - stat_syms.(!stat_index).width else (incr stat_nb; tot) in
      let tot = tot + s1.width in
      stat_syms.(!stat_index) <- s1 ;
      stat_index := (!stat_index + 1) mod nb ;
      total_width := tot;
      let space = (s2.locx - s2.hoffset) - (s1.locx + s1.width - s1.hoffset) in
      
      let threshold = if (big_space s1) || (big_space s2) then threshold2 else threshold1 in
      space > (threshold * !total_width) / (100 * !stat_nb)
    end

(* Gives a string corresponding to line l. l is a list of symbols. *)
let dump_line l =
  let is_blank = blank () in
  let l2 = List.sort at_right l in
  let line = ref ""
  and old_sym = ref dummy_symbol in
  List.iter
    (fun s ->
      if s != dummy_symbol then
	begin
	  let ascii = name_to_ascii (symbol_name s) in
          (* Is there a blank between them ? *)
	  if is_blank !old_sym s
	  then line := !line^" "^ascii
	  else line := !line^ascii ;
	  old_sym := s ;
	end
    )
    l2;
  !line

let minmax line =
  let min = ref max_int
  and max = ref min_int in
  List.iter
    (fun s ->
      let top = s.locy in
      if top > !max then max := top;
      let bottom = s.locy - s.height in
      if bottom < !min then min := bottom;
    )
    line;
  (!min, !max)

(* Tells if two lines overlap. *)
(* In principle, line min1-max1 is below min2 max2. *)
let overlap min1 max1 min2 max2 =
  max2 > min1 + (max1 - min1)/2

let glue lines =
  (* Lines are sorted from bottom to top. *)
  let rmin = ref 0
  and rmax = ref 0 in
  List.fold_left
    (fun res line ->
      let lmin,lmax = minmax line in
      match res with
      |	[] -> rmin := lmin ; rmax := lmax ; [line]
      |	a::b ->
	  if overlap !rmin !rmax lmin lmax then
	    (* Lines overlap. *)
	    begin
	      rmax := max lmax !rmax ;
	      rmin := min lmin !rmin ;
	      (Misc.reverse_concat line a)::b
	    end
	  else 
	    begin
	      rmax := lmax ;
	      rmin := lmin ;
	      line :: res
	    end
    )
    [] lines 

(* to_ascii returns a string representing the symbols in set. *)
(* Elements are filtered according to the first argument. *)
let to_ascii set =

  (* Split lines. *)
  let sorted = List.sort above !set in
  let rec split res l =
    match l,res with
    | _, ([] | []::_) -> failwith "Symbol.to_ascii(split) impossible error."
    | [], _ -> res
    | s::b, ((sym::q1) as hd)::q2 ->
	if above sym s < 0 then split ([s]::res) b
	else split ((s::hd)::q2) b
  in
  let lines = split [[dummy_symbol]] sorted in

  (* A line can be separated in two lines so we detect that and glue. *)
  let lines = List.rev (glue lines) in
  let ascii = Misc.reverse_map dump_line lines in
  String.concat "\n" ascii
  
(* to_escaped does the same, but the final string is 'escaped'. *)
let to_escaped set = String.escaped (to_ascii set)

