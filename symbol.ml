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
    locx  = -500000 ;
    locy  = -500000 ;
    voffset = 0 ;
    hoffset = 0 ;
    width  = 0 ;
    height = 0 ;
    code   = 0 ;
    fontname = "" ;
    fontratio = 1.0 }

type set = {
    sym_width  : int ;
    sym_height : int ;
    matrix : (symbol list) array array
  } 

let draw_symbol s c =
  Graphics.set_color c;
  Graphics.draw_rect s.locx s.locy s.width s.height;
  Graphics.synchronize()

(* Page is cut into small cells : *)
let dimx = 100
and dimy = 100

(* Empty set *)
let empty_set p_width p_height =
  let matrix = Array.create_matrix dimx dimy [] in
  { sym_width = 1 + p_width / dimx ;
    sym_height = 1 + p_height / dimy ;
    matrix = matrix }
  
(* Add an element. *)
let add el set =
  let posx = el.locx / set.sym_width
  and posy = el.locy / set.sym_height in
  if (posx < dimx) && (posy < dimy) && (posx >= 0) && (posy >= 0) then
    set.matrix.(posx).(posy) <- el :: set.matrix.(posx).(posy)

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

let name_to_ascii n = n

(* Says if the symbol is in zone x1-x2 y1-y2. *)
(* Size of the symbol should be used...we'll see later. *)
let inzone x1 y1 x2 y2 s =
  (s.locx - s.hoffset <= x2) && (s.locx - s.hoffset + s.width >= x1) &&
  (s.locy - s.voffset <= y2) && (s.locy - s.voffset + s.height >= y1)


(** FORMATING **)

exception Break of int*int

let empty_document = "[empty_document]"

(* Says if symbol s1 is on a line above(-1) or below (1) s2. Should return 0 for ² and x in x². *)
(* I think that hoffset and voffset _must_ be ignored. *)
let above s1 s2 =
  if s1.locy < s2.locy - s2.height then -1
  else if s2.locy < s1.locy - s1.height then 1
  else 0

(* Provided symbols s1 and s2 are on the same line, -1 means that s1 is at the left of s2. *)
let at_right s1 s2 =
  if s1.locx < s2.locx then -1 else 1

(* Threshold to detect a blank, see below. *)
let threshold = 40

(* Says if there is a blank between s1 and s2. *)
let blank () =

  (* We make stats over the width of nb previous symbols. *)
  let nb = 20 in

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
      

      if s2.locx - s1.locx - s1.width > (threshold * !total_width) / (100 * !stat_nb)
      then
	begin
	(* draw_symbol s1 Graphics.blue; *)
	  true
	end
      else 
	begin
	(* draw_symbol s1 Graphics.red; *)
	  false
	end
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

(* to_ascii returns a string representing the symbols that are in zone 'zone'. *)
(* Elements are filtered according to the first argument. *)
let to_ascii (x1,y1,x2,y2) set =

  let (x1,x2) = if x1 <= x2 then x1,x2 else x2,x1
  and (y1,y2) = if y1 <= y2 then y1,y2 else y2,y1 in

  let cx1 = x1/set.sym_width
  and cx2 = 1 + x2/set.sym_width
  and cy1 = y1/set.sym_height
  and cy2 = 1 + y2/set.sym_height in

  let result = ref [] in

  for j = cy1 to cy2 do
    for i = cx1 to cx2 do
      let l1 = set.matrix.(i).(j) in
      let l2 = Misc.reverse_filter (inzone x1 y1 x2 y2) l1 in
      result := Misc.reverse_concat l2 !result
    done;
  done;

  (* Split lines. *)
  let sorted = List.sort above !result in
  let rec split res l =
    match l,res with
    | _, ([] | []::_) -> failwith "Symbol.to_ascii(split) impossible error."
    | [], _ -> res
    | s::b, ((sym::q1) as hd)::q2 ->
	if above sym s < 0 then split ([s]::res) b
	else split ((s::hd)::q2) b
  in
  let lines = split [[dummy_symbol]] sorted in
  let ascii = Misc.reverse_map dump_line lines in
  String.concat "\n" ascii
  
(* to_escaped does the same, but the final string is 'escaped'. *)
let to_escaped zone set = String.escaped (to_ascii zone set)

