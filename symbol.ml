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
    fontratio = 1.0 ;
    glyph = None }

type 'g set = ('g symbol list) ref

type 'g line = 
    { min : int ;
      max : int ;
      content : ('g symbol list) }

(* Rules and spaces are symbols with special fontnames *)
let rulename  = "RULE"
let spacename = "SPACE"

(* Iterates ff over the set. *)
let iter ff set = List.iter ff !set

(* Empty set, we probably do not need to optimize according to page dimensions. *)
let empty_set p_width p_height = ref []
  
(* Add an element. *)
let add el set = set := el :: !set

(* Says if the character code is probably the ascii code. *)
let is_pure code =
  match Char.chr code with
  | 'A'..'Z' | 'a'..'z' | '0'..'9' -> true
  | '!'..'~' -> true (* "false" helps for debugging. *)
  | _ -> false

(* Tells if s1 and s2, known to be on the same line, overlap seriously. *)
let overlap s1 s2 =
 (* Threshold in percent is used to tells when a space goes beyond a symbol, or just overlaps it. *)
 (* That is : if the space goes 50% further than the symbol, there is a space, otherwise there is not. *)
  let threshold = 50 in
  let s1,s2 = if s1.locx <= s2.locx then s1,s2 else s2,s1 in
  let right1 = s1.locx - s1.hoffset + s1.width
  and right2 = s2.locx - s2.hoffset + s2.width in
  let dx = right2 - right1 in
  if dx < 0
  then (* s2 is inside s1 *)
    -100 * dx < threshold * s2.width
  else
    let common_width = s2.width - dx in
    if common_width <= 0 then false
    else common_width > dx (* More than the half of s2 overlaps s1. *)

(* Rules. *)
let get_rule pre s = "_"

(* Spaces are a pain: *)
(* 3 kinds of spaces exist in dvi: 'r', 'w' and 'x' spaces. *)
(* Though 'w' should only be used to separate words, *)
(* TeX use indifferently all of them inside words, between letters, and so on. *)
(* Therefore no information can be drawn from this. Well done ! *)
(* However, a stupid method seems to work in *all* cases. *)
(* By converting spaces to pixels, which is not a good idea, since it depends on resolution, *)
(* it appears that all spaces > 0 should be shown. *)
(* The stupid method give definitely the best results. *)
let get_space pre s =
  if pre == dummy_symbol
  then
    begin
      (* A line beginning with a tabulation. *)
      "\t"
    end
  (* Same test as in get_rule. *)
  else if overlap pre s then ""
  else if s.width > 0 then " " else ""

(* Keep it for debug : dumps different spaces with size information. *)
let get_space2 pre s =
  let result = 
    match s.code with
    | 1 ->
      (* Adjustment space ('C_right'), it depends on the previous symbol. *)
	if pre == dummy_symbol
	then ("(tab=r"^(string_of_int s.width)^")")
	else
	  begin
	    "(r"^(string_of_int s.width)^")"
	  end
	    
    | 2 | 3 -> ("(w"^(string_of_int s.width)^")") (* Interword : C_w0 C_w *)
    | 4 | 5 -> ("(x"^(string_of_int s.width)^")") (* Interword? : C_x0 C_x *)
    | _ -> failwith "Symbol.get_space : unknown space code."
  in
  if overlap pre s then ("{"^result^"}")
  else result

let default_encoding font code =
  if is_pure code then String.make 1 (Char.chr code)
  else Printf.sprintf "[[%s:%d]]" font code

(* cmr is one of the usual tex fonts. *)	
let cmr_encoding font code =
    match code with
    | 0 -> "\\Gamma"
    | 1 -> "\\Delta"
    | 2 -> "\\Theta"
    | 3 -> "\\Lambda"
    | 4 -> "\\Xi"
    | 5 -> "\\Pi"
    | 6 -> "\\Sigma"
    | 7 -> "\\Upsilon"
    | 8 -> "\\Phi"
    | 9 -> "\\Psi"
    | 10 -> "\\Omega"
    | 11 -> "ff"
    | 12 -> "fi"
    | 16 -> "i"
    | 18 -> "\\`"
    | 19 -> "\\'"
    | 24 -> "\\c"
    | 26 -> "\\ae"
    | 34 -> ">>"
    | 60 -> "!"
    | 62 -> "?"
    | 92 -> "<<"
    | 94 -> "\\^"
    | 123 -> "--"
    | 124 -> "---"
    | 126 -> "~"
    | _ -> default_encoding font code
	  
(* Greek font. *)
let cmmi_encoding font code =
  if code >= 11 && code <= 41 then
    let alph =
      ["\\alpha" ; "\\beta"; "\\gamma"; "\\delta"; "\\epsilon"; "\\zeta"; "\\eta" ;
       "\\theta"; "[**unknown greek letter**]"; "\\kappa"; "\\lambda"; "\\mu"; "\\nu"; "\\xi"; "\\pi";
       "\\rho"; "\\sigma"; "\\tau"; "\\upsilon"; "\\phi"; "\\chi"; "\\psi";
       "\\omega"; "\\varepsilon"; "\\vartheta"; "\\varpi"; "\\varrho";
       "\\varsigma"; "\\varphi" ] 
    in 
    List.nth alph (code-11)
  else default_encoding font code

(* Math font. *)
let cmex_encoding font code =
  match code with
  | 0 -> "-"
  | 1 -> "."
  | 20 -> "<="
  | 21 -> ">="
  | 15 -> "\\bullet"
  | 102 -> "\\{"
  | 103 -> "\\}"
  | 112 -> "[sqrt]"
  | 113 -> "[SQRT]"
  | _ -> default_encoding font code

(* List of recognized fonts (regexp) * handler *)
(* Don't optimize the regexps, it is readable that way. *)
let encodings = [
  "cm[rs][0-9]+"  , cmr_encoding ;
  "cmt[ti][0-9]+" , cmr_encoding ;
  "cmsl[0-9]+"   , cmr_encoding ;
  "cmbx[0-9]+"   , cmr_encoding ;
  "cmmi[0-9]+"   , cmmi_encoding ;
  "cmex[0-9]+"   , cmex_encoding ;
  "cmsy[0-9]+"   , cmex_encoding ;
] 

let compile_regexp source =
  List.map (fun (pat,zz) -> Str.regexp pat, zz) source

(* Compiled regexps. *)
let encodings_c = compile_regexp encodings

let erase="/back/"

(* Returns the name of the symbol s, pre is the symbol before s. *)
let symbol_name pre s =
  let font = s.fontname in
  (* Is it a rule ? *)
  if font == spacename then get_space pre s
  else
    begin
      let name =
	if font == rulename then get_rule pre s
	else
          (* For normal symbols, find a matching regexp. *)
	  let handler = 
	    try snd (List.find
		       (fun (r,h) -> Str.string_match r font 0)
		       encodings_c)
	    with Not_found -> fun f s -> "["^f^"]"^(default_encoding f s)
	  in
	  handler font s.code
      in
      if pre.fontname == spacename && overlap pre s then erase^name
      else name
    end
  

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

(* Four (4 !) backslashes because of two-level escapes !!! *)

let process_all =
  ["."^erase, "" ; (* Remove backspaces. *)
   "c\\\\c", "\\cc" ] (* Sometimes cedil \c is after the 'c'. *)

let process_totext =
  ["\\\\bullet", "o " ]

(* Compiled versions. *)
let process_all_c = compile_regexp process_all
let process_totext_c = compile_regexp process_totext

(* Apply a process list to input. *)
let apply_process pl input =
  List.fold_left
    (fun input (pat, temp) -> Str.global_replace pat temp input)
    input pl

let post_process input =
  let input2 = apply_process process_all_c input in
  let input3 = apply_process process_totext_c input2 in
  input3

let above_lines bot1 top1 bot2 top2 =
  (* How much two lines must share in common (percent) to be declared overlaping. *)
  let threshold = 30 in
  if bot1 < top2 then -1
  else if bot2 < top1 then 1
  else 
      let minsize = min (bot1 - top1) (bot2 - top2)
      and share = (min bot1 bot2) - (max top1 top2) in
      if share * 100 > threshold * minsize then 0
      else if top1 < top2 then -1 else 1

let above s1 s2 =
  let top1 = s1.locy - s1.voffset
  and top2 = s2.locy - s2.voffset in
  let bot1 = top1 + s1.height 
  and bot2 = top2 + s2.height in
  above_lines bot1 top1 bot2 top2

(* Provided symbols s1 and s2 are on the same line, -1 means that s1 is at the left of s2. *)
(* It is important to compare right sides of symbols since spaces, for example, can stretch wide. *)
let at_right s1 s2 =
  if s1.locx - s1.hoffset + s1.width < s2.locx - s2.hoffset + s2.width then -1 else 1

(* Gives a string corresponding to line l .*)
let dump_line l =
  let l2 = List.sort at_right l.content in
  let line = ref ""
  and some = ref false (* Tells if at least one symbol is on the line. *)
  and old_sym = ref dummy_symbol in
  List.iter
    (fun s ->
      if s != dummy_symbol then
	begin
	  let ascii = symbol_name !old_sym s in
	  if ascii <> "" then
	    begin
	      line := !line^ascii ;
	      old_sym := s ;
	    end;
	  if s.fontname != spacename then some := true ;
	end
    )
    l2;

  let return = post_process !line in
  if !some then return^"\n" else ""

(* Split a list of symbols in lines. *)
let rec split current lines = function
  | [] -> current :: lines
  | s :: sl ->
      let top = s.locy - s.voffset in
      let bot = top + s.height in 
      if above_lines bot top current.max current.min <> 0
      then split {min = top;
		  max = bot;
		  content = [s]}
	  (current :: lines) sl
      else split {min = min current.min top;
		  max = max current.max bot;
		  content = s :: current.content}
	  lines sl

(* to_ascii returns a string representing the symbols in set. *)
(* Elements are filtered according to the first argument. *)
let to_ascii set =

  (* Split lines. *)
  let sorted = List.sort above !set in
  let lines = split {min = 0; max = 0; content = []} [] !set in
  let ascii = Misc.reverse_map dump_line lines in
  String.concat "" ascii
  
(* to_escaped does the same, but the final string is 'escaped'. *)
let to_escaped set = String.escaped (to_ascii set)

