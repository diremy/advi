(***********************************************************************)
(*                                                                     *)
(*                             Active-DVI                              *)
(*                                                                     *)
(*                   Projet Cristal, INRIA Rocquencourt                *)
(*                                                                     *)
(*  Copyright 2002 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the GNU Lesser General Public License.          *)
(*                                                                     *)
(*  Jun Furuse, Didier R�my and Pierre Weis.                           *)
(*  Contributions by Roberto Di Cosmo, Didier Le Botlan,               *)
(*  Xavier Leroy, and Alan Schmitt.                                    *)
(*                                                                     *)
(*  Based on Mldvi by Alexandre Miquel.                                *)
(***********************************************************************)

module type T = sig
   type glyph
   type fontname = string
   type fontratio = float
   type g =
       { fontname : string;
         fontratio : float;
         glyph : glyph;
       } 
   type symbol =
     | Glyph of g
     | Space of int * int
     | Rule of int * int
     | Line of int * string option
   type element =
       { color   : int ; 
         locx    : int ; 
         locy    : int ;
         code    : int ;
         symbol : symbol }
   type set = element list

   val voffset : element -> int
   val hoffset : element -> int
   val height : element -> int
   val width : element -> int

   val clear : unit -> unit
   (* color -> locx -> locy -> code -> symbol -> unit *)
   val add : int -> int -> int -> int -> symbol -> unit

   (* to_ascii returns a string representing the symbols that are in the set. *)
   (* Could be done with a pretty printer... *)
   val to_ascii   : set -> string
   val to_escaped : set -> string

   (* Gives a copy of set where only symbols in zone x1 y1 x2 y2 are kept. *)
   val inzone : int -> int -> int -> int -> set

   (* Idem where but the resulting set is time-convex
      (intermediate symbols are also kept). *)
   val intime : int -> int -> int -> int -> set

   (* Iterates function ff over the set of symbols. *)
   val iter : (element -> unit) -> set -> unit

   type region
   val position : int -> int -> region
   val new_region : region -> int -> int -> region
   val iter_region : (element -> unit) -> region -> unit
   val iter_regions :
       (element -> unit) -> (element -> unit) -> region -> region -> unit 
   val apply : (glyph -> int -> int -> int -> unit) -> element -> unit

   val lines : int -> int -> 
     (element * int * int *
        string * string * string * string * string option) option
   val word : int -> int -> (region * string) option
   val region_to_ascii : region -> string

end;;

module Make
    (G :
       sig
         type t
         val hoffset : t -> int
         val voffset : t -> int
         val width : t -> int
         val height : t -> int
       end) =
  struct

type glyph = G.t
type fontname = string
type fontratio = float
type g =
    { fontname : string;
      fontratio : float;
      glyph : glyph;
    }
type symbol =
    Glyph of g
  | Space of int * int
  | Rule of int * int
  | Line of int * string option
type element =
    { color   : int ;
      locx    : int ;
      locy    : int ;
      code    : int ;
      symbol : symbol }
type set = element list

let fontname s =
  match s.symbol with
  | Glyph g -> g.fontname
  | Space (w,h) -> "space"
  | Rule (w,h) -> "rule"
  | Line (w,h) -> "line"
let space s = match s.symbol with Space _ -> true | _ -> false

let voffset s =
  match s.symbol with
  | Glyph g -> G.voffset g.glyph
  | Space (w,h) -> h
  | _ -> 0

let hoffset s =
  match s.symbol with
  | Glyph g -> G.hoffset g.glyph
  | _ -> 0

let height s =
  match s.symbol with
  | Glyph g -> G.height g.glyph
  | Space (w,h) | Rule (w,h) -> h
  | _ -> 0

let width s =
  match s.symbol with
  | Glyph g -> G.width g.glyph
  | Space (w,h) | Rule (w,h) -> w
  | _ -> 0

(* Symbols information. *)

let dummy_symbol =
  { color = 0 ;
    locx  =  0 ;
    locy  =  0 ;
    code   = 0 ;
    symbol = Line(-1, None) }

let set : set ref = ref [];;
let clear() = set := [];;
let give() = !set

type line =
    { min : int ;
      max : int ;
      content : element list }

(* Iterates ff over the set. *)
let iter ff set = List.iter ff set

(* Add an element. *)

let add color x y code s =
  let symbol =
    { color = color ;
      locx = x ;
      locy = y ;
      code = code ;
      symbol = s;
    } in
  set := symbol :: !set

(* Says if the character code is probably the ascii code. *)
let is_pure code =
  match Char.chr code with
  | 'A'..'Z' | 'a'..'z' | '0'..'9' -> true
  | '!'..'~' -> true (* "false" helps for debugging. *)
  | _ -> false

(* Tells if s1 and s2, known to be on the same line, overlaps (1), goes beyond (2), or not (0). *)
let overlap s1 s2 =
 (* Threshold in percent is used to tells when a space goes beyond a symbol, or just overlaps it. *)
 (* That is : if the space goes 50% further than the symbol, there is a space, otherwise there is not. *)
  let threshold = 50 in
  let s1,s2 = if s1.locx <= s2.locx then s1,s2 else s2,s1 in
  let right1 = s1.locx - hoffset s1 + width s1
  and right2 = s2.locx - hoffset s2 + width s2 in
  let dx = right2 - right1 in
  if dx < 0
  then (* s2 is inside s1 *)
    if -100 * dx < threshold * width s2 then 1 else 2
  else
    let common_width = width s2 - dx in
    if common_width <= 0 then 0
    else if dx * 100 < common_width * threshold
    then 1
    else 2

let get_line pre s = ""
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
  if width s <= 0 then ""
  else if pre == dummy_symbol then
    (* A line beginning with a tabulation. *)
    "\t"
  (* Same test as in get_rule. *)
  else if overlap pre s = 1 then  ""
  else " "

(* Keep it for debug : dumps different spaces with size information. *)
let get_space2 pre s =
  let w = string_of_int (width s) in
  let result =
    match s.code with
    | 1 ->
      (* Adjustment space ('C_right'), it depends on the previous symbol. *)
	if pre == dummy_symbol then "(tab=r" ^ w ^")" else  "(r" ^ w ^ ")"
    | 2 | 3 -> "(w" ^ w ^")" (* Interword : C_w0 C_w *)
    | 4 | 5 -> "(x" ^ w ^")" (* Interword? : C_x0 C_x *)
    | _ -> failwith "Symbol.get_space : unknown space code."
  in
  if overlap pre s = 1 then "{" ^ result ^ "}" else result

let default_encoding font code =
  if is_pure code then String.make 1 (Char.chr code)
  else String.make 1 (Char.chr code)
   (*  Printf.sprintf "[[%s:%d]]" font code *)
   

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
let ecmr_encoding font code =
  match code with
  | 027 -> "ff"
  | 028 -> "fi"
  | x -> cmr_encoding font code;;

let accents encoding font code =
  if code > 128 then String.make 1 (Char.chr code)
  else encoding font code

(* Greek font. *)
let cmmi_encoding font code =
  if code >= 11 && code <= 41 then
    let alph =
      [ "\\alpha" ; "\\beta"; "\\gamma"; "\\delta";
        "\\epsilon"; "\\zeta"; "\\eta" ;
        "\\theta"; "[**unknown greek letter**]"; "\\kappa";
        "\\lambda"; "\\mu"; "\\nu"; "\\xi"; "\\pi";
        "\\rho"; "\\sigma"; "\\tau"; "\\upsilon"; "\\phi"; "\\chi"; "\\psi";
        "\\omega"; "\\varepsilon"; "\\vartheta"; "\\varpi"; "\\varrho";
        "\\varsigma"; "\\varphi" ]
    in
    List.nth alph (code - 11)
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

(* tt font. *)
let cmtt_encoding font code =
  if is_pure code then String.make 1 (Char.chr code)
  else cmr_encoding font code;;

(* List of recognized fonts (regexp) * handler *)
(* Don't optimize the regexps, it is readable that way. *)
let encodings = [
  "cm[rs][0-9]+" , cmr_encoding ;
  "cmt[ti][0-9]+", cmtt_encoding ; 
  "cms[lxs][0-9]+"   , cmr_encoding ;
  "cmbx[0-9]+"   , cmr_encoding ;
  "cmmi[0-9]+"   , cmmi_encoding ;
  "cmex[0-9]+"   , cmex_encoding ;
  "cmsy[0-9]+"   , cmex_encoding ;
(* does not seem correct. eg. ff is \027 instead of \011? what else? *)
  "ecrm[0-9]+",    accents ecmr_encoding ;
  "ecti[0-9]+",    accents ecmr_encoding ;
  "ecbx[0-9]+",    accents ecmr_encoding ;
  "ectt[0-9]+",    accents cmtt_encoding ;
  "lcmssb?[0-9]+",   accents cmr_encoding ;
  "lcmssi[0-9]+",  accents cmmi_encoding ;
]

let compile_regexp source =
  List.map (fun (pat,zz) -> Str.regexp pat, zz) source

(* Compiled regexps. *)
let encodings_c = compile_regexp encodings

let erase="/back/"

(* Returns the name of the symbol s, pre is the symbol before s. *)
let true_symbol_name s =
  let font = fontname s in
  (* For normal symbols, find a matching regexp. *)
  let handler =
    try snd (List.find
	       (fun (r,h) -> Str.string_match r font 0)
	       encodings_c)
    with Not_found ->
      fun f s -> "["^f^"]"^(default_encoding f s)
  in
  handler font s.code

let symbol_name pre s =
  match s.symbol with
  | Line (l, f) -> get_line pre s
  | Space (w, h) -> get_space pre s
  | Rule (w, h) -> get_line pre s
  | _ ->
      let name = true_symbol_name s in
      if space pre && overlap pre s <> 0 then erase^name else name


(* Says if the symbol is in zone x1-x2 y1-y2. *)
(* Size of the symbol should be used...we'll see later. *)
let symbol_inzone x1 y1 x2 y2 =
  let (x1,x2) = if x1 <= x2 then x1,x2 else x2,x1
  and (y1,y2) = if y1 <= y2 then y1,y2 else y2,y1 in
  function s ->
    (s.locx - hoffset s <= x2) && (s.locx - hoffset s + width s >= x1) &&
    (s.locy - voffset s <= y2) && (s.locy - voffset s + height s >= y1)

let inzone x1 y1 x2 y2 =
  Misc.reverse_filter (symbol_inzone x1 y1 x2 y2) !set

let intime x1 y1 x2 y2 =
  let inz = symbol_inzone x1 y1 x2 y2 in
  let rec discard sl =
    match sl with
    | [] -> []
    | s :: l -> if inz s then sl else discard l
  in
  discard (List.rev (discard !set))


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
  let top1 = s1.locy - voffset s1
  and top2 = s2.locy - voffset s2 in
  let bot1 = top1 + height s1
  and bot2 = top2 + height s2 in
  above_lines bot1 top1 bot2 top2

(* Provided symbols s1 and s2 are on the same line, -1 means that s1 is at the left of s2. *)
(* It is important to compare right sides of symbols since spaces, for example, can stretch wide. *)
let at_right s1 s2 =
  if s1.locx - hoffset s1 + width s1 < s2.locx - hoffset s2 + width s2
  then -1 else 1

(* Gives a string corresponding to line l .*)
let dump_line l =
  let l2 = List.sort at_right l.content in
  let line = ref ""
  and some = ref false (* Tells if at least one symbol is on the line. *)
  and old_sym = ref dummy_symbol in
  let action s =
    match s.symbol with
    | Line (_,_) -> ()
    | _ ->
	let ascii = symbol_name !old_sym s in
	if ascii <> "" then
	  begin
	    line := !line^ascii ;
	    if not (space s) || width s > 0 then old_sym := s ;
	  end;
	if not (space s) then some := true  in
  List.iter action l2;

  let return = post_process !line in
  if !some then return^"\n" else ""

(* Split a list of symbols in lines. *)
let rec split current lines = function
  | [] -> current :: lines
  | s :: sl ->
      let top = s.locy - voffset s in
      let bot = top + height s in
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
  let lines = split {min = 0; max = 0; content = []} [] set in
  let ascii = Misc.reverse_map dump_line lines in
  String.concat "" ascii

(* to_escaped does the same, but the final string is 'escaped'. *)
let to_escaped set =
  String.escaped (to_ascii set)

(* get the closest lines to a given position on the screen *)
type rect =
    { mutable top : int;
      mutable bot : int;
      mutable left : int;
      mutable right : int }

type region =
    { history : element array;
      first : int; last : int;
    };;

let region_to_ascii r =
  let pos = min r.first r.last in
  let length = 1 + abs (r.first - r.last) in
  to_ascii (List.rev (Array.to_list (Array.sub r.history pos length)))

let distance x y s =
  let dist z h dh =
    if z < h then h - z else if h + dh < z then z - h - dh else 0 in
  let dx = dist x (s.locx - hoffset s) (width s) in
  let dy = dist y (s.locy - voffset s) (height s) in
  dx + dy

let closest history x y =
  let distance = distance x y in
  try
    let compare i j =
      let x = history.(i) and y = history.(j) in
      let dx = distance x and dy = distance y in
      if dx = dy then
        if dx = 0 then max (width x) (height x)
        else 100 * (i - j)
      else 10000 * (dx - dy) in
    let i = ref 0 in
    Array.iteri (fun j _ -> if compare !i j > 0 then i := j) history;
    !i
  with Invalid_argument _ ->
    assert false

let nearest r x y =
  let distance = distance x y in
  let h = r.history in
  let last = r.last in
  assert (Array.length h > last);
  if distance h.(last) = 0 then last
  else
    try
      let i = ref last in
      let compare j = if distance h.(!i) > distance h.(j) then i := j in
      let attempt d e =
        for j = max 0 (last - d) to min (Array.length h - 1) (last + d) do
          compare j done;
        if distance h.(!i) <= e then !i else raise Not_found in
      try attempt 10 4 with Not_found ->
        try attempt 100 5 with Not_found ->
          try attempt 400 7 with Not_found ->
            closest h x y
    with Invalid_argument _ ->
      assert false


let position x y =
  let history = Array.of_list !set in
  if Array.length history > 0 then
    let time = closest history x y in
    { history = history; first = time; last = time;  }
  else raise Not_found

let valid position i = i >= 0 && i < Array.length position.history;;

let around b x y =
  try
    let position = position x y in
    let space_ref = position.history.(position.first) in
    let valid = valid position in
    let rec skip_spaces move i =
      if valid i then
        let h = position.history.(i) in
        if space h then skip_spaces move (move i) else i
      else i in
    let rec word move p w i =
      if valid i then
        let h = position.history.(i) in
        let next = move i in
        let pre = if p < 0 then dummy_symbol else position.history.(p) in
        let return w = p, w in
        match h.symbol with
        | Space (_,_) ->
            let c = symbol_name pre h in
            if c = " " then return w
            else word move i w next
        | Line (_,_) -> word move i w next
        | Rule (_,_) -> return w
        | _ ->
            if pre <> dummy_symbol && above pre h <> 0 then return w
            else
              let c = symbol_name pre h in
              let add x y = if move 0 > 0 then x ^ y else y ^ x in
              word move i (add (true_symbol_name h) w) next
      else -1,  w in
    let rec prefix move i =
      if valid i then
        let h = position.history.(i) in
        let pre = position.history.(0 - (move (0 - i))) in
        if space h then
          let c = symbol_name pre h in
          if c = " " then word move i c (skip_spaces move (move i))
          else prefix move (move i)
        else word move i "" i
      else -1, "" in
    let point = position.first in
    let iw1 = prefix succ (succ point) in
    let iw2 = prefix pred (point) in
    let context move iw =
      let i = move (fst iw) in
      if valid i then
        let i, w = prefix move (skip_spaces move i) in
        if valid i then w else ""
      else "" in
    let left = context succ iw1 in
    let right = context pred iw2 in
    Some (position, left, iw1, iw2, right)
  with
  | Not_found -> None
  | Invalid_argument _ -> assert false
;;

let rec least l min =
  match l with
    {symbol = Line (l,_)} :: rest -> least rest (if l < min then l else min)
  | _ :: rest -> least rest min
  | [] -> min

let lines x y =
  match around true x y with
  | None -> None
  | Some (region, left, (i1, w1), (i2, w2), right) ->
      let rec find_line move i =
        if valid region i then
          let h = region.history.(i) in
          match h.symbol with
          | Line(n,f) when n >= 0 -> n, f
          | _ ->  find_line move (move i)
        else -1, None in
      let space_ref = region.history.(region.first) in
      let l1, f1 = find_line succ (succ i1) in
      let l2, f2 = find_line pred (pred i2) in
      let l1 = if l1 > l2 && l2 > 0 then least !set l2 else l1 in
      let f = match f1 with Some _ -> f1 | _ -> f2 in
      Some (space_ref, l1, l2, left, w1, w2, right, f)
;;

let word x y =
  match around false x y with
  | None -> None
  | Some (region, left, (i1, w1), (i2, w2), right) ->
      let take w =
        let l = String.length w in
        l > 0 && w.[0] <> ' ' && w.[l - 1] <> ' ' in
      let b1 = take w1 and b2 = take w2 in
      if b1 && b2 then
        Some ({region with first = i1; last = i2}, w1 ^ w2)
      else if b1 then
        Some ({region with first = i1}, w1)
      else if b2 then
        Some ({region with first = i2}, w2)
      else
        None
;;

let apply f = function
  | { symbol = Glyph g } as s -> f g.glyph s.color s.locx s.locy
    | _ -> ()
;;

let iter_region f r =
  let iter first last f =
    for i = min first last to max first last do f r.history.(i) done in
  iter r.first r.last f
;;

let iter_regions delete add r r' =
  assert (r.history == r'.history && r.first = r'.first);
  if r.last = r'.last then ()
  else
    let iter first last f =
      for i = min first last to max first last do f r.history.(i) done in
    let l = r.last and l' = r'.last and f = r.first in
    if l > f && l' > f then
      if l' > l then iter l l' add else iter l l' delete
  else if l < f && l' < f then
    if l' > l then iter l l' delete else iter l l' add
  else
    (iter f l  delete; iter f l' add)
;;

let rect r = r.left, r.bot, r.right - r.left, r.top - r.bot

let new_region r x y =
  let l = r.history.(r.last) in
  if distance x y l = 0 then r else  { r with last = nearest r x y }
;;

end
;;



