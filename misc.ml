(* Reverse and filters list l according to f. (Pour faire plaisir à Gérard) *)
let reverse_filter f l =
  let rec filter res = function
    | [] -> res
    | a::l -> if f a then filter (a::res) l else filter res l
  in
  filter [] l

let reverse_map f l =
  let rec map res = function
    | [] -> res
    | a::l -> map (f a::res) l
  in
  map [] l

(* Concat of List-1 and List-2 is  2-tsiL :: List-1 *)
let rec reverse_concat l1 = function
  | [] -> l1
  | a::b -> reverse_concat (a::l1) b

(* Strings *)

exception False;;
let has_prefix pre str =
  let len = String.length pre in
  let l =  String.length str in
  l >= len &&
  begin
    try
      for i = 0 to len - 1 do if str.[i] <> pre.[i] then raise False done;
      true;
    with False -> false
  end
    ;;

let has_suffix suf str =
  let lsuf = String.length suf in
  let lstr =  String.length str in
  lstr >= lsuf &&
  begin
    try
      let d = lstr - lsuf in
      for i = 0 to lsuf - 1 do if str.[d + i] <> suf.[i] then raise False done;
      true;
    with False -> false
  end
    ;;

exception Match
let get_suffix pre str =
  let lpre = String.length pre in
  let lstr = String.length str in
  if has_prefix pre str then String.sub str lpre (lstr -lpre)
  else raise Match
    ;;

let rec split_string s c start =
  let len = String.length s
  and i = ref start in
  while !i < len && s.[!i] = c do incr i done ;
  if !i >= len then [] else begin
    let i0 = !i in
    while !i < len && s.[!i] <> c do incr i done ;
    let i1 = !i in
    String.sub s i0 (i1 - i0) :: split_string s c i1
  end ;;

(* unix command line parser *)
let parse_shell_command str =
  let arglist = split_string str ' ' 0 in
  Array.of_list arglist
;;
let fork_process command = 
  let command_tokens = parse_shell_command command in
  let pid = Unix.fork () in
  if pid = 0 then
    begin (* child *)
      try
        Unix.execvp command_tokens.(0) command_tokens
      with
      | _ -> exit 127
    end;
  pid;;

(* Command line options and debugging *)

exception Found of string * string 
let pretty_options all_options = 
  let tab (o, s, m) =
    let s = split_string m '\t' 0 in
    if String.length m > 0 && m.[0] = '\t' then "" :: s else s in
  let tab_options = List.map tab all_options in
  let width =
    2 + 
    List.fold_left2
      (fun w (o,_,_) -> function
        | [] | _ :: [] -> w
        | m::_ -> max w (String.length o + String.length m))
      0 all_options tab_options in
  let margin = "\n" ^ String.make (width + 1) ' ' in
  let indent o = function
    | [] -> assert false
    | [h] -> h
    | h::m::t ->
        let length = width - String.length o - String.length h in
        let hm = h ^ (String.make length ' ') ^ m in
        if t = [] then  hm
        else hm ^ (String.concat margin t) in
  List.map2 (fun  (o, s, _ ) ml -> (o, s, indent o ml))
    all_options tab_options;;

(* to add options to the command line in different modules way *)
let options = ref [];;
let all_options() = !options;;
let set_option op action man =
  options := (op, action, man) :: !options;;

(* a special case: flag options *)
let option_flag initial option message =
  let r = ref initial in
  let _ =
    set_option option  (if initial then Arg.Clear r else Arg.Set r)
      ("\t" ^ message) in
  r;;

(* an special case: debug options *)
let make_debug r s =
  if !r then prerr_endline s;
  true;;

let debug_option option message =
  let r = ref false in
  let _ = set_option option (Arg.Set r) ("\t" ^ message) in
  make_debug r;;

(* two global options *)
let debug = debug_option
    "--debug"
    "General debug";;

let pson = 
  if Config.have_gs then
    option_flag true
      "-nogs" "Turn off display of inlined Postscript"
  else ref false;;

let dops = ref (!pson);;

let warning mes =
  Printf.fprintf stderr "Warning: %s" mes;
  prerr_newline();;
  

exception Fatal_error of string;;
let fatal_error x = raise (Fatal_error x);;

let handle_fatal_error f () =
  try f () with Fatal_error s -> prerr_string s; prerr_newline()
;;

