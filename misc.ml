(* Reverse and filters list l according to f. (Pour faire plaisir à Gérard) *)
let reverse_filter f l =
  let rec filter res = function
    | [] -> res
    | a :: l -> if f a then filter (a :: res) l else filter res l in
  filter [] l;;

let reverse_map f l =
  let rec map res = function
    | [] -> res
    | a :: l -> map (f a :: res) l in
  map [] l;;

(* Concat of List-1 and List-2 is  2-tsiL :: List-1 *)
let rec reverse_concat l1 = function
  | [] -> l1
  | a :: b -> reverse_concat (a :: l1) b;;

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
  end;;

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
  end;;

exception Match;;
let get_suffix pre str =
  let lpre = String.length pre in
  let lstr = String.length str in
  if has_prefix pre str then String.sub str lpre (lstr - lpre)
  else raise Match;;

let rec split_string s p start =
  let len = String.length s
  and i = ref start in
  while !i < len && p s.[!i] do incr i done ;
  if !i >= len then [] else begin
    let i0 = !i in
    while !i < len && not (p s.[!i]) do incr i done ;
    let i1 = !i in
    String.sub s i0 (i1 - i0) :: split_string s p i1
  end;;

let catenate_sep sep = function 
  | [] -> ""
  | x :: l -> List.fold_left (fun s s' -> s ^ sep ^ s') x l;;

(* Unix command line parser *)
let parse_shell_command str =
  let p c = c = ' ' in
  let arglist = split_string str p 0 in
  Array.of_list arglist;;

(* Handling forking problems: only father process can call the at_exit
   function, sons of the main process must leave without calling it.
   Otherwise we would attempt to kill embedded processes twice,
   leading to bus errors or bad exception handling (fatal errors). *)
let advi_process = Unix.getpid ();;

let exit code =
  (* at_exit code must be called only by the ADVI process.
     if it is one of the forked processes, it must DIE IMMEDIATELY:
     any cleaning must not be permitted. *) 
  if Unix.getpid () = advi_process then Pervasives.exit code
  else (* SUICIDE *)
  Unix.kill (Unix.getpid ()) 9;;

let fork_process command = 
  let command_tokens = parse_shell_command command in
  let pid = Unix.fork () in
  if pid = 0 then
    begin (* child *)
      try
        Unix.execvp command_tokens.(0) command_tokens;
	exit 0
      with
      | Unix.Unix_error (e,_,arg) -> 
	  Printf.fprintf stderr "Warning: %s: %s" (Unix.error_message e) arg;
	  prerr_newline ();
	  exit 127
    end;
  pid;;

(* Command line options and debugging *)

exception Found of string * string;;

let pretty_options all_options = 
  let tab (o, s, m) =
    let s = split_string m (function '\t' -> true | _ -> false) 0 in
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

(* To add options to the command line in different modules way *)
let options = ref [];;
let all_options () = !options;;
let set_option op action man =
  options := (op, action, man) :: !options;;

(* A special case: flag options *)
let option_flag initial option message =
  let r = ref initial in
  set_option option
    (if initial then Arg.Clear r else Arg.Set r)
    ("\t" ^ message);
  r;;

(* A special case: debug options *)
let make_debug r s =
  if !r then prerr_endline s;
  true;;

let debug_option option message =
  let r = ref false in
  set_option option (Arg.Set r) ("\t" ^ message);
  make_debug r;;

(* Some global options *)
let debug = debug_option "--debug" "General debug";;

let pson = 
  if Config.have_gs then
    option_flag true
      "-nogs" "Turn off display of inlined Postscript"
  else ref false;;
let dops = ref !pson;;

let global_display_mode = ref false;;
let set_global_display_mode b =
  GraphicsY11.global_display_mode b;
  global_display_mode := b;;

set_option "-fg"
 (Arg.Unit (fun () -> set_global_display_mode true))
 "Draw in the foreground";;

let warning mes =
  Printf.fprintf stderr "Warning: %s" mes;
  prerr_newline();;

exception Fatal_error of string;;
let fatal_error x = raise (Fatal_error x);;

let handle_fatal_error f () =
  try f () with Fatal_error s -> prerr_endline s; exit 1;;
