(*** Some utilities for specials ***)

let split_string s start =
  Misc.split_string s (function ' ' -> true | _ -> false) start;;

(* "hello world" is one word *)
let rec split_string_quoted s start =
  let len = String.length s
  and i = ref start in
  (* find a space *)
  while !i < len && s.[!i] = ' ' do incr i done;
  if !i >= len then [] else begin
    let i0 = !i in
    while !i < len && s.[!i] <> ' ' do
      if s.[!i] = '"' (* '"' *) then begin
        incr i;
        while !i < len && s.[!i] <> '"' do incr i done;
        if s.[!i] <> '"' then failwith "parse error (split_string_quoted)";
        incr i
      end else incr i
    done;
    let i1 = !i in
    String.sub s i0 (i1 - i0) :: split_string_quoted s i1
  end;;

(* "\"hello world\"" -> "hello world" *)
let unquote s =
  let f = if s.[0] = '"' then 1 else 0 in
  let l =
    if s.[String.length s - 1] = '"' then String.length s - 1 - f
    else String.length s - f in
  String.sub s f l;;

let split_record s =
  let tokens = split_string_quoted s 0 in
  List.map (fun token ->
    try
      let i = String.index token '=' in
      String.sub token 0 i,
      String.sub token (i + 1) (String.length token - i - 1)
    with
    | _ -> token, "") tokens
;;

let get_records s =
  List.map (fun (k, v) -> String.lowercase k, v) (split_record s);;

let ill_formed_special s =
  Misc.warning (Printf.sprintf "Ill formed special <<%s>>" s);;

let parse_float s =
 try float_of_string s
 with _ -> failwith ("advi: cannot read a floating number in \"" ^ s ^ "\"");;

let option_parse_float s r =
  try Some(parse_float (List.assoc s r))
  with _ -> None;;
