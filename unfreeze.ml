open DrDvi;;

(* freeze/unfreeze fonts/glyphs *)

let font cdvi n =
  try
    let cfont = Table.get cdvi.font_table n in
    ignore (Table.get cfont.mtable (Char.code 'A'))
  with _ -> ();;

let fonts cdvi =
  List.iter (fun (n, _) -> font cdvi n)
    cdvi.base_dvi.Dvi.font_map;;

let glyphs cdvi dpi =
  let mag = float cdvi.base_dvi.Dvi.preamble.Dvi.pre_mag /. 1000.0 in
  let sdpi = int_of_float (mag *. ldexp dpi 16)
  and mtable = ref dummy_mtable
  and gtable = ref dummy_gtable in
  let headers = ref []
  and xrefs = cdvi.base_dvi.Dvi.xrefs in
  let otherwise = function
    | Dvi.C_fnt n ->
        let (mt, gt) =
          try
            let cfont = Table.get cdvi.font_table n in
            (cfont.mtable, get_gtable cfont sdpi)
          with Not_found -> (dummy_mtable, dummy_gtable) in
        mtable := mt;
        gtable := gt
    | Dvi.C_set code ->
        begin try ignore (Table.get !mtable code) with _ -> () end;
        begin try ignore (Table.get !gtable code) with _ -> () end
    | _ -> () in

  let headers = ref []
  and xrefs = cdvi.base_dvi.Dvi.xrefs in
  let globals = headers, xrefs in
  for n = 0 to Array.length cdvi.base_dvi.Dvi.pages - 1 do
    mtable := dummy_mtable;
    gtable := dummy_gtable;
(*
    ignore (scan_special_page otherwise cdvi globals n);
*)
  done;;
(*
  Dev.add_headers (find_prologues !headers);;
*)



