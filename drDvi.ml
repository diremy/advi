(* Basic state: 100% dvi stuffs *)

(*** Cooked fonts ***)

module Symbol = GrDev.Symbol;;
module DFont = Devfont.Make(GrGlyph);;

type cooked_font = {
    name : string;
    ratio : float;
    mtable : (int * int) Table.t;
    mutable gtables : (int * GrGlyph.t Table.t) list
  };;

let dummy_mtable = Table.make (fun _ -> raise Not_found);;
let dummy_gtable = Table.make (fun _ -> raise Not_found);;
let dummy_font =
  { name = "--nofont--"; ratio = 1.0; mtable = dummy_mtable; gtables = [] };;

let cook_font fdef dvi_res =
  let name = fdef.Dvi.name
  and sf = fdef.Dvi.scale_factor
  and ds = fdef.Dvi.design_size in
  let ratio = float sf /. float ds in
  let mtable =
    try DFont.find_metrics name (dvi_res *. ratio)
    with Not_found -> dummy_mtable in
  { name = name;
    ratio = ratio;
    mtable = mtable;
    gtables = [] };;

let get_gtable cfont sdpi =
  try List.assoc sdpi cfont.gtables
  with Not_found ->
    let dpi = ldexp (float sdpi) (-16) in
    let table =
      try DFont.find_glyphs cfont.name (dpi *. cfont.ratio)
      with Not_found -> dummy_gtable in
    cfont.gtables <- (sdpi, table) :: cfont.gtables;
    table;;

(*** Cooked DVI's ***)

type cooked_dvi = {
    base_dvi : Dvi.t;
    dvi_res : float;
    font_table : cooked_font Table.t
  };;

let cook_dvi dvi =
  let dvi_res = 72.27 in
  let build n =
    cook_font (List.assoc n dvi.Dvi.font_map) dvi_res in
  { base_dvi = dvi;
    dvi_res = dvi_res;
    font_table = Table.make build };;

(*** The rendering state ***)

type reg_set = {
    reg_h : int;
    reg_v : int;
    reg_w : int;
    reg_x : int;
    reg_y : int;
    reg_z : int
  };;
     
type state = {
  device : GrDev.dvidevice;
  cdvi : cooked_dvi;
  sdpi : int;
  conv : float;
  x_origin : int;
  y_origin : int;

  (* Current font attributes *)
  mutable cur_font : cooked_font;
  mutable cur_mtable : (int * int) Table.t;
  mutable cur_gtable : GrGlyph.t Table.t;

  (* Registers *)
  mutable h : int;
  mutable v : int;
  mutable w : int;
  mutable x : int;
  mutable y : int;
  mutable z : int;
  (* register stack *)
  mutable stack : reg_set list;
} 

let get_register_set st =
  { reg_h = st.h; reg_v = st.v;
    reg_w = st.w; reg_x = st.x;
    reg_y = st.y; reg_z = st.z }
;;

let set_register_set st rset =
  st.h <- rset.reg_h;
  st.v <- rset.reg_v;
  st.w <- rset.reg_w;
  st.x <- rset.reg_x;
  st.y <- rset.reg_y;
  st.z <- rset.reg_z
;;

let push st =
  st.stack <- (get_register_set st) :: st.stack
;;

let pop st =
  match st.stack with
  | [] -> ()
  | rset :: rest ->
      set_register_set st rset;
      st.stack <- rest
;;

let create device cdvi dpi xorig yorig =
  let mag = float cdvi.base_dvi.Dvi.preamble.Dvi.pre_mag /. 1000.0 in
  { device = device;
    cdvi = cdvi;
    sdpi = int_of_float (mag *. ldexp dpi 16);
    conv = mag *. dpi /. cdvi.dvi_res /. 65536.0;
    x_origin = xorig; y_origin = yorig;
    cur_mtable = dummy_mtable;
    cur_gtable = dummy_gtable;
    cur_font = dummy_font;
    h = 0; v = 0; w = 0; x = 0; y = 0; z = 0;
    stack = [];
  } 
;;
