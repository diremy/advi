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
    (* Register stack *)
    mutable stack : reg_set list;
    (* Color & Color stack *)
    mutable color : Dvicolor.color;
    mutable color_stack : Dvicolor.color list;
    (* Other attributes *)
(*
    mutable alpha : GrImage.alpha;
    mutable alpha_stack : GrImage.alpha list;
    mutable blend : GrImage.blend;
    mutable blend_stack : GrImage.blend list;
    mutable epstransparent : bool;
    mutable epstransparent_stack : bool list;
    mutable direction : Transitions.direction option;
    mutable transition : Transitions.t;
    mutable transition_stack : Transitions.t list;
*)
(*
    (* TPIC specials state *)
    mutable tpic_pensize : float;
    mutable tpic_path : (float * float) list;
    mutable tpic_shading : float;
*)
    (* PS specials page state *)
(*
    mutable status : Dvi.known_status;
    mutable headers : (bool * string) list;
*)
(*
    mutable html : (Dev.H.tag * int) option;
    mutable draw_html : (int * int * GrGlyph.t) list;
    mutable checkpoint : int;
*)
  };;
