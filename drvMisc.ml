type cooked_font = {
    name : string;
    ratio : float;
    mtable : (int * int) Table.t;
    mutable gtables : (int * GrGlyph.t Table.t) list
  };;

(*** Cooked DVI's ***)

type cooked_dvi = {
    base_dvi : Dvi.t;
    dvi_res : float;
    font_table : cooked_font Table.t
  };;

type reg_set = {
    reg_h : int;
    reg_v : int;
    reg_w : int;
    reg_x : int;
    reg_y : int;
    reg_z : int
  };;
     
