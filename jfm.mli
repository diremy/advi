type preamble
type header

type char_info = {
  width_index : int;
  height_index : int;
  depth_index : int;
  italic_index : int;
  tag : int;
  remainder : int;
} 

type jfm = {
  preamble : preamble;
  header : header;
  char_types : (int, int) Hashtbl.t;
  char_infos : char_info array;
  widths : int array;
  heights : int array;
  depths : int array;
  italics : int array;
  gluekerns : string;
  glues : int array;
  kerns : int array;
  params : string;
} 

val load_jfm_file : string -> jfm
val find_width : jfm -> int -> int

val monospace_fix : (int * float) list
