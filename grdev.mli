type color = Dvicolor.color

module Symbol :
  sig
    type glyph = Grglyph.t
    and fontname = string
    and fontratio = float
    and g =
      Symbol.Make(Grglyph).g = {
      fontname : string;
      fontratio : float;
      glyph : glyph;
    } 
    and symbol =
      Symbol.Make(Grglyph).symbol =
        Glyph of g
      | Space of int * int
      | Rule of int * int
      | Line of int * string option
    and element =
      Symbol.Make(Grglyph).element = {
      color : int;
      locx : int;
      locy : int;
      code : int;
      symbol : symbol;
    } 
    and set = element list
    val voffset : element -> int
    val hoffset : element -> int
    val height : element -> int
    val width : element -> int
    val clear : unit -> unit
    val add : int -> int -> int -> int -> symbol -> unit
    val to_ascii : set -> string
    val to_escaped : set -> string
    val inzone : int -> int -> int -> int -> set
    val intime : int -> int -> int -> int -> set
    val iter : (element -> unit) -> set -> unit
    type region = Symbol.Make(Grglyph).region
    val position : int -> int -> region
    val new_region : region -> int -> int -> region
    val iter_region : (element -> unit) -> region -> unit
    val iter_regions :
      (element -> unit) -> (element -> unit) -> region -> region -> unit
    val apply : (glyph -> int -> int -> int -> unit) -> element -> unit
    val lines :
      int ->
      int ->
      (element * int * int * string * string * string * string *
       string option)
      option
    val word : int -> int -> (region * string) option
    val region_to_ascii : region -> string
  end

val open_dev : Ageometry.t -> unit
val close_dev : unit -> unit
val resize_dev : Ageometry.t -> unit
val clear_dev : unit -> unit
