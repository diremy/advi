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
(*  Jun Furuse, Didier Rémy and Pierre Weis.                           *)
(*  Contributions by Roberto Di Cosmo, Didier Le Botlan,               *)
(*  Xavier Leroy, and Alan Schmitt.                                    *)
(*                                                                     *)
(*  Based on Mldvi by Alexandre Miquel.                                *)
(***********************************************************************)

(* $Id$ *)

module Make
    (G :
       sig
         type t
         val hoffset : t -> int
         val voffset : t -> int
         val width : t -> int
         val height : t -> int
       end) : 
 sig

   type glyph = G.t
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

   val commands_to_ascii: (int * Dvicommands.font_def) list -> Dvicommands.command list -> string

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
        
        
        
        
        
