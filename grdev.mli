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

(* Private glyphs *)

type glyph;;

val make_glyph : Glyph.t -> glyph;;
val get_glyph  : glyph -> Glyph.t;;
val draw_glyph : glyph -> int -> int -> unit;;

module Symbol :
    sig
      type fontname = string
      type fontratio = float
      type g =
          { fontname : string;
            fontratio : float;
            glyph : glyph;
          }
      type symbol =
          Glyph of g
        | Space of int * int
        | Rule of int * int
        | Line of int * string option
      type element =
          { color   : int;
            locx    : int;
            locy    : int;
            code    : int;
            symbol : symbol }
      type set
      val voffset : element -> int
      val hoffset : element -> int
      val height : element -> int
      val width : element -> int
      val clear : unit -> unit
      val add : int -> int -> int -> int -> symbol -> unit
      val to_ascii   : set -> string
      val to_escaped : set -> string
      val commands_to_ascii: (int * Dvicommands.font_def) list -> Dvicommands.command list -> string
      val inzone : int -> int -> int -> int -> set
      val intime : int -> int -> int -> int -> set
      val iter : (element -> unit) -> set -> unit
      val lines : int -> int ->
        (element * int * int *
           string * string * string * string * string option) option
    end;;

(* Device configuration *)

val open_dev : string -> int * int;;
val close_dev : unit -> unit;;
val clear_dev : unit -> unit;;
val clear_usr1 : unit -> unit;;
val set_bbox : (int * int * int * int) option -> unit;;

(* Application embedding *)
val embed_app :
  string -> Embed.app_mode -> string -> int -> int -> int -> int -> unit;;

(* Drawing *)

type 'a rect = { x : 'a; y : 'a; h : 'a; w : 'a };;

type color = int;;
val fgcolor : unit -> color;;

val with_color : color -> ('a -> 'b) -> 'a -> 'b
val set_color : color -> unit;;
val push_bg_color : color -> unit;;
val pop_bg_color : unit -> unit;;
val fill_rect : int -> int -> int -> int -> unit;;

val draw_path : (int * int) array -> pensize:int -> unit;;
val fill_path : (int * int) array -> shade:float -> unit;;
val draw_arc :
  x:int -> y:int -> rx:int -> ry:int ->
  start:int -> stop:int -> pensize:int -> unit;;
val fill_arc :
  x:int -> y:int -> rx:int -> ry:int ->
  start:int -> stop:int -> shade:float -> unit;;

(* Alpha blending *)
val set_alpha : Drawimage.alpha -> unit;;
val set_epstransparent : bool -> unit;;
val set_epswithantialiasing : bool -> unit;;

val set_blend : Drawimage.blend -> unit;;
(** Fix the blending method (hence the blending function). *)

val draw_ps :
  string -> (int * int * int * int) -> (int * int) -> int -> int -> unit;;
val clean_ps_cache : unit -> unit;;
val sleep : float -> bool;; (* true= interrupted, false= fully performed *)

(* generic image drawing function *)

val draw_img :
  Misc.file_name ->
  Drawimage.ratiopts ->
  bool ->
  Drawimage.alpha ->
  Drawimage.blend ->
  Drawimage.ps_bbox option ->
    bool ->
   int * int -> int -> int -> unit;;
(** [draw_img fname ratio whitetrans alpha blend psbbox antialias (width, height) x y]
  draws the image contained in file [fname] at [x, y]. The image is
  resized to [(width, height)], blended with the background
  according to the blending option [blend], drawn with an alpha factor
  of [alpha] and a transparency color [whitetrans]. The [psbbox]
  argument is used to resize the image.
*)

(* Background information *)

(* Viewport:                *)
(* xsize,ysize,xoff,yoff    *)
(* in advi coordinates      *)

type viewport = (int*int*int*int);;

type bkgd_prefs = {
  mutable bgcolor : int;
  mutable bgimg : string option;
  mutable bgratio : Drawimage.ratiopts;
  mutable bgwhitetrans : bool;
  mutable bgalpha : float;
  mutable bgblend : Drawimage.blend;
  mutable bgviewport : viewport option;
  mutable bgfunction : (viewport -> unit) option;
};;

val blit_bkgd_data : bkgd_prefs -> bkgd_prefs -> unit;;
val copy_of_bkgd_data : unit -> bkgd_prefs;;
val default_bkgd_data : unit -> bkgd_prefs;;
val bkgd_data : bkgd_prefs;;
val get_playing : (unit -> int) ref;;

type bgoption =
   | BgColor of color
   | BgImg of Misc.file_name
   | BgAlpha of Drawimage.alpha
   | BgBlend of Drawimage.blend
   | BgRatio of Drawimage.ratiopts
   | BgViewport of viewport option
   | BgFun of (viewport -> unit) option
;;

val set_bg_options : bgoption list -> unit;;

(* Events *)

type status = {
    mouse_x : int;
    mouse_y : int;
    button : bool;
    keypressed : bool;
    key : char;
    modifiers : int;
  };;

module E : 
    sig
      type direction = X | Y | XY | Z
      type info = { comm : string; name : string;
                    line : string; file : string; 
                    origin : float rect; unit : float;
                    move : direction; resize : direction; }
      type figure = { rect : int rect; info : info; }
      type action = Move of int * int | Resize of int * int

      val clear : unit -> unit
      val switch_edit_mode : unit -> unit
      val editing : unit -> bool
      val add : int rect -> info -> unit
      val inside : int -> int -> figure -> bool
      val find : int -> int -> figure
      val tostring : figure -> action -> string
    end;;

module H :
    sig
      type mode = Over | Click_down
      type style = Box | Underline | Invisible
      type link =
          { link : string;
            action : (unit -> unit);
            mode : mode;
            style : style;
            color : color option;
            area : (int * int * int) option;
          }
      type tag =
        | Name of string
        | Href of string
        | Advi of link

      type anchor = {
          tag : tag;
          draw : (int * int * glyph) list
        }

      val add : anchor -> unit
      val area : tag -> int -> int -> int -> int -> unit
      val flashlight : tag -> unit

    end;;

type area = Bottom_right | Bottom_left | Top_right | Top_left | Middle;;
type button = Button1 | Button2 | Button3;;
type event =
    Resized of int * int
  | Refreshed
  | Key of char
  | Move of int * int
  | Edit of E.figure * E.action
  | Region of int * int * int * int
  | Selection of string
  | Position of int * int
  | Href of string
  | Advi of string * (unit -> unit) 
  | Click of area * button * int * int
  | Nil;;
val wait_event : unit -> event;;

exception Stop;;
exception GS;;
val continue : unit -> unit;;
val reposition : x:int -> y:int -> w:int -> h:int -> int * int;;
val exec_ps : string -> int -> int -> unit;;
val newpage : (bool * string) list -> int -> float -> int -> int -> unit;;
val add_headers : (bool * string) list -> unit;;
val current_pos : unit -> int * int;;
val synchronize : unit -> unit;;

val set_transition : Transitions.t -> unit;;

val transbox_save : int -> int -> int -> int -> unit;;
val transbox_go : Transitions.t -> unit;;

val set_title : string -> unit;;
val cut : string -> unit;;

val wait_button_up : unit -> unit;;
