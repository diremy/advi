(***********************************************************************)
(*                                                                     *)
(*                             Active-DVI                              *)
(*                                                                     *)
(*                   Projet Cristal, INRIA Rocquencourt                *)
(*                                                                     *)
(*  Copyright 2003 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the GNU Lesser General Public License.          *)
(*                                                                     *)
(*  Pierre Weis                                                        *)
(*                                                                     *)
(*  Addons for programmed backgrounds                                  *)
(*                                                                     *)
(***********************************************************************)

(* $Id$ *)

val hgradient : Graphics.color -> Graphics.color -> Grdev.viewport -> unit;;
val vgradient : Graphics.color -> Graphics.color -> Grdev.viewport -> unit;;
val dgradient : Graphics.color -> Graphics.color -> Grdev.viewport -> unit;;
val d1gradient : Graphics.color -> Graphics.color -> Grdev.viewport -> unit;;
val d2gradient : Graphics.color -> Graphics.color -> Grdev.viewport -> unit;;
val cgradient : Graphics.color -> Graphics.color -> Grdev.viewport -> unit;;
val circgradient : Graphics.color -> Graphics.color -> Grdev.viewport -> unit;;
