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

open Misc;;
open Texstack;;
open DrDvi;;

class state dvi color proc tpic = object
  method dvi = (dvi : DrDvi.state)
  method color = (color : DrColor.state)
  method proc = (proc : DrProc.state)
  method tpic = (tpic : DrTpic.state)
end
;;

(*** Specials ***)

let specials = Hashtbl.create 31;;

let add_special prefix f = Hashtbl.add specials prefix f;;

let special st s =
  Hashtbl.iter (fun pref f -> if has_prefix pref s then f st s) specials;;

(*** Page rendering ***)

let eval_dvi_command st = function
  | Dvi.C_set code -> DrRender.set st code
  | Dvi.C_put code -> DrRender.put st code
  | Dvi.C_set_rule(a, b) -> DrRender.set_rule st a b
  | Dvi.C_put_rule(a, b) -> DrRender.put_rule st a b
  | Dvi.C_push -> DrDvi.push st#dvi
  | Dvi.C_pop -> DrDvi.pop st#dvi
  | Dvi.C_right k -> (* add_blank 1 st k; *) st#dvi.h <- st#dvi.h + k
  | Dvi.C_w0 ->  (* add_blank 2 st st#dvi.w; *) st#dvi.h <- st#dvi.h + st#dvi.w
  | Dvi.C_w k -> st#dvi.w <- k; (* add_blank 3 st st#dvi.w; *) st#dvi.h <- st#dvi.h + st#dvi.w
  | Dvi.C_x0 ->  (* add_blank 4 st st#dvi.x; *) st#dvi.h <- st#dvi.h + st#dvi.x
  | Dvi.C_x k -> st#dvi.x <- k; (* add_blank 5 st st#dvi.x; *) st#dvi.h <- st#dvi.h + st#dvi.x
  | Dvi.C_down k -> st#dvi.v <- st#dvi.v + k
  | Dvi.C_y0 -> st#dvi.v <- st#dvi.v + st#dvi.y
  | Dvi.C_y k -> st#dvi.y <- k; st#dvi.v <- st#dvi.v + st#dvi.y
  | Dvi.C_z0 -> st#dvi.v <- st#dvi.v + st#dvi.z
  | Dvi.C_z k -> st#dvi.z <- k; st#dvi.v <- st#dvi.v + st#dvi.z
  | Dvi.C_fnt n -> DrRender.fnt st n
  | Dvi.C_xxx s -> special st s
  | _ -> ();;

let eval_command st c =
  DrProc.record_command st#proc c;
  eval_dvi_command st c;;

let render_step device cdvi num (* ?trans ?chst *) dpi xorig yorig =
  if num < 0 || num >= Array.length cdvi.base_dvi.Dvi.pages then
    invalid_arg "Driver.render_step";
  let mag = float cdvi.base_dvi.Dvi.preamble.Dvi.pre_mag /. 1000.0
  and page = cdvi.base_dvi.Dvi.pages.(num) in
  let otherwise _ = () in
(*
  let status =
    let headers = ref []
    and xrefs = cdvi.base_dvi.Dvi.xrefs in
    let s = scan_special_page otherwise cdvi (headers, xrefs) num in
    if !headers <> [] then
      Dev.add_headers (find_prologues !headers);
    s in
  if not !Options.dops then status.Dvi.hasps <- false;
*)
  let orid = function Some f -> f | None -> fun x->x in
  let st = new state (DrDvi.create device cdvi dpi xorig yorig)
                     (Texstack.create 0)
                     (DrProc.create ())
                     (DrTpic.create ())
  in
(*
      alpha = 1.0; alpha_stack = [];
      blend = GrImage.Normal; blend_stack = [];
      epstransparent = true; epstransparent_stack = [];
*)
(*
      direction = trans;
      transition = Transitions.TransNone;
      transition_stack = [];
*)
(*
      tpic_pensize = 0.0; tpic_path = []; tpic_shading = 0.0;
      status = (orid chst) status;
      headers = [];
      html = None;
      draw_html = [];
      checkpoint = 0;
*)
(*
  if st#status.Dvi.hasps then newpage [] st  (mag *. dpi) xorig yorig;
  setup_bkgd st#status; (* apply the background preferences in Dev *)
*)
  device#clear ();     (* and redraw the background *)
(*
  Dev.set_transition st#transition;
  st#checkpoint <- 0;
*)
(*
  let check () =
    begin try Dev.continue () with
    (* try with exn -> raise exn ?? What does that mean ? *)
    | Dev.Stop as exn -> raise exn
    end;
    st#checkpoint <- checkpoint_frequency in
*)
  let eval st x =
(*
    st#checkpoint <- st#checkpoint - 1;
*)
    let b = eval_command st x in
(*
    if st#checkpoint < 0 then check ();
*)
    b in
  Dvi.page_step (eval st) page;;

let scan_special_pages _ _ = ();;
let toggle_active () = ();;

(* Adding special handlers *)

add_special "color " DrColor.special;;
add_special "PSfile=" DrPsfile.special;;
add_special "psfile=" DrPsfile.special;;
add_special "advi: proc" (DrProc.special eval_command);;
add_special "advi: pause"  DrWait.special;;
add_special "advi: wait " DrWait.special;;
add_special "advi: embed " DrEmbed.special;;

List.iter (fun prefix -> 
  add_special prefix DrTpic.special)
    [ "pn "; "pa "; "fp"; "ip"; "da "; "dt "; "sp";
      "sp "; "ar "; "ia "; "sh "; "wh"; "bk" ]
;;
