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

(* Embedding applications (in particular tcl/tk) *)

type app_mode = Sticky | Persistent | Ephemeral;;

let app_table = Hashtbl.create 17;;

(* The function that launches all embedded applications.
   When encountering an embedded application, a call to raw_embed_app
   is stored in the list of application to launch at next pause
   in the [embeds] list reference.

   This function allocates a (sub)window for the application and tries
   to launch the application into this window.
*)
let raw_embed_app command app_mode app_name width height x gry =

 if Launch.can_execute_command command then begin

  let wid = GraphicsY11.open_subwindow ~x ~y:gry ~width ~height in

  (*** !x commands
    !p : embedding target window id (in digit)

      If !p is not specified, the applications will be treated by WM.
      (If they are X apps, of course...)

    !g : geometry like 100x100+20+30
    !w : width  of the target window in pixel
    !h : height of the target window in pixel
    !x : x of the application against the root
    !y : y of the application against the root

    Why "!"?  '\' is for TeX. "%" is for TeX. "$" is for TeX...
  ***)

  let command0 = Misc.string_replace "!p" wid command in

  (* If there is no !p, the application geometry will be treated
     by the WM. In such cases, we try to fix the geometry
     so that it is against the root. *)

  let opt_geometry, opt_x, opt_y =
    let px, py =
      let against_root = command0 = command in
      if against_root then
        (* fix the geometry *)
        let (ww, wh, wx, wy) = GraphicsY11.get_geometry () in
        wx + x, wy + (wh - gry) - height
      else 0, 0 in
    Printf.sprintf "%dx%d+%d+%d" width height px py,
    string_of_int px,
    string_of_int py in

  let command =
    Misc.string_replace "!g" opt_geometry
        (Misc.string_replace "!w" (string_of_int width)
            (Misc.string_replace "!h" (string_of_int height)
               (Misc.string_replace "!x" opt_x
                  (Misc.string_replace "!y" opt_y
                     command0)))) in
  let pid = Launch.fork_process command in
  if Hashtbl.mem app_table pid then
    raise (Failure
             (Printf.sprintf
               "pid %d is already in the app_table!" pid));
  Hashtbl.add app_table pid (app_mode, app_name, wid)
 end;;

(* In hash table t, returns the first element that verifies p. *)
let hashtbl_find t p =
  let res = ref None in
  try
   Hashtbl.iter (fun k x -> if p x then (res := Some (k, x); raise Exit)) t;
   raise Exit
  with Exit ->
   match !res with
   | None -> raise Not_found
   | Some k_x -> k_x;;

let find_embedded_app app_name =
  hashtbl_find app_table (fun (_, name, _) -> name = app_name);;

let map_embed_app command app_mode app_name width height x y =
  try
    let _, (app_mode, app_name, wid) = find_embedded_app app_name in
    GraphicsY11.map_subwindow wid
  with Not_found -> ();;

let unmap_embed_app command app_mode app_name width height x gry =
  try
    let _, (app_mode, app_name, wid) = find_embedded_app app_name in
    GraphicsY11.unmap_subwindow wid
  with Not_found -> ();;

let move_or_resize_persistent_app
    command app_mode app_name width height x gry =
  try
    let _, (app_mode, app_name, wid) = find_embedded_app app_name in
    GraphicsY11.resize_subwindow wid width height;
    let gry = gry + height - width in
    GraphicsY11.move_subwindow wid x gry
  with Not_found -> ();;

(* In hash table t, verifies that at least one element verifies p. *)
let hashtbl_exists t f =
  try Hashtbl.iter (fun _ x -> if f x then raise Exit) app_table; false
  with Exit -> true;;

(* embedded apps must be displayed when synced *)
let embed_app command app_mode app_name width height x gry =
  let already_launched app_name =
    hashtbl_exists app_table (fun (ty, name, wid) -> name = app_name) in
  match app_mode with
  | Sticky ->
     if not (already_launched app_name) then
     Launch.add_embed
      (fun () ->
        (* prerr_endline ("Launching " ^ app_name); *)
        raw_embed_app command app_mode app_name width height x gry) else
     Launch.add_persist
      (fun () ->
        (* prerr_endline ("Moving " ^ app_name); *)
        move_or_resize_persistent_app command app_mode app_name
        width height x gry)
  | Persistent ->
     if not (already_launched app_name) then
     Launch.add_embed
      (fun () ->
        (* prerr_endline ("Launching " ^ app_name); *)
        raw_embed_app command app_mode app_name width height x gry);
     Launch.add_persist
      (fun () ->
        (* prerr_endline ("Mapping " ^ app_name); *)
        map_embed_app command app_mode app_name width height x gry);
     Launch.add_unmap_embed
      (fun () ->
        (* prerr_endline ("Unmapping " ^ app_name); *)
        unmap_embed_app command app_mode app_name width height x gry)
  | Ephemeral ->
     Launch.add_embed
      (fun () ->
        (* prerr_endline ("Launching " ^ app_name); *)
        raw_embed_app command app_mode app_name width height x gry);;

let kill_app pid wid =
  (* prerr_endline (Printf.sprintf "kill_app (pid=%d, window=%s)" pid wid); *)
  begin try Hashtbl.remove app_table pid with _ -> 
    Misc.warning
      (Printf.sprintf "kill_app failed to remove application %d..." pid)
  end;
  begin try Unix.kill pid 9 with _ -> 
    (* prerr_endline
       (Printf.sprintf
          "kill_app (pid=%d,window=%s): process already dead" pid wid); *)
    ()
  end;
  while
    try
      let pid', _ = Unix.waitpid [Unix.WNOHANG] 0 in
      pid' <> 0
    with
      Unix.Unix_error(Unix.ECHILD, _, _) -> false
  do () done;
  (* prerr_endline (Printf.sprintf "kill_app (pid=%d, window=%s)" pid wid); *)
  (* if this is the forked process, do not close the window!!! *)
  if Unix.getpid () = Launch.advi_process then GraphicsY11.close_subwindow wid
;;

let kill_apps app_mode =
  (* begin match app_mode with
  | Persistent -> prerr_endline "Killing persistent apps"
  | Sticky -> prerr_endline "Killing sticky apps"
  | Ephemeral -> prerr_endline "Killing ephemeral apps"
  end; *)
  let to_be_killed =
    Hashtbl.fold (fun pid (apt, app_name, wid) acc ->
      if apt = app_mode then (pid, wid) :: acc else acc) app_table []
  in
  List.iter (fun (pid, wid) -> kill_app pid wid) to_be_killed;;

let signal_app sig_val pid wid =
  (* prerr_endline
    (Printf.sprintf
      "signal_app (pid=%d, window=%s) signal=%i killing=%b kill is %i"
      pid wid sig_val (sig_val = Sys.sigquit) Sys.sigquit); *)
  if sig_val = Sys.sigquit then kill_app pid wid else
  try Unix.kill pid sig_val with _ ->
    (* prerr_endline
        (Printf.sprintf
          "signal_app (pid=%d, window=%s) signal=%i: cannot signal process"
          pid wid sig_val); *)
    ()
;;

let kill_embedded_app sig_val app_name =
  (* prerr_endline
   (Printf.sprintf
     "kill_embedded_app (signal=%i app_name=%s)"
     sig_val app_name); *)
  try
    let pid, (app_mode, app_name, wid) = find_embedded_app app_name in
    signal_app sig_val pid wid
  with
  | Not_found ->
      Misc.warning (Printf.sprintf "application %s is not running" app_name)
;;

let kill_ephemeral_apps () =
  kill_apps Ephemeral;;

let kill_persistent_apps () =
  kill_apps Sticky;
  Launch.unmap_persistent_apps ();
  kill_apps Persistent;;

let kill_all_embedded_apps () =
  kill_ephemeral_apps ();
  kill_persistent_apps ();;
