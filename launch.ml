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

(* Unix command line parser *)
let parse_shell_command = Rc.argv_of_string;;

(* Handling forking problems: only father process can call the at_exit
   function, sons of the main process must leave without calling it.
   Otherwise we would attempt to kill embedded processes twice,
   leading to bus errors or bad exception handling (fatal errors). *)
let advi_process = Unix.getpid ();;

let exit code =
  (* at_exit code must be called only by the ADVI process.
     If it is one of the forked processes, it must DIE IMMEDIATELY:
     no cleaning is allowed. *) 
  if Unix.getpid () = advi_process then Pervasives.exit code
  else (* SUICIDE *) Unix.kill (Unix.getpid ()) 9;;

type policy =
   | Safer              (* No application is launched. *)
   | Exec               (* Application are automatically launched. *)
   | Ask                (* The user is prompted, whenever an
                           application has to be launched. *)
;;

let policy = ref Ask;;

(* Policy assignment. *)
let set_policy = function
  | Safer -> policy := Safer
  | Exec -> policy := Exec
  | Ask -> policy := Ask
;;

Options.add
  "-exec"
  (Arg.Unit
    (fun () ->
      Misc.warning "Setting policy to -exec";
      set_policy Exec))
  "\tExec mode: allow all external applications to be executed";;

Options.add
  "-safer"
  (Arg.Unit
    (fun () ->
      Misc.warning "Setting policy to -safer";
      set_policy Safer))
  "\tSafer mode: external applications are never launched";;

Options.add
  "-ask"
  (Arg.Unit
    (fun () ->
      Misc.warning "Setting policy to -ask";
      set_policy Ask))
  "\tAsk mode: ask confirmation before launching an external application";;

let cannot_execute_command command_invocation =
    Misc.warning
      (Printf.sprintf
         "Attempt to launch the embedded command:\n\n\
          \t%s\n\n\
          For security reasons, it was not executed.\n\
          Hence the presentation could be strange or incomplete.\n\
          To enable execution of embedded applications,\n\
          please rerun advi with option -ask or -exec."
         command_invocation);;

(* Opening a terminal to ask something to the user. *)
open Gterm;;

let ask_user t s1 s2 s3 =
 vtab t 16; htab t 15; print_str t s1;
 vtab t 12; htab t 10; print_str t s2;
 vtab t 8; htab t 15; 
 let answer = Gterm.ask t s3 in
 match answer with
 | "yes" -> true
 | _ -> false;;

let ask_to_launch command command_invocation =
 let ncol, nlines = 80, 24 in
 let bw = 25 in

 let sx, sy = Graphics.text_size "X" in
 let wt, ht = sx * ncol, sy * nlines in
 let xc, yc =
  (Graphics.size_x () - wt) / 2, (Graphics.size_y () - ht) / 2 in

 (*prerr_endline "Asking before launching";*)

 let t =
   make_term_gen
     Graphics.green Graphics.black
     bw Graphics.red Graphics.black
     0x6FFFFF
     xc yc nlines ncol in
 Gterm.set_title t (Printf.sprintf "Active-DVI alert for %s" command);
 draw_term t;
 ask_user t
  "Attempt to launch the following command"
  command_invocation
  "Do you want to execute it ? <yes/no> ";;

let can_launch command command_invocation =
  match !policy with
  | Exec -> true
  | Safer -> false
  | Ask -> ask_to_launch command command_invocation;;

let can_execute_table = Hashtbl.create 11;;

let can_execute command_invocation command_tokens =
  let command = command_tokens.(0) in
  (*prerr_endline command;*)
  try Hashtbl.find can_execute_table command
  with
  | Not_found ->
      let b = can_launch command command_invocation in
      (*prerr_endline (Printf.sprintf "can = %b" b);*)
      Hashtbl.add can_execute_table command b;
      b;;

let can_execute_command command_invocation =
  let command_tokens = parse_shell_command command_invocation in
  can_execute command_invocation command_tokens;;

let execute_command can_exec command_invocation command_tokens =
  if can_exec then Unix.execvp command_tokens.(0) command_tokens
  else cannot_execute_command command_invocation;;

let fork_proc command_invocation command_tokens =
  let can_exec = can_execute command_invocation command_tokens in
  (*prerr_endline (Printf.sprintf "Can_exec %b" can_exec);*)
  let pid = Unix.fork () in
  if pid = 0 then
    begin (* child *)
      try
        execute_command can_exec command_invocation command_tokens;
	exit 0
      with
      | Unix.Unix_error (e, _, arg) ->
	  Misc.warning (Printf.sprintf "%s: %s" (Unix.error_message e) arg);
	  exit 127
    end;
  pid;;

let fork_process command_invocation =
  let command_tokens = parse_shell_command command_invocation in
  fork_proc command_invocation command_tokens;;

(* Support for white run via -n option *)

let whiterun_commands = ref []
and whiterun_flag = ref false;;

let whiterun () = !whiterun_flag;;

let add_whiterun_command command =
  whiterun_commands := command :: !whiterun_commands;;

let dump_whiterun_commands () =
  let unique l =
    List.fold_right
      (fun c acc ->
	match acc with [] -> [c]
	| c' :: r as cl -> if c = c' then cl else c :: cl)
      (List.sort compare l) [] in
  let comms = unique !whiterun_commands in
  List.iter (fun c -> prerr_endline c) comms;;

Options.add 
  "-n"
  (Arg.Unit (fun () -> whiterun_flag := true))
  "\tEchoes commands, but does not execute them.";;
