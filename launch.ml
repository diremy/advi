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
let parse_shell_command str =
  let p c = c = ' ' in
  let arglist = Misc.split_string str p 0 in
  Array.of_list arglist;;

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
  else (* SUICIDE *)
  Unix.kill (Unix.getpid ()) 9;;

type policy =
   | Safer              (* No application is launched. *)
   | Exec               (* Application are automatically launched. *)
   | Ask                (* The user is prompted, whenever an
                           application has to be launched. *)
;;

let policy = ref Ask;;

(* Normal policy assignment. *)
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

let do_on_screen f x =
  Graphics.remember_mode false;
  GraphicsY11.display_mode true;
  let r = f x in
  Graphics.remember_mode true;
  GraphicsY11.display_mode false;
  r;;

let ask_user command_invocation =
 GraphicsY11.iter_subwindows (fun wid _ -> GraphicsY11.unmap_subwindow wid);
prerr_endline "Subwindows unmapped!";
 let answer = do_on_screen Gterm.ask_to_launch command_invocation in
 GraphicsY11.iter_subwindows (fun wid _ -> GraphicsY11.map_subwindow wid);
prerr_endline "Subwindows remapped!";
 answer;;
 (* Misc.warning ("Do you want to launch " ^ command_invocation); 
 false;;*)

let can_launch command_invocation = function
  | Exec -> true
  | Safer -> false
  | Ask -> ask_user command_invocation;;

let execute_command can_exec command_invocation command_tokens =
  if can_exec then Unix.execvp command_tokens.(0) command_tokens
  else cannot_execute_command command_invocation;;

let fork_process command_invocation = 
  let command_tokens = parse_shell_command command_invocation in
  let can_exec = can_launch command_invocation !policy in
prerr_endline (Printf.sprintf "Can_exec %b" can_exec);
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
