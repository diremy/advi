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

(* Cautious policy assignment: -exec does not override -safer,
   and ask can override -safer. *)
let cautious_set_policy = function
  | Safer -> policy := Safer
  | Exec ->
     if !policy = Ask then policy := Exec
  | Ask ->
     if !policy = Exec ||
        !policy = Safer then policy := Ask
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

let ask_user command_invocation =
 Misc.warning ("Do you want to launch " ^ command_invocation); 
 false;;

let execute_command command_invocation command_tokens =
  let rec exec_command command_invocation command_tokens = function
    | Exec -> Unix.execvp command_tokens.(0) command_tokens
    | Ask ->
        if ask_user command_invocation
        then exec_command command_invocation command_tokens Exec
        else exec_command command_invocation command_tokens Safer
    | Safer ->
        cannot_execute_command command_invocation in

  exec_command command_invocation command_tokens !policy;;

let fork_process command_invocation = 
  let command_tokens = parse_shell_command command_invocation in
  let pid = Unix.fork () in
  if pid = 0 then
    begin (* child *)
      try
        execute_command command_invocation command_tokens;
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
