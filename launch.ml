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

let children = Hashtbl.create 17;;

let exit code =
  (* at_exit code must be called only by the ADVI process.
     If it is one of the forked processes, it must DIE IMMEDIATELY:
     no cleaning is allowed. *) 
  if Unix.getpid () = advi_process then Pervasives.exit code
  else (* SUICIDE *) Unix.kill (Unix.getpid ()) 9;;

let failed_to_execute_command command_invocation =
    Misc.warning
      (Printf.sprintf
         "Attempt to launch the embedded command:\n\n\
          \t%s\n\n\
          The execution of the command was failed.\n\
          Hence the presentation could be strange or incomplete."
         command_invocation)
;;

let rejected_to_execute_command command_invocation =
    Misc.warning
      (Printf.sprintf
         "Attempt to launch the embedded command:\n\n\
          \t%s\n\n\
          For security reasons, it was not executed.\n\
          Hence the presentation could be strange or incomplete.\n\
          To enable execution of embedded applications,\n\
          please rerun advi with option -ask or -exec."
         command_invocation)
;;

let execute_command command_invocation command_tokens =
  try
    Unix.execvp command_tokens.(0) command_tokens
  with
  | _ -> failed_to_execute_command command_invocation
;;

let fork_proc command_invocation command_tokens =
  let pid = Unix.fork () in
  if pid = 0 then begin (* child *)
    try
      execute_command command_invocation command_tokens;
      exit 0
    with
    | Unix.Unix_error (e, _, arg) ->
	Misc.warning (Printf.sprintf "%s: %s" (Unix.error_message e) arg);
	exit 127
  end;
  pid
;;

let fork_process command_invocation =
  let command_tokens = parse_shell_command command_invocation in
  let pid = fork_proc command_invocation command_tokens in
  Hashtbl.add children pid command_invocation;
  pid
;;

let wait () =
  (* gather zombies *)
  while
    try
      let pid', _ = Unix.waitpid [Unix.WNOHANG] 0 in
      if pid' <> 0 then begin
(*
	Misc.warning
	  (Printf.sprintf "Launch.kill: death of %d is reported" pid');
*)
	try
	  Hashtbl.remove children pid'
	with
	| Not_found -> 
	    Misc.warning
	      (Printf.sprintf "Launch.kill: dead %d is not my child" pid');
      end;
      pid' <> 0
    with
      Unix.Unix_error(Unix.ECHILD, _, _) -> false
  do () done
;;

let kill ~signal pid =
  if not (Hashtbl.mem children pid) then begin
    Misc.warning
      (Printf.sprintf "failed to remove application %d" pid)
  end else begin
    begin try Unix.kill pid signal with _ -> 
      (* already dead *)       
      Misc.warning
	(Printf.sprintf "Launch.kill: application %d is already dead" pid)
    end;
    wait ()
  end
;;      

  
