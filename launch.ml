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

let unsafe =
  Options.flag false "-unsafe"
    "\tUnsafer mode: allow all external applications to be launched";;

let safe_commands =  [  ] 
(* unsafe: il faudrait le chemin absolu.
    [ "animate"; "display"; "xeyes"; "mpg123"; "advi"; "netscape"; ]
*)
;;

(* Support for white run via -n option *)

let whiterun_commands = ref []
and whiterun_flag = ref false;;
let whiterun () = !whiterun_flag;;

let add_whiterun_command command =
  whiterun_commands := command::!whiterun_commands;;
let dump_whiterun_commands () =
  let unique l =
    List.fold_right
      (fun c -> fun acc ->
	match acc with [] -> [c]
	| (c'::r as cl) -> if c=c' then cl else c::cl)
      (List.sort compare l) []
  in
  let comms = unique !whiterun_commands in
  List.iter (fun c -> prerr_endline c) comms;;

Options.set 
    "-n"
    (Arg.Unit (fun () -> whiterun_flag := true))
    "\tMake advi print the list of embedded commands in the file";;


let paranoid =
  Options.flag false "-safer"
    "\tSafer mode: external applications are never launched";;

let exec_command command args =
  if !paranoid || not !unsafe && not (List.mem command safe_commands) then
    Misc.warning
      (Printf.sprintf
         "By default, the command:

  %s %s

is not executed for security reasons. To enable launching 
any application, please rerun advi with the option -unsafe."
        command
        (String.concat " "
           (match Array.to_list args with [] -> [] | h::t -> t))
      )
  else
    Unix.execvp command args
;;

let fork_process command = 
  let command_tokens = parse_shell_command command in
  let pid = Unix.fork () in
  if pid = 0 then
    begin (* child *)
      try
        exec_command command_tokens.(0) command_tokens;
	exit 0
      with
      | Unix.Unix_error (e, _, arg) -> 
	  Misc.warning (Printf.sprintf "%s: %s" (Unix.error_message e) arg);
	  exit 127
    end;
  pid;;
