
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

let paranoid =
  Options.flag false "-safer"
    "\tRun in safe mode: no external applications are launched";;

let exec_command command args =
  if !paranoid then
     Misc.warning (Printf.sprintf "Attempt to launch %s" command) else
  Unix.execvp command args;;

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

