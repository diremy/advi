open DrDvi;;
open DrProc;;
open Special;;

(* At this moment only KILL is sent *)
(*
let int_of_signal = function
  | "SIGABRT" | "sigabrt" -> Sys.sigabrt (* -1 *)
  | "SIGALRM" | "sigalrm" -> Sys.sigalrm (* -2 *)
  | "SIGFPE" | "sigfpe" -> Sys.sigfpe (* -3 *)
  | "SIGHUP" | "sighup" -> Sys.sighup (* -4 *)
  | "SIGILL" | "sigill" -> Sys.sigill (* -5 *)
  | "SIGINT" | "sigint" -> Sys.sigint (* -6 *)
  | "SIGKILL" | "sigkill" -> Sys.sigkill (* -7 *)
  | "SIGPIPE" | "sigpipe" -> Sys.sigpipe (* -8 *)
  | "SIGQUIT" | "sigquit" -> Sys.sigquit (* -9 *)
  | "SIGSEGV" | "sigsegv" -> Sys.sigsegv (* -10 *)
  | "SIGTERM" | "sigterm" -> Sys.sigterm (* -11 *)
  | "SIGUSR1" | "sigusr1" -> Sys.sigusr1 (* -12 *)
  | "SIGUSR2" | "sigusr2" -> Sys.sigusr2 (* -13 *)
  | "SIGCHLD" | "sigchld" -> Sys.sigchld (* -14 *)
  | "SIGCONT" | "sigcont" -> Sys.sigcont (* -15 *)
  | "SIGSTOP" | "sigstop" -> Sys.sigstop (* -16 *)
  | "SIGTSTP" | "sigtstp" -> Sys.sigtstp (* -17 *)
  | "SIGTTIN" | "sigttin" -> Sys.sigttin (* -18 *)
  | "SIGTTOU" | "sigttou" -> Sys.sigttou (* -19 *)
  | "SIGVTALRM" | "sigvtalrm" -> Sys.sigvtalrm (* -20 *)
  | "SIGPROF" | "sigprof" -> Sys.sigprof (* -21 *)
  | "" -> Sys.sigquit
  | s -> int_of_string s;;
*)

let kill_embed_special st pst s =
(*
  (* advi: kill name=? signal=? *)
*)
  (* advi: kill name=? *)
  let records = get_records s in
  let app_name =
    try unquote (List.assoc "name" records)
    with Not_found -> raise (Failure ("No command to kill in " ^ s)) in
(*
  let sign = List.assoc "signal" records in
  (* prerr_endline (Printf.sprintf "Signal is ``%s''" sign); *)
  let sig_val =
    try int_of_signal (unquote (List.assoc "signal" records))
    with
    | Not_found -> raise (Failure ("No signal to kill command in " ^ s))
    | Failure _ -> raise (Failure ("Cannot understand signal in " ^ s))  in
*)
  st#dvi.device#embed#kill app_name;;

let app_mode_of_string = function
  | "sticky" -> GrEmbed.Sticky
  | "persistent" -> GrEmbed.Persistent
  | "ephemeral" | "respawn" -> GrEmbed.Respawn
  | s -> raise (Failure ("Unknown embedding mode " ^ s));;

let special st s =
  (* advi: embed mode=? width=? height=? command="command string" *)
  let records = get_records s in
  let app_mode =
    try app_mode_of_string (List.assoc "mode" records)
    with Not_found ->
      raise (Failure ("embed: no embedding mode in special " ^ s)) in
  let app_name =
    try unquote (List.assoc "name" records) with Not_found -> "" in
  let command =
    try unquote (List.assoc "command" records)
    with Not_found ->
        raise (Failure ("embed: no command to embed in special " ^ s)) in
  (* prerr_endline ("embed command=" ^ command); *)
  let get_dim dim records =
    match Dimension.normalize
            (Dimension.dimen_of_string (List.assoc dim records)) with
    | Dimension.In d -> d
    | _ -> assert false in

  let width_pixel, height_pixel =
    let w, h =
      try
        let width = get_dim "width" records in
        let height = get_dim "height" records in
        width, height
      with
      | _ -> raise (Failure ("embed: no size in special " ^ s)) in
    let dpi = ldexp (float st#dvi.sdpi) (-16) in
    let width_pixel = truncate (w *. dpi) in
    let height_pixel = truncate (h *. dpi) in
  (* prerr_endline (Printf.sprintf "%d x %d pixel" width_pixel height_pixel);*)
    width_pixel, height_pixel in
  let x = st#dvi.x_origin + int_of_float (st#dvi.conv *. float st#dvi.h)
  and y = st#dvi.y_origin + int_of_float (st#dvi.conv *. float st#dvi.v) in
  if st#proc.visible then
    st#dvi.device#embed#launch ~name: app_name ~mode: app_mode ~x ~y 
      ~width: width_pixel ~height: height_pixel command
;;

(*
(* When scanning the page, we gather information on the embedded commands *)
let scan_embed_special st s =
  let records = get_records s in
  let command =
    try unquote (List.assoc "command" records)
    with Not_found ->
        raise (Failure ("embed: no command to embed in special " ^ s)) in
  Launch.add_whiterun_command command ;;
*)

