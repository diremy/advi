open Special;;
open DrDvi;;

type proc_unit = {
    escaped_register : reg_set;
    escaped_stack : reg_set list;
    escaped_cur_font : cooked_font;
    escaped_cur_mtable : (int * int) Table.t;
    escaped_cur_gtable : GrGlyph.t Table.t;
    mutable escaped_commands : Dvi.command list
  };;

type recording = { tag : string; unit : proc_unit};;

type state = {
    procs : (string, proc_unit) Hashtbl.t;
    mutable current_recording_proc : recording list; 
    mutable visible : bool;
    mutable visible_stack : bool list;
    mutable playing : int
  } 
;;

let is_recording pst = pst.current_recording_proc <> [];;

let record_command pst c =
  let record r =
    let u = r.unit in
    match c with
    (* The advi: proc specials are not recorded *)
    (* | Dvi.C_xxx s when has_prefix "advi: proc" s -> () *)
    | _ -> u.escaped_commands <- c :: u.escaped_commands 
  in
  List.iter record pst.current_recording_proc
;;

let create () =
  {
   procs= Hashtbl.create 17;
   current_recording_proc = [];
   visible= true;
   visible_stack = [];
   playing = 0
 } 
;;

  
(* will be replaced by Driver.eval_command *)
let special eval_command st s =
  let dst = st#dvi in
  let pst = st#proc in
  let records = get_records s in
  try
    let v = List.assoc "record" records in
    match v with
    | "start" ->
        let procname =
          try unquote (List.assoc "proc" records)
          with Not_found -> raise (Failure "proc: invalid special") in
        pst.visible_stack <- pst.visible :: pst.visible_stack;
        pst.visible <- List.mem_assoc "play" records;
        if pst.playing = 0 then
          let recording =
            { tag = procname;
              unit =
                { escaped_register = get_register_set dst;
                  escaped_stack = dst.stack;
                  escaped_cur_mtable = dst.cur_mtable;
                  escaped_cur_gtable = dst.cur_gtable;
                  escaped_cur_font = dst.cur_font;
                  escaped_commands = [] }
            } in
          pst.current_recording_proc <- recording :: pst.current_recording_proc
    | "end" ->
        if pst.playing = 0 then begin
          match pst.current_recording_proc with
          | [] -> Misc.warning (Printf.sprintf "'xxx %s' not recording" s)
          | recording :: rest ->
              let procname = recording.tag in
              pst.current_recording_proc <- rest;
              let u = recording.unit in
              Hashtbl.add pst.procs procname u;
              match u.escaped_commands with
              | h :: rest -> u.escaped_commands <- List.rev rest
              | [] -> assert false
        end;
        begin match pst.visible_stack with
        | h :: rest ->
            pst.visible <- h; 
	    pst.visible_stack <- rest;
        | [] ->
            (* Ill-formed DVI not recording error should have ben reported
               right above *)
            ();
        end;
    | _ -> ill_formed_special s
  with
  | Not_found ->
      let procname =
        try unquote (List.assoc "proc" records)
        with Not_found -> raise (Failure "proc: invalid special") in
      try
        ignore (List.assoc "play" records);
        if not (is_recording pst) then begin
          let us = Hashtbl.find_all pst.procs procname in
          let escaped_cur_font = dst.cur_font
          and escaped_cur_mtable = dst.cur_mtable
          and escaped_cur_gtable = dst.cur_gtable in
          let escaped_stack = push dst; dst.stack in
          pst.playing <- pst.playing + 1;
          List.iter
            (fun u ->
               set_register_set dst u.escaped_register;
               dst.stack <- u.escaped_stack;
               dst.cur_mtable <- u.escaped_cur_mtable;
               dst.cur_gtable <- u.escaped_cur_gtable;
               dst.cur_font <- u.escaped_cur_font;
               List.iter (eval_command st) u.escaped_commands
            ) us;
          pst.playing <- pst.playing - 1;
          dst.stack <- escaped_stack; 
	  pop dst;
          dst.cur_mtable <- escaped_cur_mtable;
          dst.cur_gtable <- escaped_cur_gtable;
          dst.cur_font <- escaped_cur_font;
        end
      with
      | Not_found ->
          Misc.warning
            (Printf.sprintf "xxx '%s': %s not recorded" s procname);;

