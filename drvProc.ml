open Stack;;
open Special;;

type proc_unit = {
    escaped_register : DrvMisc.reg_set;
    escaped_stack : DrvMisc.reg_set list;
    escaped_cur_font : DrvMisc.cooked_font;
    escaped_cur_mtable : (int * int) Table.t;
    escaped_cur_gtable : GrGlyph.t Table.t;
    mutable escaped_commands : Dvi.command list
  };;

type recording = { tag : string; unit : proc_unit}

class state = object
  val visible_stack = 
    let st = Stack.create () in
    Stack.push true st;
    st

  method push_visible c = Stack.push c visible_stack
  method pop_visible = try ignore (Stack.pop visible_stack) with Empty -> ()
  method visible = Stack.top visible_stack

  val mutable playing = 0
  method playing = playing
  method set_playing v = playing <- v

  val mutable current_recording_proc = ([] : recording list)
  method current_recording_proc = current_recording_proc
  method set_current_recording_proc v = current_recording_proc <- v

  method is_recording = current_recording_proc <> []

  val procs = (Hashtbl.create 17 : (string, proc_unit) Hashtbl.t)
  method procs = procs
end

