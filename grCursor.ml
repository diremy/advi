(* options *)
let show_busy =
  Options.flag true
    "-nowatch"
    "\tDon't display a watch when busy";;

let busy_delay = ref 0.5;;

Options.add
  "-watch"
  (Arg.Float (fun x -> busy_delay := x))
  (Printf.sprintf
    "FLOAT\tDelay before the watch cursor appears (default %fs)" !busy_delay);;

type mode =
  | Free | Busy | Pause | Disk | Question | Selection | Move
  | Resize | Resize_x | Resize_y
;;

let free = `LEFT_PTR;;
let busy = `WATCH;;
let pause = `RIGHT_SIDE;;
let disk = `EXCHANGE;;
let question = `QUESTION_ARROW;;
let selection = `XTERM;;
let move = `FLEUR;;
let resize = `CROSS;;
let resize_x = `SB_H_DOUBLE_ARROW;;
let resize_y = `SB_V_DOUBLE_ARROW;;

let select = function
  | Free -> free
  | Busy -> busy
  | Pause -> pause
  | Disk -> disk
  | Question -> question
  | Selection -> selection
  | Move -> move
  | Resize -> resize
  | Resize_x -> resize_x
  | Resize_y -> resize_y
;;

let mode = ref Free;;
let busy = ref false;;

(* To be called before system calls that may take a long time *)
let busy_timeouts = ref [];;

class t (window : Gdk.window) = object (self)
  method private raw_set cursor =
    (* memory leak ? *)
    Gdk.Window.set_cursor window (Gdk.Cursor.create cursor)

  method set m =
    mode := m; 
    if not !busy then self#raw_set (select m)

  method set_busy () =
    busy := true;
    if !show_busy then self#raw_set (select Busy)

  method unset_busy () =
    busy := false;
    if !show_busy then self#raw_set (select !mode)

  method init () = self#raw_set (select Free)


  (** Starts a timer which triggers the indication of a busy state. *)
  method private start_busy_timer () =
    busy_timeouts := 
      GMain.Timeout.add ~ms:(truncate (!busy_delay *. 1000.0))
	~callback: (fun () -> self#set_busy (); true) :: !busy_timeouts

  (* Stop the busy cursor, remove the timer if any and restore previous cursor. *)
  method private stop_busy_timer () =
    List.iter GMain.Timeout.remove !busy_timeouts;
    busy_timeouts := []

  method busyf : 'a 'b. ('a -> 'b) -> 'a -> 'b =
    fun f arg ->
      Misc.after (fun () ->
	self#start_busy_timer ();
	f arg)
	self#stop_busy_timer
end

