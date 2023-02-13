let get_time () =
  let ic = Unix.open_process_in "date" in
  Fun.protect
    (fun () -> input_line ic)
    ~finally:(fun () -> Unix.close_process_in ic |> ignore)
;;

let make_ev event ident time =
  let open Kqueue.Event_list in
  Event.set_ident event ident;
  Event.set_filter event Kqueue.Filter.timer;
  Event.set_flags event Kqueue.Flag.(add + oneshot);
  Event.set_fflags event Kqueue.Note.seconds;
  Event.set_data event time;
  Event.set_udata event 0
;;

let process_event event =
  let open Kqueue.Event_list in
  let flags = Event.get_flags event in
  let data = Event.get_data event in
  if Kqueue.Flag.intersect flags Kqueue.Flag.error
  then (
    let msg = Printf.sprintf "Error event received: %d" data in
    raise (Failure msg));
  let filter = Event.get_filter event in
  if filter = Kqueue.Filter.timer
  then
    Printf.printf
      "%s : Timer event received for ident: %d, data: %d\n"
      (get_time ())
      (Event.get_ident event)
      data
  else Format.printf "Unknown event received: %a\n" Kqueue.Filter.pp filter
;;

let run () =
  let k = Kqueue.create () in
  for i = 1 to 5 do
    let changelist = Kqueue.Event_list.create 1 in
    let event = Kqueue.Event_list.get changelist 0 in
    make_ev event i 1;
    let n =
      Kqueue.kevent
        k
        ~changelist
        ~eventlist:Kqueue.Event_list.null
        Kqueue.Timeout.immediate
    in
    assert (n = 0);
    let eventlist = Kqueue.Event_list.create 1 in
    let n =
      Kqueue.kevent k ~changelist:Kqueue.Event_list.null ~eventlist Kqueue.Timeout.never
    in
    assert (n = 1);
    process_event (Kqueue.Event_list.get eventlist 0);
    flush stdout
  done
;;

let () = run ()
