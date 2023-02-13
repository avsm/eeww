let get_time () =
  let ic = Unix.open_process_in "date" in
  Fun.protect
    (fun () -> input_line ic)
    ~finally:(fun () -> Unix.close_process_in ic |> ignore)
;;

let process_event event =
  let open Kqueue.Event_list in
  let flag = Event.get_flags event in
  let data = Event.get_data event in
  if Kqueue.Flag.(intersect flag error)
  then (
    let msg = Printf.sprintf "Error event received: %d" data in
    failwith msg);
  let filter = Event.get_filter event in
  if Kqueue.Filter.(filter = timer)
  then
    Printf.printf
      "%s : Timer event received for ident: %d\n"
      (get_time ())
      (Event.get_ident event)
  else Format.printf "Unknown event received: %a\n" Kqueue.Filter.pp filter
;;

let run k =
  print_endline "Processing events";
  let eventlist = Kqueue.Event_list.create 5 in
  let rec loop () =
    let n =
      Kqueue.kevent
        k
        ~changelist:Kqueue.Event_list.null
        ~eventlist
        Kqueue.Timeout.immediate
    in
    assert (n >= 0);
    for i = 0 to n - 1 do
      let event = Kqueue.Event_list.get eventlist i in
      process_event event;
      flush stdout
    done;
    loop ()
  in
  loop ()
;;

let make_ev event ident time =
  let open Kqueue.Event_list in
  Event.set_ident event ident;
  Event.set_filter event Kqueue.Filter.timer;
  Event.set_flags event Kqueue.Flag.add;
  Event.set_fflags event Kqueue.Note.seconds;
  Event.set_data event time;
  Event.set_udata event 0
;;

let () =
  let k = Kqueue.create () in
  let changelist = Kqueue.Event_list.create 3 in
  make_ev (Kqueue.Event_list.get changelist 0) 1 1;
  make_ev (Kqueue.Event_list.get changelist 1) 2 5;
  make_ev (Kqueue.Event_list.get changelist 2) 3 10;
  let n =
    Kqueue.kevent k ~changelist ~eventlist:Kqueue.Event_list.null Kqueue.Timeout.never
  in
  assert (n = 0);
  run k
;;
