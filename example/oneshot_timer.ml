open Ctypes
open Kqueue

let get_time () =
  let ic = Unix.open_process_in "date" in
  Fun.protect
    (fun () -> input_line ic)
    ~finally:(fun () -> Unix.close_process_in ic |> ignore)

let process_event i event =
  ( if Unsigned.UInt16.(logand (Kevent.flags event) Bindings.ev_error > zero)
  then
    let msg =
      Printf.sprintf "Error event received: %d"
        (Kevent.data event |> Intptr.to_int)
    in
    raise (Failure msg) ) ;
  let filter = Kevent.filter event in
  if filter = Bindings.evfilt_timer then
    Printf.printf "%d - %s : Timer event received for ident: %d, data: %d\n" i
      (get_time ())
      (Kevent.ident event |> Uintptr.to_int)
      (Kevent.data event |> Intptr.to_int)
  else Printf.printf "Unknown event received: %d\n" filter

let make_ev ident time =
  let flags = Unsigned.UInt16.logor Bindings.ev_add Bindings.ev_oneshot in
  Kevent.make ~ident:(Uintptr.of_int ident) ~filter:Kqueue.Bindings.evfilt_timer
    ~flags ~fflags:Bindings.note_seconds ~data:(Intptr.of_int time)
    ~udata:Uintptr.zero

let run q =
  print_endline "Processing events" ;
  for i = 1 to 5 do
    let events = [make_ev 2 1] in
    ignore
      (kevent_exn q
         ~changelist:(CArray.of_list Bindings.Kevent.t events)
         ~eventlist:(CArray.make Bindings.Kevent.t 0)) ;
    let eventlist = CArray.make Bindings.Kevent.t 1 in
    let n =
      kevent_exn q ~changelist:(CArray.make Bindings.Kevent.t 0) ~eventlist
    in
    assert (n = 1) ;
    process_event i (CArray.get eventlist 0) ;
    flush stdout
  done ;
  print_endline "Bye!"

let () = run (kqueue_exn ())
