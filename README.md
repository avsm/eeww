# kqueue-ml

OCaml bindings to [kqueue](https://en.wikipedia.org/wiki/Kqueue)

Timer example using kqueue:

```ocaml
open Ctypes
open Kqueue

let get_time () =
  let ic = Unix.open_process_in "date" in
  Fun.protect
    (fun () -> input_line ic)
    ~finally:(fun () -> Unix.close_process_in ic |> ignore)

let process_event event =
  ( if Unsigned.UInt16.(logand (Kevent.flags event) Bindings.ev_error > zero)
  then
    let msg =
      Printf.sprintf "Error event received: %d"
        (Kevent.data event |> Intptr.to_int)
    in
    raise (Failure msg) ) ;
  let filter = Kevent.filter event in
  if filter = Bindings.evfilt_timer then
    Printf.printf "%s : Timer event received for ident: %d, data: %d\n"
      (get_time ())
      (Kevent.ident event |> Uintptr.to_int)
      (Kevent.data event |> Intptr.to_int)
  else Printf.printf "Unknown event received: %d\n" filter

let run q =
  print_endline "Processing events" ;
  let rec aux () =
    let eventlist = CArray.make Bindings.Kevent.t 10 in
    (* wait for any output events from kevent. `kevent` call will block till we
       have any events to process. We can use the optional ?timeout param to
       have kevent return after the timeout period even if there are no events
       to report. *)
    let n = kevent q ~changelist:(CArray.make Bindings.Kevent.t 0) ~eventlist in
    match n with
    | -1 ->
        prerr_endline "Error during kevent" ;
        exit 1
    | 0 ->
        print_endline "Timeout reached" ;
        aux ()
    | n' ->
        for i = 0 to n' - 1 do
          let event = CArray.get eventlist i in
          process_event event ; flush stdout
        done ;
        aux ()
  in
  aux ()

let make_ev ident time =
  let flags = Unsigned.UInt16.logor Bindings.ev_add Bindings.ev_enable in
  Kevent.make ~ident:(Uintptr.of_int ident) ~filter:Kqueue.Bindings.evfilt_timer
    ~flags ~fflags:Bindings.note_seconds ~data:(Intptr.of_int time)
    ~udata:Uintptr.zero

let () =
  let q = kqueue () in
  let events = [make_ev 2 1; make_ev 3 5] in
  (* initilize 2 timer events and attach them to kqueue. changelist is a list of
     events to register with kqueue. eventlist is the array that will be filled
     by kqueue with events that are read from kqueue. Setting eventlist to 0
     ensures that kevent returns immediately without waiting for any output
     events/timeout. *)
  let res =
    kevent q
      ~changelist:(CArray.of_list Bindings.Kevent.t events)
      ~eventlist:(CArray.make Bindings.Kevent.t 0)
  in
  assert (res = 0) ;
  run q
```

## Caveats

This is mostly tested on macOS. At the moment the constant values in the bindings only refer to the values that are
a subset of kqueue constants available on macOS and freebsd.
