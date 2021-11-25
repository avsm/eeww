[%%import "config.h"]
[%%if defined KQUEUE_AVAILABLE && defined KQUEUE_ML_ARCH_SIXTYFOUR]

let%expect_test "kqueue available" =
  Printf.printf "Kqueue available: %b\n" Kqueue.available;
  [%expect {| Kqueue available: true |}]
;;

let%expect_test "kqueue timer" =
  let make_ev event ident time =
    let open Kqueue.Event_list in
    Event.set_ident event ident;
    Event.set_filter event Kqueue.Filter.timer;
    Event.set_flags event Kqueue.Flag.(add + oneshot);
    Event.set_fflags event Kqueue.Note.nseconds;
    Event.set_data event time;
    Event.set_udata event 0
  in
  let k = Kqueue.create () in
  for i = 1 to 5 do
    let changelist = Kqueue.Event_list.create 1 in
    let event = Kqueue.Event_list.get changelist 0 in
    make_ev event i (i * 1_000_000);
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
    let event = Kqueue.Event_list.get eventlist 0 in
    Format.printf
      "Received %a event for ident: %d\n"
      Kqueue.Filter.pp
      (Kqueue.Event_list.Event.get_filter event)
      (Kqueue.Event_list.Event.get_ident event)
  done;
  [%expect
    {|
    Received EVFILT_TIMER event for ident: 1
    Received EVFILT_TIMER event for ident: 2
    Received EVFILT_TIMER event for ident: 3
    Received EVFILT_TIMER event for ident: 4
    Received EVFILT_TIMER event for ident: 5 |}]
;;

[%%else]

let%expect_test "kqueue unavailable" =
  Printf.printf "Kqueue available: %b\n" Kqueue.available;
  [%expect {| Kqueue available: false |}]
;;

[%%endif]
