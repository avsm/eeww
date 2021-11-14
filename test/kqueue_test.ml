let%expect_test "create_kqueue" =
  let r, w = Unix.pipe () in
  let k = Kqueue.kqueue ~changelist_size:32 in
  Kqueue.add k r `Read;
  (match Kqueue.wait k ~ms:0 with
  | `Ok -> print_endline "Events available"
  | `Timeout -> print_endline "No available events");
  [%expect {| No available events |}];
  ignore (Unix.write_substring w "Hello" 0 5);
  (match Kqueue.wait k ~ms:0 with
  | `Ok -> print_endline "Events available"
  | `Timeout -> print_endline "No available events");
  [%expect {| Events available |}];
  Kqueue.iter_ready k ~f:(fun fd flag event ->
      Format.printf
        "(r = fd) = %b, event = %a, flags = %a"
        (r = fd)
        Kqueue.pp_event
        event
        Kqueue.Flag.pp
        flag);
  [%expect {| (r = fd) = true, event = read, flags = EV_ADD, EV_ONESHOT |}]
;;
