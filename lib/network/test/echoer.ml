let main_queue = Dispatch.Queue.main ()

let stdin_echoer () =
  (* let group = Dispatch.Group.create () in *)
  let stdin = Dispatch.Io.create Stream Unix.stdin main_queue in
  (* Dispatch.Group.enter group; *)
  Dispatch.Io.with_read ~queue:main_queue ~off:0 ~length:8192
    ~f:(fun ~err:_ ~finished:_ _data -> print_endline "DATA SENDING")
    stdin

(* let () = Dispatch.(Group.wait (stdin_echoer ()) @@ Time.forever ()) |> ignore *)

let () =
  stdin_echoer ();
  Dispatch.main ()
