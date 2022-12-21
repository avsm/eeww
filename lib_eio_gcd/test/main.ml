open Eio

let setup_log () =
  Fmt_tty.setup_std_outputs ();
  Logs.set_level (Some Debug);
  Logs.set_reporter (Logs_fmt.reporter ())

let () = setup_log ()

type exn += Graceful_shutdown

let file_io env =
  let test = Path.(env#fs / "test.txt") in 
  let _write =
    Eio.Path.(with_open_out ~create:(`If_missing 0o644) test) @@ fun f ->
    Flow.copy_string "Copying `Hello GCD!' to file\n" env#stdout;
    Flow.copy_string "Hello GCD!\n" f
  in
  let _read = 
    Eio.Path.(with_open_in test) @@ fun f ->
    let buf = Buffer.create 16 in
    Flow.copy f Flow.(buffer_sink buf);
    Eio.traceln "Read test.txt: %s" (Buffer.contents buf)
  in
    Eio.Path.unlink test

let read_all flow =
  let b = Buffer.create 100 in
  Eio.Flow.copy flow (Eio.Flow.buffer_sink b);
  Buffer.contents b

let run_client ~sw ~net ~addr =
  traceln "Connecting to server...";
  let flow = Eio.Net.connect ~sw net addr in
  Eio.Flow.copy_string "Hello from client" flow;
  Eio.Flow.shutdown flow `Send;
  let msg = read_all flow in
  traceln "Client received: %S" msg

let run_server ~sw socket =
  while true do
    Eio.Net.accept_fork socket ~sw (fun flow _addr ->
      traceln "Server accepted connection from client";
      Fun.protect (fun () ->
        let msg = read_all flow in
        traceln "Server received: %S" msg
      ) ~finally:(fun () -> Eio.Flow.copy_string "Bye" flow)
    )
    ~on_error:(function
      | Graceful_shutdown -> ()
      | ex -> traceln "Error handling connection: %s" (Printexc.to_string ex)
    );
  done

let test_address addr ~net sw =
  let server = Eio.Net.listen net ~sw ~reuse_addr:true ~backlog:5 addr in
  Fiber.both
    (fun () -> run_server ~sw server)
    (fun () ->
      traceln "client";
      run_client ~sw ~net ~addr;
      traceln "Client finished - cancelling server";
      raise Graceful_shutdown
    )

let network env =
  Eio.Switch.run @@ fun sw ->
  test_address (`Tcp (Net.Ipaddr.V4.loopback, 9999)) ~net:env#net sw

let forking _env =
  Eio.Switch.run @@ fun sw ->
  let p, r = Eio.Promise.create () in
  let q, s = Eio.Promise.create () in
  traceln "Starting fork 1 ...";
  Fiber.fork ~sw (fun () -> Promise.await p; Promise.resolve s ());
  traceln "Starting fork 2 ...";
  Fiber.fork ~sw (fun () -> Promise.resolve r (); Promise.await q);
  Eio.traceln "Done!"

let random env =
  let buf = Cstruct.create 16 in
  Eio.traceln "Before fill: %a" Cstruct.hexdump_pp buf;
  Flow.read_exact env#secure_random buf;
  Eio.traceln "After fill: %a" Cstruct.hexdump_pp buf

let timer env =
  Eio.traceln "First print this and wait 2 seconds";
  Eio.Time.sleep env#clock 2.;
  Eio.traceln "Done!"

let big_flow_copy env =
  let big_file = Path.(env#fs / "big") in
  let _file =
    Eio.Path.(with_open_out ~create:(`If_missing 0o644) big_file) @@ fun f ->
    Flow.copy_string (String.make 12_000 'a') f
  in
  let _file =
    Eio.Path.(with_open_in big_file) @@ fun f ->
    Flow.copy f env#stdout
  in
  Path.unlink big_file;
  ()

let () = 
  Eio_gcd.run @@ fun env ->
  (* file_io env *)
  (* network env *)
  (* forking env *)
  (* random env *)
  (* timer env *)
  big_flow_copy env