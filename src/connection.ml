open Eio
let server sw mgr net =
  let sock = Net.listen ~backlog:100 ~reuse_addr:true ~sw net (`Tcp (Net.Ipaddr.V4.any, 9999)) in
  Net.run_server sock ~on_error:(fun exn -> Eio.traceln "%s" (Printexc.to_string exn)) (fun s _ ->
    Eio.Domain_manager.run mgr (fun () ->
      Eio.traceln "Hmmm?";
      Unix.sleepf 2.;
      Eio.traceln "Done sleeping";
      Flow.copy_string "HTTP/1.1 200 OK\r\nDate: Mon, 24 Oct 2022 16:12:15 GMT\r\ncontent-length: 1\r\ncontent-type: text/plain\r\n\r\na" s;
      Eio.traceln "Done!"
    )
  )

let () =
  Eio_main.run @@ fun env ->
  Eio.traceln "RUNNING";
  Switch.run @@ fun sw ->
  server sw env#domain_mgr env#net