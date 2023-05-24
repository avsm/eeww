open Capnp_rpc_lwt
open Ocluster_api

(* Verbose logging *)
let pp_qid f = function
  | None -> ()
  | Some x ->
      let s = Stdint.Uint32.to_string x in
      Fmt.(styled `Magenta (fun f x -> Fmt.pf f " (qid=%s)" x)) f s

let reporter =
  let report src level ~over k msgf =
    let src = Logs.Src.name src in
    msgf @@ fun ?header ?(tags = Logs.Tag.empty) fmt ->
    let qid = Logs.Tag.find Capnp_rpc.Debug.qid_tag tags in
    let print _ =
      Fmt.(pf stdout) "%a@." pp_qid qid;
      over ();
      k ()
    in
    Fmt.kpf print Fmt.stdout
      ("%a %a: @[" ^^ fmt ^^ "@]")
      Fmt.(styled `Magenta string)
      (Printf.sprintf "%11s" src)
      Logs_fmt.pp_header (level, header)
  in
  { Logs.report }

let run_client ~sw cluster stdin =
  let hostname = "alpha" in
  let cmd = ("/usr/bin/bash", [| "bash";  |]) in
  Capability.with_ref cluster @@ fun t ->
  let agent = Client.ClusterUser.find ~hostname t in
  let stdout_q = Eio.Stream.create 100 in
  let stderr_q = Eio.Stream.create 100 in
  let on_complete_t, on_complete_u = Eio.Promise.create () in
  let pout = Ocluster.Server.process_out stdout_q stderr_q on_complete_u in
  let pin = Client.Agent.spawn cmd pout agent in
  let print_stream q =
   while true do
     Eio.Stream.take q |> Cstruct.to_string |> print_endline
   done
  in
  Eio.Fiber.fork ~sw (fun () -> print_stream stdout_q);
  Eio.Fiber.fork ~sw (fun () -> print_stream stderr_q);
  Eio.Flow.copy stdin (Ocluster.Server.Eiox.capnp_sink (fun chunk -> Ocluster_api.Client.Process.In.stdin ~chunk pin));
  let exit_code = Eio.Promise.await on_complete_t in
  (* TODO flush stdout *)
  Logs.info (fun l -> l "exit code %ld" exit_code);
  (* TODO triggers shutdown trace *)
  (* exit (Int32.to_int exit_code) *)
  Eio.Fiber.await_cancel ()

let connect uri =
  Eio_main.run @@ fun env ->
  Mirage_crypto_rng_eio.run (module Mirage_crypto_rng.Fortuna) env @@ fun () ->
  Eio.Switch.run @@ fun sw ->
  Logs.info (fun l -> l "Connecting to cluster service at: %a" Uri.pp_hum uri);
  let client_vat = Capnp_rpc_unix.client_only_vat ~sw (Eio.Stdenv.net env) in
  let sr = Capnp_rpc_unix.Vat.import_exn client_vat uri in
  let proxy_to_service = Sturdy_ref.connect_exn sr in
  Eio.Switch.run @@ fun sw ->
  run_client ~sw proxy_to_service (env#stdin)

open Cmdliner

let connect_addr =
  let i = Arg.info [] ~docv:"ADDR" ~doc:"Address of server (capnp://...)" in
  Arg.(required @@ pos 0 (some Capnp_rpc_unix.sturdy_uri) None i)

let connect_cmd =
  let doc = "run the client" in
  Cmd.v (Cmd.info "connect" ~doc) Term.(const connect $ connect_addr)

let () =
  Fmt_tty.setup_std_outputs ();
  Logs.set_reporter reporter;
  Logs.set_level ~all:true (Some Logs.Info);
  exit (Cmd.eval connect_cmd)
