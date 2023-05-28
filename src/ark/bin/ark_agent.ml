open Capnp_rpc_lwt
open Ark_api

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

let run_client ~mgr cluster hostname =
  Eio.Switch.run @@ fun sw ->
  Capability.with_ref (Ark.Server.agent ~sw mgr) @@ fun callback ->
  match Ark.Agents.hostinfo () with
  | Error (`Msg msg) ->
      Eio.traceln "hostinfo: %s" msg;
      exit 1
  | Ok hostinfo ->
      Client.ClusterMember.register ~hostname ~callback ~hostinfo cluster;
      Eio.traceln "Registered with cluster";
      (* TODO register sig handler for unregister *)
      Eio.Fiber.await_cancel ()

let connect uri hostname =
  Eio_main.run @@ fun env ->
  Mirage_crypto_rng_eio.run (module Mirage_crypto_rng.Fortuna) env @@ fun () ->
  Eio.Switch.run @@ fun sw ->
  Eio.traceln "Connecting to cluster service at: %a@." Uri.pp_hum uri;
  let client_vat = Capnp_rpc_unix.client_only_vat ~sw (Eio.Stdenv.net env) in
  let sr = Capnp_rpc_unix.Vat.import_exn client_vat uri in
  let proxy_to_service = Sturdy_ref.connect_exn sr in
  run_client ~mgr:(Eio.Stdenv.process_mgr env) proxy_to_service hostname

open Cmdliner

let hostname =
  let i = Arg.info [] ~docv:"HOSTNAME" ~doc:"Hostname of this node to register with cluster" in
  Arg.(required @@ pos 1 (some string) None i)

let connect_addr =
  let i = Arg.info [] ~docv:"ADDR" ~doc:"Address of server (capnp://...)" in
  Arg.(required @@ pos 0 (some Capnp_rpc_unix.sturdy_uri) None i)

let connect_cmd =
  Cmd.v (Cmd.info "agent" ~doc:"run the agent")
    Term.(const connect $ connect_addr $ hostname)

let () =
  Fmt_tty.setup_std_outputs ();
  Logs.set_reporter reporter;
  Logs.set_level ~all:true (Some Logs.Info);
  exit (Cmd.eval connect_cmd)
