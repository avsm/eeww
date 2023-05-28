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

let run_client hostname pty cluster env =
  let cmd = ("/usr/bin/bash", [| "bash";  |]) in
  Capability.with_ref cluster @@ fun t ->
  let agent = Client.ClusterUser.find ~hostname t in
  let stdout_q = Eio.Stream.create 100 in
  let stderr_q = Eio.Stream.create 100 in
  let on_complete_t, on_complete_u = Eio.Promise.create () in
  let pout = Ark.Server.process_out stdout_q stderr_q on_complete_u in
  let pin = Client.Agent.spawn ~pty cmd pout agent in
  let stdin = Ark.Eiox.capnp_sink (fun chunk -> Ark_api.Client.Process.In.stdin ~chunk pin) in
  let stdout = Ark.Eiox.stream_source stdout_q in
  let pty_fn = if pty then Ark.Eiox.run_with_raw_term else Ark.Eiox.run in
  pty_fn env ~stdin ~stdout on_complete_t

let connect uri hostname pty =
  Eio_main.run @@ fun env ->
  Mirage_crypto_rng_eio.run (module Mirage_crypto_rng.Fortuna) env @@ fun () ->
  Eio.Switch.run @@ fun sw ->
  Logs.info (fun l -> l "Connecting to cluster service at: %a" Uri.pp_hum uri);
  let client_vat = Capnp_rpc_unix.client_only_vat ~sw (Eio.Stdenv.net env) in
  let sr = Capnp_rpc_unix.Vat.import_exn client_vat uri in
  let proxy_to_service = Sturdy_ref.connect_exn sr in
  let exit_code = run_client hostname pty proxy_to_service env in
  exit (Int32.to_int exit_code)

open Cmdliner

let pty =
  let info = Arg.info ["t"] ~doc:"Force allocation of a pseudoterminal" in
  Arg.value (Arg.flag info)

let connect_addr =
  let i = Arg.info [] ~docv:"ADDR" ~doc:"Address of server (capnp://...)" in
  Arg.(required @@ pos 0 (some Capnp_rpc_unix.sturdy_uri) None i)

let hostname =
  let i = Arg.info [] ~docv:"HOSTNAME" ~doc:"Hostname of this node to register with cluster" in
  Arg.(required @@ pos 1 (some string) None i)

let connect_cmd =
  let doc = "run the remote shell" in
  Cmd.v (Cmd.info "ark-shell" ~doc) Term.(const connect $ connect_addr $ hostname $ pty)

let () =
  Fmt_tty.setup_std_outputs ();
  Logs.set_reporter reporter;
  Logs.set_level ~all:true (Some Logs.Info);
  exit (Cmd.eval connect_cmd)
