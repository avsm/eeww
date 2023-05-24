open Capnp_rpc_net
open Ark

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

let clmember_cap_file = "cluster_member.cap"

let cluser_cap_file = "cluster_user.cap"

let write_cap vat sid file =
  match Capnp_rpc_unix.Cap_file.save_service vat sid file with
  | Error (`Msg msg) ->
      Logs.err (fun l -> l "Error writing cap file: %s" msg);
      exit 1
  | Ok () -> ()

let serve config =
  Eio_main.run @@ fun env ->
  Mirage_crypto_rng_eio.run (module Mirage_crypto_rng.Fortuna) env @@ fun () ->
  Eio.Switch.run @@ fun sw ->
  let agents = Agents.init () in
  let clmember_sid = Capnp_rpc_unix.Vat_config.derived_id config "cluster_member" in
  let clmember_cap = Server.cluster_member agents in
  let cluser_sid = Capnp_rpc_unix.Vat_config.derived_id config "cluster_user" in
  let cluser_cap = Server.cluster_user agents in
  (* TODO make_sturdy_id needs to also add hostname and fingerprint *)
  let make_sturdy id = Uri.make ~path:(Restorer.Id.to_string id) () in
  let table = Restorer.Table.create make_sturdy in
  Restorer.Table.add table clmember_sid clmember_cap;
  Restorer.Table.add table cluser_sid cluser_cap;
  let restore = Restorer.of_table table in
  let vat = Capnp_rpc_unix.serve ~sw ~net:(Eio.Stdenv.net env) config ~restore in
  write_cap vat clmember_sid clmember_cap_file;
  write_cap vat cluser_sid cluser_cap_file;
  Eio.traceln "Server running. Connect using either %S or %S." clmember_cap_file cluser_cap_file

open Cmdliner

let serve_cmd =
  Cmd.v 
    (Cmd.info "serve" ~doc:"run the server")
    (Term.(const serve $ Capnp_rpc_unix.Vat_config.cmd))

let () =
  Fmt_tty.setup_std_outputs ();
  Logs.set_reporter reporter;
  Logs.set_level ~all:true (Some Logs.Info);
  exit (Cmd.eval serve_cmd)
