open Lwt.Infix

module Calc = Examples.Calc

(* Verbose logging *)

let pp_qid f = function
  | None -> ()
  | Some x ->
    let s = Uint32.to_string x in
    Fmt.(styled `Magenta (fun f x -> Fmt.pf f " (qid=%s)" x)) f s

let reporter =
  let report src level ~over k msgf =
    let src = Logs.Src.name src in
    msgf @@ fun ?header ?(tags=Logs.Tag.empty) fmt ->
    let qid = Logs.Tag.find Capnp_rpc.Debug.qid_tag tags in
    let print _ =
      Fmt.(pf stdout) "%a@." pp_qid qid;
      over ();
      k ()
    in
    Fmt.kpf print Fmt.stdout ("%a %a: @[" ^^ fmt ^^ "@]")
      Fmt.(styled `Magenta string) (Printf.sprintf "%11s" src)
      Logs_fmt.pp_header (level, header)
  in
  { Logs.report = report }

(* Run as server *)

let serve vat_config =
  Lwt_main.run begin
    Capnp_rpc_unix.serve vat_config ~offer:Examples.Calc.local >>= fun vat ->
    Fmt.pr "Waiting for incoming connections at:@.%a@." Capnp_rpc_unix.Vat.pp_bootstrap_uri vat;
    fst @@ Lwt.wait ()
  end

(* Run as client *)

let connect addr =
  Lwt_main.run begin
    Capnp_rpc_unix.connect addr >>= fun calc ->
    Logs.info (fun f -> f "Evaluating expression...");
    let remote_add = Calc.getOperator calc `Add in
    let result = Calc.evaluate calc Calc.Expr.(Call (remote_add, [Float 40.0; Float 2.0])) in
    Calc.Value.read result >>= fun v ->
    Fmt.pr "Result: %f@." v;
    Lwt.return_unit
  end

(* Command-line parsing *)

open Cmdliner

let connect_addr =
  let i = Arg.info [] ~docv:"ADDR" ~doc:"Address of server (capnp://...)" in
  Arg.(required @@ pos 0 (some (Capnp_rpc_unix.sturdy_ref ())) None i)

let serve_cmd =
  Term.(const serve $ Capnp_rpc_unix.Vat_config.cmd),
  let doc = "provide a Cap'n Proto calculator service" in
  Term.info "serve" ~doc

let connect_cmd =
  Term.(const connect $ connect_addr),
  let doc = "connect to a Cap'n Proto calculator service" in
  Term.info "connect" ~doc

let default_cmd =
  let doc = "a calculator example" in
  Term.(ret (const (`Help (`Pager, None)))),
  Term.info "calc" ~version:"v0.1" ~doc

let cmds = [serve_cmd; connect_cmd]

let () =
  Fmt_tty.setup_std_outputs ();
  Logs.set_reporter reporter;
  Logs.set_level ~all:true (Some Logs.Info);
  match Term.eval_choice ~catch:false default_cmd cmds with
  | exception Failure msg -> Fmt.epr "%s@." msg; exit 1
  | status -> Term.exit status
