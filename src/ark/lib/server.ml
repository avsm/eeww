open Eio
open Capnp_rpc_lwt
module R = Ocluster_api.Raw

let process_out stdout_q stderr_q complete_u =
  let module P = R.Service.ProcessOut in
  P.local
  @@ object
       inherit P.service

       method stdout_impl params release_param_caps =
         let open P.Stdout in
         let buf = Params.chunk_get params in
         release_param_caps ();
         Eio.Stream.add stdout_q (Cstruct.of_string buf);
         Service.return @@ Service.Response.create_empty ()

       method stderr_impl params release_param_caps =
         let open P.Stderr in
         let buf = Params.chunk_get params in
         release_param_caps ();
         Eio.Stream.add stderr_q (Cstruct.of_string buf);
         Service.return @@ Service.Response.create_empty ()

       method complete_impl params release_param_caps =
         let open P.Complete in
         let exit_code = Params.exit_code_get params in
         release_param_caps ();
         Eio.Promise.resolve complete_u exit_code;
         Service.return @@ Service.Response.create_empty ()
     end

let process_in stdin_push =
  let module P = R.Service.ProcessIn in
  P.local
  @@ object
       inherit P.service

       method stdin_impl params release_param_caps =
         let open P.Stdin in
         let buf = Params.chunk_get params in
         release_param_caps ();
         stdin_push (Some (Cstruct.of_string buf));
         Service.return_empty ()

       method cancel_impl _params release_param_caps =
         let open! P.Cancel in
         release_param_caps ();
         stdin_push None;
         Service.return_empty ()
     end

module Eiox = struct

  let return =
      function
      | Ok () -> ()
      | Error (`Capnp _) ->
          Eio.traceln "error TODO"

  let stream_source ?(capacity=1) () =
    let q = Eio.Stream.create capacity in
    let src = object(self)
      val mutable data = []
      inherit Eio.Flow.source
      method read_into dst =
        let avail, src = Cstruct.fillv ~dst ~src:data in
        match avail with
        | 0 -> begin
          match Eio.Stream.take q with
          | None -> raise End_of_file
          | Some d ->
             data <- [d];
             self#read_into dst
        end
        | _ ->
          data <- src;
          avail
    end in
    src, (Eio.Stream.add q)

  let capnp_sink f =
    let sink = object
      inherit Eio.Flow.sink
     
      method copy src =
        let buf = Cstruct.create 4096 in
        try
          while true do
            let got = src#read_into buf in
            let chunk = Cstruct.to_string ~len:got buf in
            return @@ f chunk
          done
       with End_of_file -> ()

      method! write bufs =
        List.iter (fun buf -> return @@ f @@ Cstruct.to_string buf) bufs
    end in
    sink
end

let agent ~sw env =
  let module Agent = R.Service.Agent in
  Agent.local
  @@ object
       inherit Agent.service

       method exec_impl params release_param_caps =
         let open Agent.Exec in
         let command = Params.cmd_get params in
         release_param_caps ();
         let binary = R.Reader.Command.binary_get command in
         let args = R.Reader.Command.args_get_list command in
         traceln "exec: %s %s" binary (String.concat " " args);
         match Eio.Process.spawn ~sw env ~executable:binary args with
         | exception exn -> Service.fail "%s" (Printexc.to_string exn)
         | proc ->
             match Eio.Process.await proc with
             | `Signaled s -> Service.fail "Signal %d" s
             | `Exited exit_code ->
                  let response, results = Service.Response.create Results.init_pointer in
                  Results.exit_code_set results (Int32.of_int exit_code);
                  traceln "proc return %d" exit_code;
                  Service.return response

       method spawn_impl params release_param_caps =
         let open Agent.Spawn in
         let module PO = Ocluster_api.Client.Process.Out in
         let command = Params.cmd_get params in
         let pout = Params.pout_get params in
         release_param_caps ();
         match pout with
         | None -> Service.fail "must specify callback process"
         | Some pout ->
             let binary = R.Reader.Command.binary_get command in
             let args = R.Reader.Command.args_get_list command in
             traceln "spawn: %s %s" binary (String.concat " " args);
             let stdout = Eiox.capnp_sink (fun chunk -> PO.stdout ~chunk pout) in
             let stderr = Eiox.capnp_sink (fun chunk -> PO.stderr ~chunk pout) in
             let stdin, stdin_push = Eiox.stream_source () in
             match Eio.Process.spawn ~sw env ~stdin ~stdout ~stderr ~executable:binary args with
             | exception _exn -> Service.fail "TODO FAIL"
             | proc ->
                 let pin = process_in stdin_push in
                 Fiber.fork ~sw (fun () ->
                    match Eio.Process.await proc with
                    | `Signaled _ -> failwith "XXX"
                    | `Exited code ->
                       match PO.complete ~exit_code:(Int32.of_int code) pout with
                       | Ok () -> ()
                       | Error (`Capnp _) -> Eio.traceln "XXX await error" 
                       (* TODO this should be in a Switch.on_release in case the await is cancelled? *)
                 );
                 let response, results = Service.Response.create Results.init_pointer in
                 Results.pin_set results (Some pin);
                 Service.return response
     end

let cluster_member t =
  let module Cluster = R.Service.ClusterMember in
  Cluster.local
  @@ object
       inherit Cluster.service

       method register_impl params release_param_caps =
         let open Cluster.Register in
         let hostname = Params.hostname_get params in
         let callback = Params.callback_get params in
         let hostinfo =
           let hi = Params.hostinfo_get params in
           let of_sexp_exn fn conv =
             Sexplib.Sexp.of_string_conv_exn (fn hi) conv
           in
           try
             let os_version = R.Reader.HostInfo.os_version_get hi in
             if os_version = "" then
               raise (Failure "unable to parse OS version");
             let os_distrib =
               of_sexp_exn R.Reader.HostInfo.os_distrib_get
                 Osrelease.Distro.t_of_sexp
             in
             let arch =
               of_sexp_exn R.Reader.HostInfo.arch_get Osrelease.Arch.t_of_sexp
             in
             Ok
               Ocluster_api.Client.ClusterMember.
                 { os_version; os_distrib; arch }
           with
           | Failure msg -> Error (`Msg msg)
           | exn ->
               Error
                 (`Msg
                   (Printf.sprintf "Unable to parse hostinfo: %s"
                      (Printexc.to_string exn)))
         in
         release_param_caps ();
         match (callback, hostinfo) with
         | None, _ -> Service.fail "no callback specified"
         | _, Error (`Msg m) -> Service.fail "%s" m
         | Some callback, Ok hostinfo -> (
             Logs.info (fun l -> l "Registered %s" hostname);
             Capability.inc_ref callback;
             match Agents.register ~hostname ~hostinfo callback t with
             | Ok () -> Service.return_empty ()
             | Error (`Msg msg) ->
                 Capability.dec_ref callback;
                 (* TODO add agent unregister to decr cap *)
                 Service.fail "%s" msg )
     end

let cluster_user t =
  let module Cluster = R.Service.ClusterUser in
  Cluster.local
  @@ object
       inherit Cluster.service

       method find_impl params release_param_caps =
         let open Cluster.Find in
         let hostname = Params.hostname_get params in
         release_param_caps ();
         match hostname with
         | "" -> Service.fail "must specify a hostname"
         | hostname -> (
             match Agents.find ~hostname t with
             | None -> Service.fail "hostname not found"
             | Some agent ->
                 let response, results =
                   Service.Response.create Results.init_pointer
                 in
                 Results.callback_set results (Some agent.agent_cap);
                 Service.return response )

       method list_impl _params release_param_caps =
         let open Cluster.List in
         release_param_caps ();
         let _response, _results =
           Service.Response.create Results.init_pointer
         in
         let _agent_caps =
           Agents.list t |> List.map (fun a -> a.Agents.agent_cap)
         in
         (* let _ = Results.agents_set_list results agent_caps in *)
         Service.fail "TODO"
     end
