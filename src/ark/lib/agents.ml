open Ocluster_api
(** Manage a cluster of agents registering *)

type agent = {
  hostname : string;
  agent_cap : Client.Agent.t;
  hostinfo : Client.ClusterMember.hostinfo;
}

type cluster = { mutable agents : agent list }

let hostinfo () =
  let arch = Osrelease.Arch.v () in
  let os_distrib = Osrelease.Distro.v () |> Result.get_ok in
  let os_version = Osrelease.Version.v () |> Result.get_ok in
  match os_version with
  | None -> Error (`Msg "Agent is unable to determine OS version")
  | Some os_version -> Ok { Client.ClusterMember.arch; os_distrib; os_version }

let init () =
  let agents = [] in
  let cluster = { agents } in
  Eio.traceln "Initialised cluster state";
  cluster

let find ~hostname t = List.find_opt (fun t -> t.hostname = hostname) t.agents

let register ~hostname ~hostinfo agent_cap t =
  let agent = { hostname; agent_cap; hostinfo } in
  match find ~hostname t with
  | Some _ -> Error (`Msg "Hostname already exists in cluster")
  | None ->
      t.agents <- agent :: t.agents;
      Eio.traceln "Registered agent %s" hostname;
      Ok ()

let list t = t.agents

(* TODO turn this into an enum / union on the wire *)
let exit_code_of_status = function
  | Unix.WEXITED c -> Int32.of_int c
  | Unix.WSIGNALED _ -> -1l (* TODO expose signal/stopped info? *)
  | Unix.WSTOPPED _ -> -2l
