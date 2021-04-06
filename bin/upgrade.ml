open! Stdune
open Import

let doc = "Upgrade jbuilder projects to dune"

let man =
  [ `S "DESCRIPTION"
  ; `P
      {|$(b,dune upgrade) upgrade all the jbuilder projects
         in the workspace to Dune|}
  ; `Blocks Common.help_secs
  ]

let info = Term.info "upgrade" ~doc ~man

let term =
  let+ common = Common.term in
  let config = Common.set_common common ~recognize_jbuilder_projects:true in
  Scheduler.go ~common ~config (fun () -> Dune_upgrader.upgrade ())

let command = (term, info)
