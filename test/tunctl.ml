(* Test creating a tun interface *)

open Tuntap

let () =
  if Array.length Sys.argv < 2 then
    Printf.eprintf "Usage: %s [add|del|inet|hwaddr] <args...>\n" Sys.argv.(0)
  else
    match Sys.argv.(1) with
    | "add" -> let _, name = opentun ~persist:true ?devname:(try Some Sys.argv.(2) with _ -> None) Tap in
      Printf.printf "Created interface %s, exiting now.\n" name
    | "del" -> let _, name = opentun ~persist:false ~devname:Sys.argv.(2) Tap in
      Printf.printf "Destroyed interface %s, exiting now.\n" name
    | "inet" -> set_ipv4 Sys.argv.(2) Sys.argv.(3); set_up_and_running Sys.argv.(2)
    | "hwaddr" -> Printf.printf "%s\n" (string_of_hwaddr (get_hwaddr Sys.argv.(2)))
    | _ -> Printf.eprintf "Usage: %s [add|del] <args...>\n" Sys.argv.(0)
