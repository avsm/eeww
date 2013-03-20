(* Test creating a tun interface *)

open Tuntap

let () =
  if Array.length Sys.argv < 2 then
    Printf.eprintf "Usage: %s [add|del] <args...>\n" Sys.argv.(0)
  else
    match Sys.argv.(1) with
    | "add" -> let _, name = opendev ~persist:true ?devname:(try Some Sys.argv.(2) with _ -> None) Tap in
      Printf.printf "Created interface %s, exiting now.\n" name
    | "del" -> let _, name = opendev ~persist:false ~devname:Sys.argv.(2) Tap in
      Printf.printf "Destroyed interface %s, exiting now.\n" name
    | "hwaddr" -> Printf.printf "%s\n" (string_of_hwaddr (get_hwaddr Sys.argv.(2)))
    | _ -> Printf.eprintf "Usage: %s [add|del] <args...>\n" Sys.argv.(0)
