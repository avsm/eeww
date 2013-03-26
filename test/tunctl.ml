(* Test creating a tun interface *)

open Tuntap

let () =
  if Array.length Sys.argv < 2 then
    Printf.eprintf "Usage: %s [add|del|inet|hwaddr|ifconfig] <args...>\n" Sys.argv.(0)
  else
    match Sys.argv.(1) with
    | "add" -> let _, name = opentap ~persist:true ?devname:(try Some Sys.argv.(2) with _ -> None) () in
      Printf.printf "Created interface %s, exiting now.\n" name
    | "del" -> let _, name = opentap ~persist:false ~devname:Sys.argv.(2) () in
      Printf.printf "Destroyed interface %s (%s), exiting now.\n" name (string_of_hwaddr (get_hwaddr name))
    | "inet" -> set_ipv4 Sys.argv.(2) Sys.argv.(3); set_up_and_running Sys.argv.(2)
    | "hwaddr" -> Printf.printf "%s\n" (string_of_hwaddr (get_hwaddr Sys.argv.(2)))
    | "ifconfig" -> set_ipv4 Sys.argv.(2) Sys.argv.(3); set_up_and_running Sys.argv.(2)
    (* | "osxtest" -> let _, name = opentap ~devname:Sys.argv.(2) () in *)
    (*   set_ipv4 name "10.0.0.2"; *)
    (*   set_up_and_running name; *)
    (*   ignore(Unix.system("ifconfig")); *)
    | _ -> Printf.eprintf "Usage: %s [add|del] <args...>\n" Sys.argv.(0)
