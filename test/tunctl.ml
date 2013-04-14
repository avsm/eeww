(* Test creating a tun interface *)

open Tuntap

let print_usage () = Printf.eprintf "Usage: %s [add|del|inet|hwaddr] <args...>\n"  Sys.argv.(0)

let () =
  if Array.length Sys.argv < 2 then print_usage ()
  else
    match Sys.argv.(1) with
    | "add" -> let _, name = opentap ~persist:true ?devname:(try Some Sys.argv.(2) with _ -> None) () in
      Printf.printf "Created interface %s, exiting now.\n" name
    | "del" -> let _, name = opentap ~persist:false ~devname:Sys.argv.(2) () in
      Printf.printf "Destroyed interface %s (%s), exiting now.\n" name (string_of_hwaddr (get_hwaddr name))
    | "inet" -> set_ipv4 ~dev:Sys.argv.(2) ~ipv4:Sys.argv.(3) ~netmask:(try Sys.argv.(4) with _ -> "") ()
    | "hwaddr" -> Printf.printf "%s\n" (string_of_hwaddr (get_hwaddr Sys.argv.(2)))
    | _ -> print_usage ()
