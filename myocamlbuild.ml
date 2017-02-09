open Ocamlbuild_plugin

let () =
  dispatch begin function
  | After_rules ->
      flag ["link"; "ocaml"; "link_tuntap"]
        (A "lib/libtuntap_stubs.a");
      dep ["link"; "ocaml"; "use_tuntap"]
        ["lib/libtuntap_stubs.a"];
  | _ -> ()
  end
