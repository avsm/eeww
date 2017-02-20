open Ocamlbuild_plugin

let () =
  dispatch begin function
  | After_rules ->
      flag ["link"; "library"; "ocaml"; "byte"; "use_tuntpo"]
        (S ([A "-dllib"; A "-ltuntap_stubs"]));
      flag ["link"; "library"; "ocaml"; "native"; "use_tuntap"]
        (S ([A "-cclib"; A "-ltuntap_stubs"]));
      flag ["link"; "ocaml"; "link_tuntap"]
        (A "lib/libtuntap_stubs.a");
      dep ["link"; "ocaml"; "use_tuntap"]
        ["lib/libtuntap_stubs.a"];
  | _ -> ()
  end
