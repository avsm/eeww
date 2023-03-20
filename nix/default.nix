{ stdenv, lib, ocamlPackages, doCheck ? false, nix-filter }:

with ocamlPackages;

buildDunePackage {
  pname = "eio-ssl";
  version = "0.0.1-dev";

  src = with nix-filter; filter {
    root = ./..;
    include = [
      "dune"
      "eio-ssl.opam"
      "dune-project"
      "src"
    ];
  };

  duneVersion = "3";

  propagatedBuildInputs = [ ssl eio ];
  inherit doCheck;

  meta = {
    description = "OpenSSL bindings to OCaml's EIO";
    license = lib.licenses.lgpl21;
  };
}
