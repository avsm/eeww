{
  description = "Experimental effects-based wonderful webserver";

  inputs.opam-nix.url = "github:tweag/opam-nix";

  outputs = { self, nixpkgs, flake-utils, opam-nix }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};
        opam-nix-lib = opam-nix.lib.${system};
        depexts = with pkgs; [
          # for libhdr_histogram
          cmake
          zlib
          gmp
          openssl
          capnproto
        ];
      in {
        defaultPackage = pkgs.stdenv.mkDerivation {
          name = "eeww";
          src = self;
          buildInputs = depexts;
          dontUseCmakeConfigure = true;
          installPhase = ''
            mkdir $out
            cp _build/default/src/main.exe $out/
          '';
        };
        devShells.default =
          let scope = opam-nix-lib.queryToScope {} {
            ocaml-base-compiler = "5.0.0";
            dune = "3.7.0";
            ocaml-lsp-server = "1.15.0-5.0";
          };
          in
          pkgs.mkShell {
            buildInputs =
                [
                  scope.ocaml
                  scope.dune
                  scope.ocaml-lsp-server
                ]
                ++ depexts;
          };
      });
}
