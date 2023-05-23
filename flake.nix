{
  description = "Experimental effects-based wonderful webserver";

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};
        depexts = with pkgs; [
          # for libhdr_histogram
          cmake
          zlib
          gmp
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
        devShells.default = pkgs.mkShell {
          buildInputs = depexts;
        };
      });
}
