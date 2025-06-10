{
  description = "Perkelang development environment";

  inputs = {
    nixpkgs.url = "nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = import nixpkgs { inherit system; };
        ocamlPkgs = pkgs.ocamlPackages;
      in {
        devShells.default = pkgs.mkShell {
          name = "perkelang-dev";

          buildInputs = [
            ocamlPkgs.ocaml
            pkgs.dune_3
            ocamlPkgs.findlib
            ocamlPkgs.menhir
            ocamlPkgs.menhirLib
            ocamlPkgs.ppx_deriving
            ocamlPkgs.sedlex
            pkgs.opam
            pkgs.gcc
            pkgs.SDL2
            pkgs.vsce
            pkgs.nodePackages.typescript
            pkgs.nodejs
          ];

          shellHook = ''
            export OPAMROOT="$(pwd)/_opam"
            export OPAMYES=1
            export OPAMSWITCH=default

            if [ ! -d "$OPAMROOT" ]; then
              echo "Initializing local opam switch in ./_opam..."
              opam init --disable-sandboxing --no-setup
              eval "$(opam env)"
            else
              eval "$(opam env)"
            fi

            echo "Local opam switch ready (in ./_opam)"
          '';
        };

        packages.default = pkgs.stdenv.mkDerivation {
          pname = "perkc";
          version = "0.1.0";

          src = builtins.path {
            path = ./.;
            name = "perkelang-src";
          };

          nativeBuildInputs = [
            pkgs.dune_3
            ocamlPkgs.ocaml
            ocamlPkgs.findlib
            ocamlPkgs.menhir
            ocamlPkgs.menhirLib
            ocamlPkgs.ppx_deriving
            ocamlPkgs.sedlex
          ];

          buildPhase = ''
            dune build @install
          '';

          installPhase = ''
            mkdir -p $out/bin
            cp _build/default/bin/perkc.exe $out/bin/perkc
          '';

        };
      });
}
