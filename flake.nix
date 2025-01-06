{
  description = "A monad transformers library for OCaml, based on transformers and mtl";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs =
    {
      nixpkgs,
      flake-utils,
      ...
    }:
    flake-utils.lib.eachDefaultSystem (
      system:
      let
        pkgs = import nixpkgs {
          inherit system;
          overlays = [
            (final: prev: {
              ocamlPackages = prev.ocamlPackages.overrideScope (ofinal: oprev: {
                monadic = final.ocamlPackages.callPackage ./. { };
              });
            })
          ];
        };
      in
      {
        packages = {
          default = pkgs.ocamlPackages.monadic;
          inherit (pkgs.ocamlPackages) monadic;
        };

        devShell = pkgs.mkShell {
          inputsFrom = [
            pkgs.ocamlPackages.monadic
          ];
          buildInputs = with pkgs; [
            ocamlPackages.ocaml-lsp
            ocamlPackages.utop
            ocamlformat
          ];
        };
      }
    );
}
