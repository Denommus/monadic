{
  description = "A monad transformers library for OCaml, based on transformers and mtl";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils, ... }:
    flake-utils.lib.eachDefaultSystem(system:
      let
        pname = "monadic";
        pkgs = import nixpkgs {
          inherit system;
          overlays = [
            (final: prev: {
              "${pname}" = final.callPackage ./. { inherit pname; };
            })
          ];
        };
      in
        {
          packages = {
            "${pname}" = pkgs."${pname}";
          };
          defaultPackage = pkgs."${pname}";

          devShell = pkgs.mkShell {
            inputsFrom = with pkgs; [
              pkgs."${pname}"
            ];
            buildInputs = with pkgs; [
            ];
          };
        });
}
