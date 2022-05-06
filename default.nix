{ ocamlPackages
, nodePackages
, ocamlformat
, pname
}:
ocamlPackages.buildDunePackage {
  inherit pname;
  useDune2 = true;
  version = "0.1";
  src = ./.;
  minimumOCamlVersion = "4.08";
  buildInputs = with ocamlPackages; [
    utop
    nodePackages.ocaml-language-server
    merlin
    ounit2
    ocamlformat
  ];

  buildPhase = "dune build -p ${pname}";

  checkPhase = "dune build -p ${pname}";

  doCheck = true;
}
