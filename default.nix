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
  minimumOCamlVersion = "4.13";
  buildInputs = with ocamlPackages; [
    utop
    ounit2
  ];

  buildPhase = "dune build -p ${pname}";

  checkPhase = "dune build -p ${pname}";

  doCheck = true;
}
