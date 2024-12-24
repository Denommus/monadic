{
  buildDunePackage,
  utop,
  ounit2,
  pname,
}:
buildDunePackage {
  inherit pname;
  useDune2 = true;
  version = "0.1";
  src = ./.;
  minimumOCamlVersion = "5.0";
  buildInputs = [
    utop
    ounit2
  ];

  buildPhase = "dune build -p ${pname}";

  checkPhase = "dune build -p ${pname}";

  doCheck = true;
}
