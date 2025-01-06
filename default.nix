{
  buildDunePackage,
  ounit2,
  melange,
}:
buildDunePackage {
  pname = "monadic";
  duneVersion = "3";
  version = "0.1";
  src = ./.;
  minimumOCamlVersion = "5.0";

  buildInputs = [
    ounit2
  ];

  nativeBuildInputs = [ melange ];

  doCheck = true;
}
