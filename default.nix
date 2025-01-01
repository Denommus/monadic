{
  buildDunePackage,
  ounit2,
  pname,
  melange,
}:
buildDunePackage {
  inherit pname;
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
