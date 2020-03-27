{ mkDerivation, aeson, attoparsec, base, bytestring, dhall
, directory, filepath, hashable, lens, pretty-simple, prettyprinter
, prettyprinter-ansi-terminal, stdenv, text, unordered-containers
}:
mkDerivation {
  pname = "generate-dhall-vmadm";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    aeson attoparsec base bytestring dhall directory filepath hashable
    lens pretty-simple prettyprinter prettyprinter-ansi-terminal text
    unordered-containers
  ];
  description = "Generate Dhall interface for SmartOS vmadm";
  license = stdenv.lib.licenses.bsd3;
}
