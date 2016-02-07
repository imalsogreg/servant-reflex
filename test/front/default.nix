{ mkDerivation, base, servant, servant-reflex, stdenv }:
mkDerivation {
  pname = "front";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [ base servant servant-reflex ];
  license = stdenv.lib.licenses.bsd3;
}
