{ mkDerivation, aeson, base, servant, servant-snap, snap, snap-core
, snap-server, stdenv, text
}:
mkDerivation {
  pname = "testserver";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    aeson base servant servant-snap snap snap-core snap-server text
  ];
  license = stdenv.lib.licenses.bsd3;
}
