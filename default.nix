{ mkDerivation, aeson, base, bytestring, case-insensitive
, containers, data-default, exceptions, generic-lens, ghcjs-dom, http-api-data
, http-media, jsaddle, mtl, network-uri, reflex, reflex-dom-core
, safe, scientific, servant, servant-auth, stdenv
, string-conversions, text, transformers
}:
mkDerivation {
  pname = "servant-reflex";
  version = "0.3.4";
  src = builtins.filterSource
        (path: type:
        baseNameOf path != "result"
        && baseNameOf path != "nix"
        && baseNameOf path != "testdriver"
        && baseNameOf path != "testserver"
        ) ./.;
  configureFlags = [ "-fexample" ];
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    base bytestring case-insensitive containers data-default exceptions
    generic-lens ghcjs-dom http-api-data http-media jsaddle mtl network-uri reflex
    reflex-dom-core safe servant servant-auth string-conversions text
    transformers
  ];
  executableHaskellDepends = [
    aeson base reflex reflex-dom-core scientific servant text
  ];
  description = "servant API generator for reflex apps";
  license = stdenv.lib.licenses.bsd3;
}
