{ mkDerivation, aeson, base, bytestring, case-insensitive
, containers, data-default, exceptions, ghcjs-dom, http-api-data
, http-media, jsaddle, mtl, network-uri, reflex, reflex-dom-core
, safe, servant, servant-auth, servant-checked-exceptions, stdenv
, string-conversions, text, transformers
}:
mkDerivation {
  pname = "servant-reflex";
  version = "0.3.4";
  configureFlags = [ "-fexample" ];
  src = builtins.filterSource
        (path: type:
        baseNameOf path != "result"
        && baseNameOf path != "nix") ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson base bytestring case-insensitive containers data-default
    exceptions ghcjs-dom http-api-data http-media jsaddle mtl
    network-uri reflex reflex-dom-core safe servant servant-auth
    servant-checked-exceptions string-conversions text transformers
  ];
  description = "servant API generator for reflex apps";
  license = stdenv.lib.licenses.bsd3;
}
