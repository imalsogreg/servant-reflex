{ mkDerivation, base, aeson, bytestring, case-insensitive, containers
, data-default, exceptions, ghcjs-dom, http-api-data, http-media
, jsaddle, mtl, network-uri, reflex, reflex-dom, reflex-dom-core, safe
, scientific, servant, servant-auth, servant-client-core, stdenv
, string-conversions, text, transformers
}:
mkDerivation {
  pname = "servant-reflex";
  version = "0.3.4";
  src = builtins.filterSource
        (path: type:
        baseNameOf path != "result"
        && baseNameOf path != "nix"
        ) ./.;
  configureFlags = [ "-fexample" ];
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    base bytestring case-insensitive containers data-default exceptions
    ghcjs-dom http-api-data http-media jsaddle mtl network-uri reflex
    reflex-dom-core safe servant servant-auth servant-client-core
    string-conversions text transformers
  ];
  executableHaskellDepends = [
    aeson base reflex reflex-dom scientific servant text
  ];
  description = "Servant reflex API generator";
  license = stdenv.lib.licenses.bsd3;
}
