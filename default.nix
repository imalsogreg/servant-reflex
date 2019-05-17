{ mkDerivation, aeson, base, bytestring, case-insensitive
, containers, data-default, exceptions, ghcjs-dom, http-api-data
, http-media, jsaddle, mtl, network-uri, reflex, reflex-dom
, reflex-dom-core, safe, scientific, servant, servant-auth, stdenv
, string-conversions, text, transformers, megaparsec, wreq
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
    reflex-dom-core safe servant servant-auth string-conversions text
    transformers megaparsec wreq
  ];
  executableHaskellDepends = [
    aeson base reflex reflex-dom scientific servant text
  ];
  description = "Servant reflex API generator";
  license = stdenv.lib.licenses.bsd3;
}
