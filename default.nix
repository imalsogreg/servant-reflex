{ mkDerivation, base, bytestring, case-insensitive, containers
, data-default, exceptions, ghcjs-dom, http-api-data, http-media
, jsaddle, jsaddle-dom, mtl, network-uri, reflex, reflex-dom
, reflex-dom-core, safe, servant, servant-auth, stdenv
, string-conversions, text, transformers
}:
mkDerivation {
  pname = "servant-reflex";
  version = "0.3.4";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    base bytestring case-insensitive containers data-default exceptions
    ghcjs-dom http-api-data http-media jsaddle jsaddle-dom mtl
    network-uri reflex reflex-dom reflex-dom-core safe servant
    servant-auth string-conversions text transformers
  ];
  description = "Servant reflex API generator";
  license = stdenv.lib.licenses.bsd3;
  configureFlags = [ "-fexample" ];

}
