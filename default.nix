{ mkDerivation, exceptions, http-types, reflex, reflex-dom,
  reflex-dom-contrib, servant
}:

mkDerivation {
  pname = "servant-reflex";
  version = "0.1";
  src = builtins.filterSource (path: type: baseNameOf path != ".git") ./.;
  isExecutable = true;
  isLibrary = true;
  buildDepends = [
    exceptions
    http-types
    reflex
    reflex-dom
    reflex-dom-contrib
    servant
  ];
  license = null;
}
