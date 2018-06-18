{ mkDerivation, base, hspec, hspec-webdriver, stdenv, text
, webdriver
}:
mkDerivation {
  pname = "testdriver";
  version = "0.2";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    base hspec hspec-webdriver text webdriver
  ];
  description = "Selenium test driver for Servant reflex";
  license = stdenv.lib.licenses.bsd3;
  hydraPlatforms = stdenv.lib.platforms.none;
}
