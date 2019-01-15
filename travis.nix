{
  nativeCompiler ? "ghc",
  # ^ Choose a GHC for compiling backend components (web server, selenium)
  # (ghc, ghc8_0, ghc8_2, ghc8_4 or ghcHEAD)

  jsCompiler     ? "ghcjs"
  # ^ Choose a GHCJS for compiling the frontend
  # (ghcjs, ghcjs8_0, ghcjs8_2 or ghcjs8_4)

}:

let

  reflexPlatform = import ./nix/reflex-platform.nix;

  lib = reflexPlatform.nixpkgs.haskell.lib;
  do  = funs: pkg: builtins.foldl' (a: b: b a) pkg funs;

  ghcjsPkgs = with lib; reflexPlatform.${jsCompiler}.override {
    overrides = self: super: {
      http-media     = dontCheck super.http-media;
      servant        = do [ dontCheck doJailbreak ] super.servant;
      servant-reflex = lib.appendConfigureFlag
                         (self.callPackage ./default.nix {}) "-fExample";
      uri-bytestring = doJailbreak super.uri-bytestring;
      http-api-data  = dontHaddock super.http-api-data;
    };
  };

  ghcPkgs = with lib; reflexPlatform.${nativeCompiler}.override {
    overrides = self: super: {
      heist           = do [ doJailbreak dontCheck ] super.heist;
      http-media      = dontCheck super.http-media;
      snap            = doJailbreak (self.callHackage "snap" "1.1.1.0" {});
      hspec-webdriver = lib.appendPatch super.hspec-webdriver
                          ./nix/hspec-webdriver.patch;
      hspec-snap      = doJailbreak super.hspec-snap;
      snap-server     = dontCheck super.snap-server;
      servant-snap    = do [ dontCheck doJailbreak ] (self.callHackage "servant-snap" "0.8.1" {});
      servant-reflex = lib.appendConfigureFlag
                         (self.callPackage ./default.nix {}) "-fExample";
      skylighting     = dontCheck super.skylighting;
      testdriver      = self.callCabal2nix "testdriver" ./testdriver {};
      testserver      = import nix/testserver.nix ghcjsPkgs.servant-reflex self super;
    };
  };

  testresults = import ./nix/testresults.nix
    { inherit reflexPlatform;
      inherit (ghcPkgs) testserver testdriver;
      inherit (reflexPlatform.nixpkgs) curl;
      phantomjs = reflexPlatform.nixpkgs.phantomjs2;
    };

in
  rec {

    inherit reflexPlatform ghcPkgs ghcjsPkgs;

    build       = ghcjsPkgs.servant-reflex;
    testserver  = ghcPkgs.testserver;
    testdriver  = ghcPkgs.testdriver;
    inherit testresults;

    cabalBuild = reflexPlatform.${jsCompiler}.shellFor {
      name = "servant-reflex-cabal-builder";

      packages = p: [
        build
      ];

      shellHook = ''
        cabal configure --ghcjs -f Example
        cabal build
        exec/toSite.sh
        exit $?
      '';
    };
  }
