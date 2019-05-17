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
      http-media      = dontCheck super.http-media;
      servant         = dontCheck super.servant;
      lens-aeson      = dontCheck super.lens-aeson;
      servant-reflex = lib.appendConfigureFlag
                         (self.callPackage ./default.nix {}) "-fExample";
    };
  };

  ghcPkgs = with lib; reflexPlatform.${nativeCompiler}.override {
    overrides = self: super: {
      servant-snap    = dontCheck ((import ./nix/servant-snap.nix {}) self super);
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
