let
  reflex-platform = import ./deps/reflex-platform { };
  pkgs = reflex-platform.nixpkgs;
  ghcjs = reflex-platform.ghcjs;
in
  rec {
    build = ghcjs.callCabal2nix "servant-reflex" ./. {};
    example = pkgs.haskell.lib.appendConfigureFlag build "-f Example";
    cabalBuild = ghcjs.shellFor {
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
