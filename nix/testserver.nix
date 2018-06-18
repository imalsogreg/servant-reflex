{ servant-reflex, reflexPlatform }:

let
  ghcPackages = reflexPlatform.ghc.override {
    overrides = self: super: {
      snap = reflexPlatform.lib.doJailbreak (self.callCabal2nix "snap" ../../snap {});
      hspec-snap = reflexPlatform.lib.doJailbreak (super.hspec-snap);
      servant-snap = reflexPlatform.lib.doJailbreak (super.servant-snap);
      skylighting = reflexPlatform.lib.dontCheck super.skylighting;
      testserver = (self.callPackage ../testserver/default.nix {}).overrideDerivation (old:
        { postInstall = ''
        echo "SOURCE IS: $src"
        echo "ls IS: $ls"
        echo "EXAMPLE IS: ${servant-reflex}"
        cp dist/build/back/back $out/back
        mkdir $out/static
        cp ${servant-reflex}/bin/example.jsexe/* $out/static/
        '';
         }
      );
      heist       = reflexPlatform.lib.doJailbreak super.heist;
    };
  };

in ghcPackages.testserver