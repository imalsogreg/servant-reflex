{ reflex-platform, ... }:
let

  nixpkgs = (import <nixpkgs> {});
  dontCheck = nixpkgs.pkgs.haskell.lib.dontCheck;
  cabal2nixResult = reflex-platform.cabal2nixResult;

in
reflex-platform.ghcjs.override {
  overrides = self: super: { 
     reflex-dom    = dontCheck (self.callPackage deps/reflex-dom {});
     servant       = dontCheck (self.callPackage (cabal2nixResult deps/servant/servant) {});
     http-api-data = dontCheck (self.callPackage (cabal2nixResult deps/http-api-data) {});
  };
}
