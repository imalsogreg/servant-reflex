{ reflex-platform, ... }:
let 
  pkgs = import <nixpkgs> {};
  reflex-platform = import deps/reflex-platform {};
in reflex-platform.ghc.override {
  overrides = self: super: {
    reflex-dom-contrib = pkgs.haskell.lib.dontCheck (self.callPackage deps/reflex-dom-contrib {});
    servant = pkgs.haskell.lib.dontCheck (self.callPackage (reflex-platform.cabal2nixResult deps/servant/servant) {});
  };
}
