{ try-reflex, reflex-platform }:
let 
  pkgs = import <nixpkgs> {};
in reflex-platform.ghcjs.override {
  overrides = self: super: {
    self.lens = pkgs.haskell.lib.dontCheck (self.lens);
    reflex-dom-contrib = pkgs.haskell.lib.dontCheck (self.callPackage deps/reflex-dom-contrib {});
    servant = self.callPackage (reflex-platform.cabal2nixResult deps/servant/servant) {};
  };
}
