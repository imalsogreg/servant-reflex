{ reflex-platform, ... }:
let 
  pkgs = import <nixpkgs> {};
  reflex-platform = import deps/reflex-platform {};
in reflex-platform.ghc.override {
  overrides = self: super: {
    reflex-dom = pkgs.haskell.lib.dontCheck (self.callPackage deps/reflex-dom {});
  };
}
