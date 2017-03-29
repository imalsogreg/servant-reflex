{ reflex-platform, ... }:
let

  nixpkgs = (import <nixpkgs> {});
  dontCheck = nixpkgs.pkgs.haskell.lib.dontCheck;
  cabal2nixResult = reflex-platform.cabal2nixResult;

in
reflex-platform.ghcjs.override {
  overrides = self: super: { 
  };
}
