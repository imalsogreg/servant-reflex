{ reflex-platform, ... }:
let
  dc = reflex-platform.lib.dontCheck;
  c2n = reflex-platform.cabal2nixResult;
in
reflex-platform.ghcjs.override {
  overrides = self: super: { 
  };
}
