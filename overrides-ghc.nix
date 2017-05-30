{ reflex-platform, ... }:
let
  c2n = reflex-platform.cabal2nixResult;
  dc  = reflex-platform.lib.dontCheck;
in reflex-platform.ghc.override {
  overrides = self: super: {
    servant-snap        = dc (self.callPackage (c2n deps/servant-snap) {});
    heist               = dc (self.callPackage (c2n deps/servant-snap/deps/snap/deps/heist) {});
    xmlhtml             = dc (self.callPackage (c2n deps/servant-snap/deps/snap/deps/xmlhtml) {});
  };
}
