{ reflex-platform, ... }:
let
  c2n = reflex-platform.cabal2nixResult;
  dc  = reflex-platform.lib.dontCheck;
in reflex-platform.ghc.override {
  overrides = self: super: {
    servant-snap        = dc (self.callPackage (c2n deps/servant-snap) {});
    heist               = dc (self.callPackage (c2n deps/servant-snap/deps/snap/deps/heist) {});
    xmlhtml             = dc (self.callPackage (c2n deps/servant-snap/deps/snap/deps/xmlhtml) {});
    # snap-core           = dc (self.callPackage (c2n deps/servant-snap/deps/snap/deps/snap-core) {});
    # snap                = dc (self.callPackage (c2n deps/servant-snap/deps/snap) {});
    # snap-server         = dc (self.callPackage (c2n deps/servant-snap/deps/snap/deps/snap-server) {});
    # io-streams          = dc (self.callPackage (c2n deps/servant-snap/deps/snap/deps/io-streams) {});
    # io-streams-haproxy  = dc (self.callPackage (c2n deps/servant-snap/deps/snap/deps/io-streams-haproxy) {});

    # http-api-data = pkgs.haskell.lib.dontCheck (self.callPackage (c2n deps/http-api-data) {});
  };
}
