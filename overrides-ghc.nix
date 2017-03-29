{ reflex-platform, ... }:
let
  dc  = reflex-platform.lib.dontCheck;
  c2n = reflex-platform.cabal2nixResult;
in reflex-platform.ghc.override {
  overrides = self: super: {
    servant-snap        = dc (self.callPackage (c2n deps/servant-snap) {});
    snap                = dc (self.callPackage (c2n deps/servant-snap/deps/snap) {});
    snap-server         = dc (self.callPackage (c2n deps/servant-snap/deps/snap/deps/snap-server) {});
    io-streams          = dc (self.callPackage (c2n deps/servant-snap/deps/snap/deps/io-streams) {});
    io-streams-haproxy  = dc (self.callPackage (c2n deps/servant-snap/deps/snap/deps/io-streams-haproxy) {});
    heist               = dc (self.callPackage (c2n deps/servant-snap/deps/snap/deps/heist) {});
    xmlhtml             = dc (self.callPackage (c2n deps/servant-snap/deps/snap/deps/xmlhtml) {});
    snap-core           = dc (self.callPackage (c2n deps/servant-snap/deps/snap/deps/snap-core) {});
    servant             = dc (self.callPackage (c2n deps/servant/servant) {});
    Glob                = dc super.Glob;
    http-api-data       = dc (self.callPackage (c2n deps/http-api-data) {});
  };
}
