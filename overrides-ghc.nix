{ reflex-platform, ... }:
let
  pkgs = import <nixpkgs> {};
  c2n = reflex-platform.cabal2nixResult;
in reflex-platform.ghc.override {
  overrides = self: super: {
    servant-snap = pkgs.haskell.lib.dontCheck (self.callPackage (c2n deps/servant-snap) {});
    snap         = pkgs.haskell.lib.dontCheck (self.callPackage (c2n deps/servant-snap/deps/snap) {});
    snap-server  = pkgs.haskell.lib.dontCheck (self.callPackage (c2n deps/servant-snap/deps/snap/deps/snap-server) {});
    io-streams  = pkgs.haskell.lib.dontCheck (self.callPackage (c2n deps/servant-snap/deps/snap/deps/io-streams) {});
    io-streams-haproxy  = pkgs.haskell.lib.dontCheck (self.callPackage (c2n deps/servant-snap/deps/snap/deps/io-streams-haproxy) {});
    heist  = pkgs.haskell.lib.dontCheck (self.callPackage (c2n deps/servant-snap/deps/snap/deps/heist) {});
    xmlhtml  = pkgs.haskell.lib.dontCheck (self.callPackage (c2n deps/servant-snap/deps/snap/deps/xmlhtml) {});
    snap-core  = pkgs.haskell.lib.dontCheck (self.callPackage (c2n deps/servant-snap/deps/snap/deps/snap-core) {});
    servant  = pkgs.haskell.lib.dontCheck (self.callPackage (c2n deps/servant/servant) {});
    Glob = pkgs.haskell.lib.dontCheck super.Glob;
    http-api-data = pkgs.haskell.lib.dontCheck (self.callPackage (c2n deps/http-api-data) {});
  };
}
