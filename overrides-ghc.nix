{ reflex-platform, ... }:
let 
  pkgs = import <nixpkgs> {};
  reflex-platform = import deps/reflex-platform {};
  c2n = reflex-platform.cabal2nixResult;
in reflex-platform.ghc.override {
  overrides = self: super: {
    reflex-dom   = pkgs.haskell.lib.dontCheck (self.callPackage deps/reflex-dom {});
    servant-snap = pkgs.haskell.lib.dontCheck (self.callPackage (c2n deps/servant-snap) {});
    snap         = pkgs.haskell.lib.dontCheck (self.callPackage (c2n deps/servant-snap/deps/snap) {});
    snap-server  = pkgs.haskell.lib.dontCheck (self.callPackage (c2n deps/servant-snap/deps/snap/deps/snap-server) {});
    io-streams  = pkgs.haskell.lib.dontCheck (self.callPackage (c2n deps/servant-snap/deps/snap/deps/io-streams) {});
    io-streams-haproxy  = pkgs.haskell.lib.dontCheck (self.callPackage (c2n deps/servant-snap/deps/snap/deps/io-streams-haproxy) {});
    heist  = pkgs.haskell.lib.dontCheck (self.callPackage (c2n deps/servant-snap/deps/snap/deps/heist) {});
    xmlhtml  = pkgs.haskell.lib.dontCheck (self.callPackage (c2n deps/servant-snap/deps/snap/deps/xmlhtml) {});
    snap-core  = pkgs.haskell.lib.dontCheck (self.callPackage (c2n deps/servant-snap/deps/snap/deps/snap-core) {});
    servant  = pkgs.haskell.lib.dontCheck (self.callPackage (c2n deps/servant/servant) {});
    http-api-data = pkgs.haskell.lib.dontCheck (self.callPackage (c2n deps/http-api-data) {});
  };
}
