{ reflex-platform, ... }:
let 
  pkgs = import <nixpkgs> {};
  reflex-platform = import deps/reflex-platform {};
in reflex-platform.ghc.override {
  overrides = self: super: {
    reflex-dom-contrib = pkgs.haskell.lib.dontCheck (self.callPackage deps/reflex-dom-contrib {});
    servant = pkgs.haskell.lib.dontCheck (self.callPackage (reflex-platform.cabal2nixResult deps/servant/servant) {});
    servant-server = pkgs.haskell.lib.dontCheck (self.callPackage (reflex-platform.cabal2nixResult deps/servant/servant-server) {});
    servant-docs = pkgs.haskell.lib.dontCheck (self.callPackage (reflex-platform.cabal2nixResult deps/servant/servant-docs) {});
    servant-blaze = pkgs.haskell.lib.dontCheck (self.callPackage (reflex-platform.cabal2nixResult deps/servant/servant-blaze) {});

    servant-snap = pkgs.haskell.lib.dontCheck (self.callPackage (reflex-platform.cabal2nixResult deps/servant-snap) {});

    hspec-snap = pkgs.haskell.lib.dontCheck (self.callPackage (reflex-platform.cabal2nixResult deps/servant-snap/deps/hspec-snap) {});
    snap-loader-static = pkgs.haskell.lib.dontCheck (self.callPackage (reflex-platform.cabal2nixResult deps/servant-snap/deps/snap-loader-static) {});
    snap-loader-dynamic = pkgs.haskell.lib.dontCheck (self.callPackage (reflex-platform.cabal2nixResult deps/servant-snap/deps/snap-loader-dynamic) {});
    snap = pkgs.haskell.lib.dontCheck (self.callPackage (reflex-platform.cabal2nixResult deps/servant-snap/deps/snap) {});
    snap-server = pkgs.haskell.lib.dontCheck (self.callPackage (reflex-platform.cabal2nixResult deps/servant-snap/deps/snap/deps/snap-server) {});
    snap-core = pkgs.haskell.lib.dontCheck (self.callPackage (reflex-platform.cabal2nixResult deps/servant-snap/deps/snap/deps/snap-core) {});
    io-streams = pkgs.haskell.lib.dontCheck (self.callPackage (reflex-platform.cabal2nixResult deps/servant-snap/deps/snap/deps/io-streams) {});
    io-streams-haproxy = pkgs.haskell.lib.dontCheck (self.callPackage (reflex-platform.cabal2nixResult deps/servant-snap/deps/snap/deps/io-streams-haproxy) {});
    xmlhtml = pkgs.haskell.lib.dontCheck (self.callPackage (reflex-platform.cabal2nixResult deps/servant-snap/deps/snap/deps/xmlhtml) {});
    heist = pkgs.haskell.lib.dontCheck (self.callPackage (reflex-platform.cabal2nixResult deps/servant-snap/deps/snap/deps/heist) {});
    clock = pkgs.haskell.lib.dontCheck (self.callPackage (reflex-platform.cabal2nixResult deps/servant-snap/deps/clock) {});
  };
}
