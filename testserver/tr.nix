{ reflex-platform, ... }:
let

  dontCheck = (import <nixpkgs> {}).pkgs.haskell.lib.dontCheck;
  cabal2nixResult = reflex-platform.cabal2nixResult;
  nixpkgs = (import <nixpkgs> {});
in
reflex-platform.ghc.override {
  overrides = self: super: { 
     servant-snap        = dontCheck (self.callPackage (cabal2nixResult ../deps/servant-snap) {});
     snap                = dontCheck (self.callPackage (cabal2nixResult ../deps/servant-snap/deps/snap) {});
     io-streams          = dontCheck (self.callPackage (cabal2nixResult ../deps/servant-snap/deps/snap/deps/io-streams) {});
     io-streams-haproxy  = dontCheck (self.callPackage (cabal2nixResult ../deps/servant-snap/deps/snap/deps/io-streams-haproxy) {});
     heist               = dontCheck (self.callPackage (cabal2nixResult ../deps/servant-snap/deps/snap/deps/heist) {});
     xmlhtml             = dontCheck (self.callPackage (cabal2nixResult ../deps/servant-snap/deps/snap/deps/xmlhtml) {});
     snap-core           = dontCheck (self.callPackage (cabal2nixResult ../deps/servant-snap/deps/snap/deps/snap-core) {});
     snap-server         = dontCheck (self.callPackage (cabal2nixResult ../deps/servant-snap/deps/snap/deps/snap-server) {});
     snap-loader-static  = dontCheck (self.callPackage (cabal2nixResult ../deps/servant-snap/deps/snap-loader-static) {});
     snap-loader-dynamic = dontCheck (self.callPackage (cabal2nixResult ../deps/servant-snap/deps/snap-loader-dynamic) {});
     hspec-snap          = dontCheck (self.callPackage (cabal2nixResult ../deps/servant-snap/deps/hspec-snap) {});

     # servant-foreign     = dontCheck (self.callPackage (cabal2nixResult ../deps/servant-snap/deps/servant/servant-foreign) {});
     # servant-js          = dontCheck (self.callPackage (cabal2nixResult ../deps/servant-snap/deps/servant/servant-js) {});
     # servant-docs        = dontCheck (self.callPackage (cabal2nixResult ../deps/servant-snap/deps/servant/servant-docs) {});
     # servant-client      = dontCheck (self.callPackage (cabal2nixResult ../deps/servant-snap/deps/servant/servant-client) {});
     # servant-blaze       = dontCheck (self.callPackage (cabal2nixResult ../deps/servant-snap/deps/servant/servant-blaze) {});
     # servant-lucid       = dontCheck (self.callPackage (cabal2nixResult ../deps/servant-snap/deps/servant/servant-lucid) {});
     # servant-server      = dontCheck (self.callPackage (cabal2nixResult ../deps/servant-snap/deps/servant/servant-server) {});
     # servant             = dontCheck (self.callPackage (cabal2nixResult ../deps/servant-snap/deps/servant/servant) {});

  };
}
