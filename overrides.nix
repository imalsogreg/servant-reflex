{ reflex-platform, ... }:
let

  dc = reflex-platform.lib.dontCheck;
  c2n = reflex-platform.cabal2nixResult;

in
reflex-platform.ghcjs.override {
  overrides = self: super: { 
     servant       = dc (self.callPackage (c2n deps/servant/servant) {});
     http-api-data = dc (self.callPackage (c2n deps/http-api-data) {});
  };
}
