{ reflex-platform, ... }:
let
  c2n = reflex-platform.cabal2nixResult;
  dc  = reflex-platform.lib.dontCheck;
  mypkgs = (import <nixpkgs> {}).haskellPackages;
in reflex-platform.ghc.override {
  overrides = self: super: {
    servant-snap        = dc (self.callPackage (c2n deps/servant-snap) {});
    heist               = dc (self.callPackage (c2n deps/servant-snap/deps/snap/deps/heist) {});
    xmlhtml             = dc (self.callPackage (c2n deps/servant-snap/deps/snap/deps/xmlhtml) {});
    servant = self.callHackage "servant" "0.12" {};
    servant-auth = self.callHackage "servant-auth" "0.3.0.1" {};
    servant-client-core = self.callHackage "servant-client-core" "0.12" {};
    # servant             = self.callCabal2nix "servant" (import ./nix/servant-src.nix + "/servant") {};
    # servant-client-core = self.callCabal2nix "servant-client-core" (import ./nix/servant-src.nix + "/servant-client-core") {};
  };
}
