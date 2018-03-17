{ reflex-platform ? import ./nix/reflex-platform.nix
, compiler        ? "ghc"
} :
let

  ghc = (reflex-platform.${compiler}.override {
    overrides = self: super: {
      servant = self.callCabal2nix "servant" (import ./nix/servant-src.nix + "/servant") {};
      servant-client-core = self.callCabal2nix "servant-client-core" (import ./nix/servant-src.nix + "/servant-client-core") {};
    };
  });

  drv = ghc.callPackage ./servant-reflex.nix {};

in
  drv