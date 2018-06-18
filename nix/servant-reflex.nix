{ pkgs ? import <nixpkgs> {}, reflexPlatform, compiler ? "ghcjs" }:

let

  ghcjsPackages = reflexPlatform.${compiler}.override {
    overrides = self: super: {
      servant = super.servant;
      wai = super.wai;
      servant-reflex = pkgs.haskell.lib.appendConfigureFlag 
                       (self.callPackage ../. {}) "-fExample";
    };
  };

in ghcjsPackages.servant-reflex