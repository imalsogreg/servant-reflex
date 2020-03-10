{ pkgs ? import <nixpkgs> {}, reflexPlatform, compiler ? "ghcjs" }:

let

  # generic-lens-src = pkgs.fetchFromGitHub {
  #   owner = "kcsongor";
  #   repo = "generic-lens";
  #   rev = "6787a4e85d09ad9fcfe5baae53fea2cccbae5976";
  #   sha256 = "1bvjwxm4ik2zcx9s57ibi376y255vm5mawx12s1sjcn09mv1vvdh";
  # };

  ghcjsPackages = reflexPlatform.${compiler}.override {
    overrides = self: super: {
      servant = super.servant;
      # generic-lens = pkgs.haskell.lib.dontCheck (self.callCabal2nix "generic-lens" generic-lens-src {
      #   inspection-testing = null;
      # });
      wai = super.wai;
      servant-reflex = pkgs.haskell.lib.appendConfigureFlag 
                       (self.callPackage ../. {}) "-fExample";
    };
  };

in ghcjsPackages.servant-reflex
