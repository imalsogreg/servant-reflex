let  
  initialNixpkgs = import <nixpkgs> {};

  sources = rec {
    reflex-platform-info-pinned = initialNixpkgs.pkgs.lib.importJSON ./reflex-platform.json;
    reflex-platform = initialNixpkgs.pkgs.fetchFromGitHub {
      owner = "reflex-frp";
      repo = "reflex-platform";
      inherit (reflex-platform-info-pinned) rev sha256;
    };
  };

  reflex-platform = import sources.reflex-platform {};
in
  reflex-platform
