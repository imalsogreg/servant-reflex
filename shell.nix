{ nixpkgs ? import <nixpkgs> {}
, compiler ? "ghc"
} :
let
  inherit (nixpkgs) pkgs;
  reflex-platform = import ./nix/reflex-platform.nix;
  drv = import ./. { inherit reflex-platform compiler; };
in
  if pkgs.lib.inNixShell then drv.env else drv
