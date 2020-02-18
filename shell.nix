{ nativeCompiler ? "ghc8_6" }:
(import ./travis.nix { inherit nativeCompiler; }).ghcPkgs.servant-reflex.env
