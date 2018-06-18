{ compiler ? "ghcjs" }:

(import ./release.nix { inherit compiler; }).servant-reflex.env
