# Servant-snap is marked broken in nixpkgs
{ nixpkgs ? import <nixpkgs> {}}: self: super:
self.callCabal2nix "servant-snap" (nixpkgs.fetchgit {
  url    = "https://github.com/haskell-servant/servant-snap.git";
  rev    = "af5172a6de5bb2a07eb1bf4c85952075ec6ecdf3";
  sha256 = "sha256-fzzQGwG8Ko72OO/kwLMG52eNZSt3F2dxhNgM9gaO4yQ=";
}) {}
