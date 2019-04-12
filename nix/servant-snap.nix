# Servant-snap is stuck a bit behind atm. But we have a fork that mostly works
# except for streaming, which is good enough for this test server. See:
# https://github.com/haskell-servant/servant-snap/issues/20
{ nixpkgs ? import <nixpkgs> {}}: self: super:
self.callCabal2nix "servant-snap" (nixpkgs.fetchgit {
  url    = "https://github.com/antislava/servant-snap.git";
  rev    = "fc9658e8f52ebce9e4f304ea0c6705d697d4fa84";
  sha256 = "0zlipmx1fb73mhpnndwmdmigxxrsdnsnb1157pgsrxpx89l9pjig";
}) {}
