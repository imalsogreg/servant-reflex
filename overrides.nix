{ reflex-platform, ... }:
let

  nixpkgs = (import <nixpkgs> {});
  dontCheck = nixpkgs.pkgs.haskell.lib.dontCheck;
  cabal2nixResult = reflex-platform.cabal2nixResult;

in
reflex-platform.ghcjs.override {
  overrides = self: super: { 
     servant       = dontCheck (self.callPackage (cabal2nixResult deps/servant/servant) {});
     http-api-data = dontCheck (self.callPackage (cabal2nixResult deps/http-api-data) {});
     natural-transformation = self.callPackage (
       { mkDerivation, base, containers, fetchgit, quickcheck-instances
       , stdenv, tasty, tasty-quickcheck
       }:
       mkDerivation {
         pname = "natural-transformation";
         version = "0.4";
         src = fetchgit {
           url = "https://github.com/ku-fpg/natural-transformation";
           sha256 = "0xbnnxbf5ydszbhf7h5ra3mrqy4mcyqc3zb2k8bwm1zyqrz6v0fs";
           rev = "b1200c09dcafd034e32846413913b74735c8ba56";
         };
         libraryHaskellDepends = [ base ];
         testHaskellDepends = [
           base containers quickcheck-instances tasty tasty-quickcheck
         ];
         homepage = "https://github.com/ku-fpg/natural-transformation";
         description = "A natural transformation package";
         license = stdenv.lib.licenses.bsd3;
       }
       ) {};
  };
}
