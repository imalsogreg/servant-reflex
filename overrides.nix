{}:
let try-reflex = import deps/try-reflex {};
in try-reflex.ghcjs.override {
  overrides = self: super: {
    reflex-dom-contrib = self.callPackage deps/reflex-dom-contrib {};
    servant = self.callPackage (try-reflex.cabal2nixResult deps/servant/servant) {};
  };
}
