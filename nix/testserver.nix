servant-reflex: self: super:

(self.callCabal2nix "testserver" ../testserver {}).overrideDerivation (drv: {
  postInstall = ''
    cp dist/build/back/back $out/back
    mkdir $out/static
    cp ${servant-reflex}/bin/example.jsexe/* $out/static/
  '';
})
