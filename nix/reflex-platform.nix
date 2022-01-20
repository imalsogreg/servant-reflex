let
  rev = "123a6f487ca954fd983f6d4cd6b2a69d4c463d10" ;
in import (builtins.fetchTarball
  {
    url = "https://github.com/reflex-frp/reflex-platform/archive/${rev}.tar.gz";
    sha256 = "sha256:16q1rq0rwi6l28fv46q8m0hvb9rxrzf574j865vaz04xy8d5p1ya" ;
  }) {}
