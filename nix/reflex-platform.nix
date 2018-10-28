let
  rev = "3d232e2d1d8e8d10fbafc928972c901c764ae324";
in import (builtins.fetchTarball
  {
    url = "https://github.com/reflex-frp/reflex-platform/archive/${rev}.tar.gz";
    sha256 = "15ysjn0aknbl06spn4qls7xapm0d3qw2awx6k8jl28zwkaid45yg";
  }) {}
