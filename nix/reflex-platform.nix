let
  rev = "2df9a830193f551ff222ac6b0c739c622578ddc0";
in import (builtins.fetchTarball
  {
    url = "https://github.com/reflex-frp/reflex-platform/archive/${rev}.tar.gz";
    sha256 = "17084xqhixk79av0gnjgwkhdigf9zwkd4axb6qp89dyyffy5s3hi";
  }) {}
