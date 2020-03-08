let
  rev = "a15d3a2411e7ca7d4ee4853b57c72fe83faee272";
in import (builtins.fetchTarball
  {
    url = "https://github.com/reflex-frp/reflex-platform/archive/${rev}.tar.gz";
    sha256 = "1dsvw0lah7761vndip1hqal4fjpjv84ravinnfhy83jgfav5ivna";
  }) {}
