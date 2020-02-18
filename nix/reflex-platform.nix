let
  rev = "733b07ca683f4929ef3fedfc10ee01ab792c44c6";
in import (builtins.fetchTarball
  {
    url = "https://github.com/reflex-frp/reflex-platform/archive/${rev}.tar.gz";
    sha256 = "0w7jgwh9b8wdljjbb1rwc7aw9kfywfg26w0pahzd5lkshzrfix72";
  }) {}
