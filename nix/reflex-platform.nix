let
  rev = "df0bdcca5eb2a3236ec0496e4430d91876b29cf5";
in import (builtins.fetchTarball
  {
    url = "https://github.com/reflex-frp/reflex-platform/archive/${rev}.tar.gz";
    sha256 = "1ja3vkq9px8f9iyiazq44mamaahgiphi9l236pvzbl5jvhi5c4qr";
  }) {}
