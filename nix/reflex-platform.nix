let
  rev = "1aba6f367982bd6dd78ec2fda75ab246a62d32c5" ;
in import (builtins.fetchTarball
  {
    url = "https://github.com/reflex-frp/reflex-platform/archive/${rev}.tar.gz";
    sha256 = "137vg6i8yy4lp6rz4wbvvhfcc9d9qf7j1b3fnng3r7sj63k7j664" ;
  }) {}
