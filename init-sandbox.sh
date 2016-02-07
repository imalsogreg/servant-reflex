#! /bin/sh

cabal sandbox init

cabal sandbox add-source deps/snaplet-postgresql-simple

#! /bin/sh

cabal sandbox init

echo "Depend on snap-1.0 from github. Move to Hackage version when snap-1.0 is released"
cabal sandbox add-source deps/servant-snap/deps/snap/deps/io-streams
cabal sandbox add-source deps/servant-snap/deps/snap/deps/io-streams-haproxy
cabal sandbox add-source deps/servant-snap/deps/snap/deps/snap-core
cabal sandbox add-source deps/servant-snap/deps/snap/deps/snap-server
cabal sandbox add-source deps/servant-snap/deps/snap/deps/xmlhtml
cabal sandbox add-source deps/servant-snap/deps/snap/deps/heist
cabal sandbox add-source deps/servant-snap/deps/snap-loader-static
cabal sandbox add-source deps/servant-snap/deps/snap-loader-static
cabal sandbox add-source deps/servant-snap/deps/snap
cabal sandbox add-source deps/servant-snap/deps/hspec-snap

echo "Depend on particular servant branch with (Raw m a) changes"
echo "TODO: Move to hackage version when servant-0.5 is released"
cabal sandbox add-source deps/servant-snap/deps/servant/servant
cabal sandbox add-source deps/servant-snap/deps/servant/servant-docs
cabal sandbox add-source deps/servant-snap/deps/servant/servant-client
cabal sandbox add-source deps/servant-snap/deps/servant/servant-blaze
cabal sandbox add-source deps/servant-snap/deps/servant/servant-js
cabal sandbox add-source deps/servant-snap/deps/servant/servant-lucid
cabal sandbox add-source deps/servant-snap/deps/servant/servant-mock

cabal sandbox add-source deps/servant-snap
cabal sandbox add-source deps/reflex-dom
cabal sandbox add-source deps/reflex-dom-contrib

cabal sandbox add-source .

cd test/front && cabal sandbox init --sandbox=../../.cabal-sandbox && cd ../..
cd test/back  && cabal sandbox init --sandbox=../../.cabal-sandbox && cd ../..
