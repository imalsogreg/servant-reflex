#! /bin/sh

cabal sandbox init

cabal sandbox add-source deps/servant-snap

cabal sandbox add-source .

cd testserver && cabal sandbox init --sandbox=../.cabal-sandbox && cd ..
