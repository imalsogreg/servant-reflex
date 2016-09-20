#!/bin/sh

deps/reflex-platform/work-on ./overrides.nix ./. --run "cabal configure --ghcjs && cabal build && testdriver/toSite.sh"
