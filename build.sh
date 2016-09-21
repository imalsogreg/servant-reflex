#!/usr/bin/env bash

deps/reflex-platform/work-on ./overrides.nix ./. --run "cabal configure --ghcjs && cabal build && exec/toSite.sh"
