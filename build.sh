#!/usr/bin/env bash

reflex-platform/work-on ./overrides.nix ./. --run "cabal configure --ghcjs && cabal build && exec/toSite.sh"
