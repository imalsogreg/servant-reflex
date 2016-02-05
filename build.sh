#!/bin/sh

deps/try-reflex/work-on ./overrides.nix ./. --run "cabal configure --ghcjs ; cabal build"
