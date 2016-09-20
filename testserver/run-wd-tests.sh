#!/usr/bin/env bash

dist/build/back/back -p 8000 &
phantomjs --webdriver=127.0.0.1:4444 &
dist/build/spec/spec
