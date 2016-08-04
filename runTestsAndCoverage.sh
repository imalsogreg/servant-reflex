#!/bin/sh

set -e

# All directory variables relative to project root
# DIR=dist-newstyle/hpc
DIR=./dist/hpc

#SUITE=./dist-newstyle/build/servant-reflex-0.2/build/testsuite/testsuite
SUITE=./dist/build/testsuite/testsuite

if [ -z "$DEBUG" ]; then
    export DEBUG=snap-testsuite
fi

rm -f testsuite.tix
rm -rf "$DIR"
mkdir -p "$DIR"

if [ ! -f $SUITE ]; then
    cat <<EOF
Testsuite executable not found, please run:
    cabal install --enable-tests --only-dependencies
    cabal configure --enable-tests
    cabal build
EOF
    exit;
fi

$SUITE $*

EXCLUDES='Main
Paths_servant-reflex
'

EXCL=""

for m in $EXCLUDES; do
    EXCL="$EXCL --exclude=$m"
done

# TODO - actually send results to /dev/null when hpc kinks are fully removed
echo "Call hpc:"
echo hpc markup $EXCL --destdir=$DIR testsuite # >/dev/null 2>&1
hpc markup $EXCL --destdir=$DIR testsuite # >/dev/null 2>&1

echo "ls $DIR:"
ls $DIR

echo "ls:"
ls

cp -r $DIR out/

cat <<EOF


Test coverage report written to $HTMLDIR.
EOF
