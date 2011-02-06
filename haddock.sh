#!/bin/sh

set -x

rm -Rf dist/doc

HADDOCK_OPTS='--html-location=http://hackage.haskell.org/packages/archive/$pkg/latest/doc/html --css=extra/haddock.css'

cabal haddock $HADDOCK_OPTS --hyperlink-source $@

cp -r extra/fonts dist/doc/html/xmlhtml/
cp extra/logo.gif dist/doc/html/xmlhtml/haskell_icon.gif
cp extra/hscolour.css dist/doc/html/xmlhtml/src/
