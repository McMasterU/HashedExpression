#!/bin/bash

set -ev
cabal install --enable-tests --enable-benchmarks --force-reinstalls --ghc-options=-O0 --reorder-goals --max-backjumps=-1 $CABALARGS $PACKAGES

ORIGDIR=$(pwd)
for dir in $PACKAGES
do
  cd $dir
  cabal check || [ "$CABALVER" == "1.16" ]
  cabal sdist
  PKGVER=$(cabal info . | awk '{print $2;exit}')
  SRC_TGZ=$PKGVER.tar.gz
  cd dist
  tar zxfv "$SRC_TGZ"
  cd "$PKGVER"
  cabal configure --enable-tests --ghc-options -O0
  cabal build
  if [ "$CABALVER" = "1.16" ] || [ "$CABALVER" = "1.18" ]; then
    cabal test
  else
    cabal test --show-details=streaming --log=/dev/stdout
  fi
  cd $ORIGDIR
done
;;
set +ev