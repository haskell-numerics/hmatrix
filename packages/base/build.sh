#!/bin/sh

set -e

if [ "$IN_NIX_SHELL" ]; then

   # Use openblas on Linux
   #
   if [ "Linux" = "$(uname -s)" ]; then
      EXTRA_BUILD_ARGS="--flag hmatrix:openblas"
   fi

   BUILD_CMD="stack --install-ghc build $EXTRA_BUILD_ARGS $*"
else
   BUILD_CMD="./clean.sh && nix-build"
fi

echo Building with \"$BUILD_CMD\"
eval $BUILD_CMD
