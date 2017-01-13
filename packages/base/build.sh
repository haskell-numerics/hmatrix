#!/bin/sh

EXTRA_STACK_ARGS="--nix --install-ghc"

# Use openblas on Linux
#
if [ "Linux" = "$(uname -s)" ]; then
   EXTRA_BUILD_ARGS="--flag hmatrix:openblas"
fi

echo Issuing stack $EXTRA_STACK_ARGS build $EXTRA_BUILD_ARGS "$@"
eval stack $EXTRA_STACK_ARGS build $EXTRA_BUILD_ARGS "$@"
