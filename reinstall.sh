cd packages/base
cabal install --force-reinstalls --enable-documentation
cd ../hmatrix
cabal install --force-reinstalls --enable-documentation
cd ../special
cabal install --force-reinstalls --enable-documentation
cd ../glpk
cabal install --force-reinstalls --enable-documentation
cd ../tests
cabal install --force-reinstalls --enable-documentation --enable-tests
cd ../..

