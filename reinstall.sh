cabal install --force-reinstalls

cd packages/special
cabal install --force-reinstalls --enable-documentation
cd ../glpk
cabal install --force-reinstalls --enable-documentation
cd ../tests
cabal install --force-reinstalls --enable-documentation --enable-tests
cd ../..

