cabal install --force-reinstalls

cd packages/special
cabal install --force-reinstalls
cd ../glpk
cabal install --force-reinstalls
cd ../tests
cabal install --force-reinstalls --enable-tests
cd ../..

