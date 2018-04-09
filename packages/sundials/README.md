I think this builds the executable. I haven't tried cleaning
everything yet to double check.

```
cabal install
cd src
/nix/store/qrwfai93qhdaj5d4xd34n9bq58slzfxw-gcc-wrapper-6.4.0/bin/gcc -c helpers.c -I/nix/store/dr0yrlmscybjs5ifzw1m063g6xgw5imq-ghc-8.2.2-with-packages/lib/ghc-8.2.2/include
cd ..
ghc src/Main.hs src/helpers.o -lsundials_arkode -o Main
```

To run you will (on MACos) also need

```
export DYLD_LIBRARY_PATH=~/.cabal/bin:/nix/store/dr0yrlmscybjs5ifzw1m063g6xgw5imq-ghc-8.2.2-with-packages/lib/ghc-8.2.2/rts
```