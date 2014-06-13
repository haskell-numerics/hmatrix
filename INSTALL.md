# hmatrix installation

This package requires [GHC 7.8](http://www.haskell.org/ghc), [cabal-install](http://www.haskell.org/haskellwiki/Cabal-Install) (available in the [Haskell Platform](http://hackage.haskell.org/platform)), and the development packages for BLAS/[LAPACK](http://www.netlib.org/lapack) and [GSL](http://www.gnu.org/software/gsl).

## Linux ##################################################

Ubuntu/Debian:

    $ sudo apt-get install libgsl0-dev liblapack-dev libatlas-base-dev
    $ cabal update
    $ cabal install hmatrix-tests

Other distributions may require additional libraries. They can be given in a **--configure-option**.

## Mac OS/X ###############################################

GSL must be installed via Homebrew or MacPorts.

Via Homebrew:

    $ brew install gsl
    $ cabal install hmatrix

Via MacPorts:

    $ sudo port install gsl +universal
    $ cabal install hmatrix

(Contributed by Heinrich Apfelmus, Torsten Kemps-Benedix and Ted Fujimoto).

## Windows ###############################################

(Not tested). It should be possible to install the new package hmatrix >= 0.16 using
the dlls contributed by Gilberto Camara available in [gsl-lapack-windows.zip][winpack].

1) > cabal update

2) Download and unzip [gsl-lapack-windows.zip][winpack] into a stable folder %GSL%

3.a) In a msys shell:

    $ cabal install hmatrix-0.13.1.0 --extra-lib-dir=${GSL} --extra-include-dir=${GSL}

3.b) In a normal windows cmd:

     > cabal install --extra-lib-dir=%GSL% --extra-include-dir=%GSL%

It may be necessary to put the dlls in the search path.

It is expected that a future version of the new hmatrix-gsl package can also be installed
using this method.

[winpack]: https://github.com/downloads/AlbertoRuiz/hmatrix/gsl-lapack-windows.zip

## Tests ###############################################

After installation we can verify that the library works as expected:

    $ cabal install hmatrix-tests
    $ ghci
    > Numeric.LinearAlgebra.Tests.runTests 20
    +++ OK, passed 100 tests.
    +++ OK, passed 100 tests.
    ... etc...
    +++ OK, passed 100 tests.
    ------ some unit tests
    Cases: 71  Tried: 71  Errors: 0  Failures: 0

