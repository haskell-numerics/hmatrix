# hmatrix installation

*WARNING*: some of these instructions are now very out of date. However, there
is a CI script which shows what does work; you are advised to look at
that and emulate its installation steps.

## Linux ##################################################

This package requires [GHC 7.8](http://www.haskell.org/ghc),
[cabal-install](http://www.haskell.org/haskellwiki/Cabal-Install)
(available in the [Haskell
Platform](http://hackage.haskell.org/platform)), and the development
packages for BLAS/[LAPACK](http://www.netlib.org/lapack) and
[GSL](http://www.gnu.org/software/gsl).

Ubuntu/Debian:

    $ sudo apt-get install libgsl0-dev liblapack-dev libatlas-base-dev
    $ cabal update
    $ cabal install hmatrix-tests

Other distributions may require additional libraries. They can be given in a **--configure-option**.

Adrian Victor Crisciu has developed an [installation method](http://comments.gmane.org/gmane.comp.lang.haskell.glasgow.user/24976) for systems
which don't provide shared lapack libraries.

## Mac OS/X ###############################################

GSL must be installed via Homebrew or MacPorts.

Via Homebrew:

    $ brew install gsl
    $ cabal install hmatrix

Via MacPorts:

    $ sudo port install gsl +universal
    $ cabal install hmatrix

(Contributed by Heinrich Apfelmus, Torsten Kemps-Benedix and Ted Fujimoto).

## Windows 10 #############################################

### Stack-based build (preferred)

1) Install the Haskell Tool Stack tool (`stack`). How to do that is explained
   [here](https://docs.haskellstack.org/en/stable/README/).

2) Get, and link to the import library of, the OpenBLAS library. There are
   various ways to do that. Perhaps the easiest is to use
   [MSYS2](https://www.msys2.org/) to get it, which is explained in the steps
   under subheading 2(a) below.

   #### 2(a) Get OpenBLAS library with stack's MSYS2 ######

   1) `stack` comes with a version of MSYS2. It is located in a subfolder of the
      folder returned by the `stack path --programs` command. At the time of
      writing (28 July 2020), that subfolder is `msys2-20180531`. Change
      directory to that folder. In PowerShell:

          > stack path --programs | cd
          > cd msys2-20180531

   2) Open a MSYS2 terminal window, with the `msys2_shell.cmd` command. In
      PowerShell:

          > .\msys2_shell.cmd

   3) In principle, MSYS2 itself can be updated with the `pacman -Syu` command.
      At the time of writing, that may be complicated by keyring issues (see
      [here](https://www.msys2.org/news/#2020-06-29-new-packagers) to overcome
      such issues). In MSYS2:

          $ pacman -Syu

   4) Use MSYS2 to install the OpenBLAS package (and its dependencies). In
      MSYS2:

          $ pacman -S mingw-w64-x86_64-openblas

      MSYS2 will put the import libraries in subfolder `\ming64\lib`. The
      `stack` enviroment will automatically add that folder to the list of
      library folders (see, back in PowerShell, the result of command
      `stack path --extra-library-dirs`).

   5) `hmatrix` depends on OpenBLAS. `hmatrix-gsl` depends on GSL (the GNU
      Scientific Library) and `hmatrix-glpk` depends on GLPK (the GNU Linear
      Programming Kit). You can use MSYS2 to install GSL and GLPK packages too.
      In MSYS2:

          $ pacman -S mingw-w64-x86_64-gsl
          $ pacman -S mingw-w64-x86_64-glpk

      Again, MSYS2 will put the import libraries in subfolder `\ming64\lib`.

   6) In `hmatrix.cabal`, ensure that the `extra-libraries` specifies `openblas`
      (only). (This is incorrect in `hmatrix-0.20.0.0`.) That is, the extract
      should read:

      ```
      if os(windows)
          if flag(openblas)
              extra-libraries: openblas
          else
              extra-libraries: blas lapack
      ```
   7) Similarly, in `hmatrix-gsl.cabal`, ensure that the `extra-libraries`
      specifies `gsl` (only). (This is incorrect in `hmatrix-gsl-0.19.0.1`.)
      That is, the extract should read:

      ```
      if os(windows)
          extra-libraries: gsl
      ```

   8) `stack ghci` can be invoked in the repository root folder (`hmatrix`) with
      an appropriate `resolver` and setting the flag `openblas` for package
      `hmatrix`. At the time of writing, `resolver nightly-2020-07-28` works
      with GHC 8.10.1. In PowerShell:

          > stack --resolver nightly-2020-07-28 ghci --flag hmatrix:openblas

      (At the time of writing, GHC 8.8.3 does not work with Windows 10 version
      2004. To use GHC 8.10.1, the upper bound of `base` in `examples.cabal`
      must be set to be at least 4.14.)

   9) In GHCi, test the `Numeric.LinearAlbebra` module with the following (the
      successful output has many lines and so is shortened below):

      ```
      > Numeric.LinearAlgebra.Tests.runTests 20

      ------ index
      +++ OK, passed 100 tests.
      +++ OK, passed 100 tests.
      ...
      +++ OK, passed 100 tests.
      ------ some unit tests
      Cases: 52  Tried: 52  Errors: 0  Failures: 0
      ```

   #### 2(b) Build OpenBLAS library from source ################

   *WARNING*: these alternative instructions may be out of date.

   1) Download and unzip somewhere OpenBLAS http://www.openblas.net/

   2) In MSYS2 console of Stack, i.e.:
      C:\Users\{User}\AppData\Local\Programs\stack\x86_64-windows\msys2-{version}\msys2_shell.bat

          $ cd /.../OpenBLAS
          $ pacman -Sy
          $ pacman -S make perl gcc-fortran
          $ make clean
          $ make
          $ make install

   3) Then in normal Windows console for building hmatrix base lib (fill in user
      name, versions and check if paths are different on your machine):

          > stack install --flag hmatrix:openblas --extra-include-dirs=C:\Users\{User}\AppData\Local\Programs\stack\x86_64-windows\msys2-20150512\opt\OpenBLAS\include --extra-lib-dirs=C:\Users\{User}\AppData\Local\Programs\stack\x86_64-windows\msys2-20150512\opt\OpenBLAS\bin --extra-lib-dirs=C:\Users\{User}\AppData\Local\Programs\stack\x86_64-windows\msys2-20150512\usr\lib\gcc\x86_64-pc-msys\6.3.0\

### Cabal-based build

(Not tested). It should be possible to install the new package hmatrix >= 0.16 using
the dlls contributed by Gilberto Camara available in [gsl-lapack-windows.zip][winpack].

1) > cabal update

2) Download and unzip [gsl-lapack-windows.zip][winpack] into a stable folder %GSL%

3.a) In a msys shell:

    $ cabal install hmatrix-0.13.1.0 --extra-lib-dir=${GSL} --extra-include-dirs=${GSL}

3.b) In a normal windows cmd:

     > cabal install --extra-lib-dir=%GSL% --extra-include-dirs=%GSL%

It may be necessary to put the dlls in the search path.

It is expected that a future version of the new hmatrix-gsl package can also be installed
using this method.

[winpack]: https://github.com/downloads/AlbertoRuiz/hmatrix/gsl-lapack-windows.zip

### Alternative Windows build

1)

	> cabal update

2) Download and unzip somewhere OpenBLAS http://www.openblas.net/

3) In a normal Windows cmd:

    > cabal install --flags=openblas --extra-lib-dirs=C:\...\OpenBLAS\lib --extra-include-dirs=C:\...\OpenBLAS\include

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
