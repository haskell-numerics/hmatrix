
# [hmatrix][hmatrix2] installation

This package requires the [Glasgow Haskell Compiler](http://www.haskell.org/ghc/index.html) ghc >= 6.10, and [cabal-install](http://www.haskell.org/haskellwiki/Cabal-Install), conveniently available in the [Haskell Platform](http://hackage.haskell.org/platform), and the development packages for [GSL](http://www.gnu.org/software/gsl) and BLAS/[LAPACK](http://www.netlib.org/lapack). (The graphical functions also require **gnuplot** and **imagemagick**.)

[hmatrix]: http://code.haskell.org/hmatrix
[hmatrix2]: http://perception.inf.um.es/hmatrix


## Linux ##################################################


Ubuntu/Debian:

    $ sudo apt-get install libgsl0-dev liblapack-dev
    $ cabal install hmatrix

Arch Linux: If the automatic installation from Hackage fails, install atlas-lapack and gsl, unpack the source, change the build-type to Simple in hmatrix.cabal (line 28) and add extra-libraries: gsl lapack (line 194). 

Other distributions may require additional libraries. They can be given in a **--configure-option**.

## Mac OS/X ###############################################


GSL must be installed via MacPorts:

    $ sudo port install gsl-devel +universal
    $ cabal install hmatrix

(Contributed by Heinrich Apfelmus and Torsten Kemps-Benedix).

## Windows ###############################################

We use this [GSL binary](http://www.miscdebris.net/blog/2009/04/20/mingw-345-binaries-of-gnu-scientific-library-112-for-use-with-mingw-and-visual-c/), and blas/lapack dlls built with g77 (contributed by Gilberto Camara). All required files are in [gsl-lapack-windows.zip][winpack].

(Due to [issue 21](https://github.com/albertoruiz/hmatrix/issues/21) we need hmatrix-0.13.1.0.)

1) Install the Haskell Platform (tested on 2011.2.0.1)

    > cabal update

2) Download and unzip the following file into a stable folder %GSL%

    http://perception.inf.um.es/hmatrix/gsl-lapack-windows.zip

3.a) In a msys shell the installation should be fully automatic:

    $ cabal install hmatrix-0.13.1.0 --extra-lib-dir=${GSL} --extra-include-dir=${GSL}

3.b) Alternatively, in a normal windows cmd:

     > cabal unpack hmatrix-0.13.1.0

   Edit hmatrix.cabal, in line 28 change build-type to "Simple", and then

     > cabal install --extra-lib-dir=%GSL% --extra-include-dir=%GSL%

   It may be necessary to put the dlls in the search path.


NOTE: The examples using graphics do not yet work in windows.

[install]: http://code.haskell.org/hmatrix/INSTALL
[install2]: http://patch-tag.com/r/aruiz/hmatrix/snapshot/current/content/pretty/INSTALL
[winpack2]: http://perception.inf.um.es/hmatrix/gsl-lapack-windows.zip
[winpack]: https://github.com/downloads/AlbertoRuiz/hmatrix/gsl-lapack-windows.zip

## Tests ###############################################

After installation we must verify that the library works as expected:

    $ cabal install hmatrix-tests --enable-tests
    $ ghci
    > Numeric.LinearAlgebra.Tests.runTests 20
    OK, passed 100 tests. 
    OK, passed 100 tests.
    ... etc...

If you get any failure please run lapack's own tests to confirm that your version is not broken. For instance, in ubuntu 9.04, **libatlas-sse2** does not work (see this [bug report](https://bugs.launchpad.net/ubuntu/+source/atlas/+bug/368478)). If your lapack library is ok but hmatrix's tests fail please send a bug report!


## Optimized BLAS/LAPACK ##########################################

I have successfully tested ATLAS and MKL on Linux.

### [ATLAS](http://math-atlas.sourceforge.net/)  ####################

In Ubuntu >= 9.04 we need:

    $ sudo apt-get install libatlas-base-dev

In older Ubuntu/Debian versions we needed:

    $ sudo apt-get install refblas3-dev lapack3-dev atlas3-base-dev

We may use a version (sse2, 3dnow, etc.) optimized for the machine.

### Intel's MKL  ###############################################

There is a free noncommercial download available from Intel's website. To use it I have added the following lines in my .bashrc configuration file:

    export LD_LIBRARY_PATH=/path/to/mkl/lib/arch
    export LIBRARY_PATH=/path/to/mkl/lib/arch

where arch = 32 or em64t.

The library must be installed with the -fmkl flag:

    $ cabal install hmatrix -fmkl


