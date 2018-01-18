I thank Don Stewart, Henning Thielemann, Bulat Ziganshin, Heinrich Apfelmus,
and all the people in the Haskell mailing lists for their help.

I am particularly grateful to Vivian McPhail for his excellent
contributions: improved configure.hs, Binary instances for
Vector and Matrix, support for Float and Complex Float elements,
module reorganization, monadic mapVectorM, and many other improvements.

- Nico Mahlo discovered a bug in the eigendecomposition wrapper.

- Frederik Eaton discovered a bug in the design of the wrappers.

- Eric Kidd has created a wiki page explaining the installation on MacOS X:
  http://www.haskell.org/haskellwiki/GSLHaskell_on_MacOS_X

- Fawzi Mohamed discovered a portability bug in the lapack wrappers.

- Pedro E. López de Teruel fixed the interface to lapack.

- Antti Siira discovered a bug in the plotting functions.

- Paulo Tanimoto helped to fix the configuration of the required libraries.
  He also discovered the segfault of minimize.hs in ghci.

- Xiao-Yong Jin reported a bug on x86_64 caused by the assumptions in f2c.h,
  which are wrong for this architecture.

- Jason Schroeder reported an error in the documentation.

- Bulat Ziganshin gave invaluable help for the ST monad interface to
  in-place modifications.

- Don Stewart fixed the implementation of the internal data structures
  to achieve excellent, C-like performance in Haskell functions which
  explicitly work with the elements of vectors and matrices.

- Dylan Alex Simon improved the numeric instances to allow optimized
  implementations of signum and abs on Vectors.

- Pedro E. López de Teruel discovered the need of asm("finit") to
  avoid the wrong NaNs produced by foreign functions.

- Reiner Pope added support for luSolve, based on (d|z)getrs.
  Made Matrix a product type and added changes to improve the code generated
  by hmatrix-syntax.

- Simon Beaumont reported the need of QuickCheck<2 and the invalid
  asm("finit") on ppc. He also contributed the configuration options
  for the accelerate framework on OS X.

- Daniel Schüssler added compatibility with QuickCheck 2 as well
  as QuickCheck 1 using the C preprocessor. He also added some
  implementations for the new "shrink" method of class Arbitrary.

- Tracy Wadleigh improved the definitions of (|>) and (><), which now
  apply an appropriate 'take' to the given lists so that they may be
  safely used on lists that are too long (or infinite).

- Chris Waterson improved the configure.hs program for OS/X.

- Erik de Castro Lopo added buildVector and buildMatrix, which take a
  size parameter(s) and a function that maps vector/matrix indices
  to the values at that position.

- Jean-Francois Tremblay discovered an error in the tutorial.

- Gilberto Camara contributed improved blas and lapack dlls for Windows.

- Heinrich Apfelmus fixed hmatrix.cabal for OS/X. He also tested the package
  on PPC discovering a problem in zgesdd.

- Felipe Lessa tested the performance of GSL special function bindings
  and contributed the cabal flag "safe-cheap".

- Ozgur Akgun suggested better symbols for the Bound constructors in the
  Linear Programming package.

- Tim Sears reported the zgesdd problem also in intel mac.

- Max Suica simplified the installation on Windows and improved the instructions.

- John Billings first reported an incompatibility with QuickCheck>=2.1.1

- Alexey Khudyakov cleaned up PRAGMAS and fixed some hlint suggestions.

- Torsten Kemps-Benedix reported an installation problem in OS/X.

- Stefan Kersten fixed hmatrix.cabal for 64-bit ghc-7 in OS/X

- Sacha Sokoloski reported an installation problem on Arch Linux and
  helped with the configuration.

- Carter Schonwald helped with the configuration for Homebrew OS X and
  found a tolerance problem in test "1E5 rots". He also discovered
  a bug in the signature of cmap and fixed the cabal file.

- Duncan Coutts reported a problem with configure.hs and contributed
  a solution and a simplified Setup.lhs.

- Mark Wright fixed the import of vector >= 0.8.

- Bas van Dijk fixed the import of vector >= 0.8, got rid of some
  deprecation warnings, used more explicit imports, and updated to ghc-7.4.

- Tom Nielsen discovered a problem in Config.hs, exposed by link problems
  in Ubuntu 11.10 beta, and fixed the link options on freebsd.

- Daniel Fischer reported some Haddock markup errors.

- Danny Chan added support for integration over infinite intervals, and fixed
  Configure.hs using platform independent functions.

- Clark Gaebel removed superfluous thread safety.

- Jeffrey Burdges reported a glpk link problem on OS/X

- Jian Zhang reported the Windows installation problem due to new ODE interface.

- Mihaly Barasz and Ben Gamari fixed mapMatrix* and mapMatrixWithIndex

- Takano Akio fixed off-by-one errors in gsl-aux.c producing segfaults.

- Alex Lang implemented uniRoot and uniRootJ for one-dimensional root-finding, and
  fixed asRow and asColumn for empty vectors.

- Mike Ledger contributed alternative FFI helpers for matrix interoperation with C

- Stephen J. Barr suggested flipping argument order in the double integral example

- Greg Horn fixed the bus error on ghci 64-bit.

- Kristof Bastiaensen added bindings for one-dimensional minimization.

- Matthew Peddie added bindings for gsl_integrate_cquad doubly-adaptive quadrature
  for difficult integrands.

- Ben Gamari exposed matrixFromVector for Development.

- greg94301 reported tolerance issues in the tests.

- Clemens Lang updated the MacPort installation instructions.

- Henning Thielemann reported the pinv inefficient implementation and the need of
  pkgconfig-depends.

- bdoering reported the problem of zero absolute tolerance in the integration functions.

- Alexei Uimanov replaced fromList by Vector.fromList.

- Adam Vogt updated the code for ghc-7.7

- Mike Meyer (mwm) added freeBSD library configuration information.

- tfgit updated the OSX installation instructions via Homebrew

- "yokto" and "ttylec" reported the problem with the dot product of complex vectors.

- Samium Gromoff reported a build failure caused by a size_t - int mismatch.

- Denis Laxalde separated the gsl tests from the base ones.

- Dominic Steinitz (idontgetoutmuch) reported a bug in the static diagonal creation functions and
  added Cholesky to Static. He also added support for tridiagonal matrix solver and fixed several bugs.

- Dylan Thurston reported an error in the glpk documentation and ambiguity in
  the description of linearSolve.

- Adrian Victor Crisciu developed an installation method for platforms which
  don't provide shared lapack libraries.

- Ian Ross reported the max/minIndex bug.

- Niklas Hambüchen improved the documentation and fixed compilation with GHC-8.2
  adding type signatures. Added disable-default-paths flag.

- "erdeszt" optimized "conv" using a direct vector reverse.

- John Shahbazian added support for openBLAS.

- "yongqli" reported the bug in randomVector (rand() is not thread-safe and drand48_r() is not portable).

- Kiwamu Ishikura improved randomVector for OSX

- C.J. East fixed the examples for simplex.

- Ben Gamari contributed fixes for ghc 7.10

- Piotr Mardziel added general sparse constraints to simplex and the interface to glp_exact

- Maxim Baz fixed an instance declaration for ghc 7.11

- Thomas M. DuBuisson fixed a C include file.

- Matt Peddie wrote the interfaces to the interpolation and simulated annealing modules.

- "maxc01" solved uninstallability in FreeBSD, improved urandom, and fixed a Windows
  link error using rand_s.

- "ntfrgl" added {take,drop}Last{Rows,Columns} and odeSolveVWith with generalized step control function
   and fixed link errors related to mod/mod_l.

- "cruegge" discovered a bug in the conjugate gradient solver for sparse symmetric systems.

- Ilan Godik and Douglas McClean helped with Windows support.

- Vassil Keremidchiev fixed the cabal options for OpenBlas, fixed several installation
  issues, and added support for stack-based build. He also added support for LTS 8.15
  under Windows.

- Greg Nwosu fixed arm compilation

- Patrik Jansson changed meanCov and gaussianSample to use Herm type. Fixed stack.yaml.

- Justin Le added NFData instances for Static types, added mapping and outer product
  methods to Domain, and many other functions to the Static module.

- Sidharth Kapur added Normed and numeric instances for several Static types,
fixed the CPP issue in cabal files, and made many other contributions.

- Matt Renaud improved the documentation.

- Joshua Moerman fixed cabal/stack flags for windows.

- Francesco Mazzoli, Niklas Hambüchen, Patrick Chilton, and Andras Slemmer
  discovered a serious and subtle bug in the wrapper helpers causing memory corruption.
  Andras Slemmer fixed the bug. Thank you all.

- Kevin Slagle implemented thinQR and thinRQ, much faster than the original qr,
  and added compactSVDTol. He also added an optimized reorderVector for hTensor.

- "fedeinthemix" suggested a better name and a more general type for unitary.

- Huw Campbell fixed a bug in equal.

- Hiromi Ishii fixed compilation problems for ghc-8.4

