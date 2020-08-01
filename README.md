A Haskell library for numerical computation
-------------------------------------------

A purely functional interface to linear algebra and other numerical
algorithms, internally implemented using [LAPACK][lapack],
[BLAS][blas], [GSL][gsl] and [GLPK][glpk].

This package includes matrix decompositions (eigensystems, singular
values, Cholesky, QR, etc.), linear solvers, numeric integration, root
finding, etc.

- [What's new][changes] in version 0.19 (April 2018). This is not
  intended to be a breaking change but a lot of modules have been
  modified to ensure that continuous integration is green.

- [Code examples][examples]

- Source code and documentation (Hackage)
    - linear algebra: [hmatrix][hmatrix]
    - numeric algorithms: [hmatrix-gsl][hmatrix-gsl]
    - special functions: [hmatrix-special][hmatrix-special]
    - linear programming: [hmatrix-glpk][hmatrix-glpk]

- [Tutorial (old version)][tutorial]

- [Installation help][installation]

For numerical algorithms internally implemented using [SUNDIALS][sundials], see
package [hmatrix-sundials] and its separate respository.

Contributions, suggestions, and bug reports are welcome!



[lapack]: https://www.netlib.org/lapack/
[blas]: https://www.netlib.org/blas/
[glpk]: https://www.gnu.org/software/glpk/
[gsl]: https://www.gnu.org/software/gsl/
[sundials]: https://computation.llnl.gov/projects/sundials

[tutorial]: http://dis.um.es/profesores/alberto/material/hmatrix.pdf
[installation]: https://github.com/AlbertoRuiz/hmatrix/blob/master/INSTALL.md
[changes]: https://github.com/albertoruiz/hmatrix/tree/master/packages/base/CHANGELOG
[examples]: http://dis.um.es/~alberto/hmatrix/hmatrix.html

[hmatrix]: https://hackage.haskell.org/package/hmatrix
[hmatrix-glpk]: https://hackage.haskell.org/package/hmatrix-glpk
[hmatrix-gsl]: https://hackage.haskell.org/package/hmatrix-gsl
[hmatrix-gsl-stats]: https://hackage.haskell.org/package/hmatrix-gsl-stats
[hmatrix-special]: https://hackage.haskell.org/package/hmatrix-special
[hmatrix-static]: https://hackage.haskell.org/package/hmatrix-static
[hmatrix-sundials]: https://hackage.haskell.org/package/hmatrix-sundials
[hstatistics]: https://hackage.haskell.org/package/hstatistics
[hsignal]: https://hackage.haskell.org/package/hsignal
[hTensor]: https://github.com/AlbertoRuiz/hTensor
[pBLAS]: https://hackage.haskell.org/package/blas
[pLAPACK]: https://github.com/patperry/lapack
[aGSL]: https://hackage.haskell.org/package/bindings-gsl
[nprelude]: https://hackage.haskell.org/package/numeric-prelude
[mathHack]: https://hackage.haskell.org/packages/#cat:Math
[easyVision]: https://github.com/AlbertoRuiz/easyVision
[repa]: https://hackage.haskell.org/package/repa
