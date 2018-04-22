A Haskell library for numerical computation
-------------------------------------------

A purely functional interface to linear algebra and other numerical
algorithms, internally implemented using [LAPACK][lapack],
[BLAS][blas], [GSL][gsl] and [SUNDIALS][sundials].

This package includes matrix decompositions (eigensystems, singular
values, Cholesky, QR, etc.), linear solvers, numeric integration, root
finding, etc.

- [What's new][changes] in version 0.19 (April 2018). This is not
  intended to be a breaking change but a lot of modules have been
  modified to ensure that continuous integration is green.

- Support for SUNDIALS has been added. It should be possible to
  replace Numeric.GSL.ODE with Numeric.Sundials.ARKode.ODE and have
  your program work as before bearing in mind that the methods and
  error control might differ (even for those with the same names!).

- [Code examples][examples]

- Source code and documentation (Hackage)
    - linear algebra: [hmatrix](http://hackage.haskell.org/package/hmatrix)
    - numeric algorithms: [hmatrix-gsl](http://hackage.haskell.org/package/hmatrix-gsl)
    - special functions: [hmatrix-special](http://hackage.haskell.org/package/hmatrix-special)
    - linear programming: [hmatrix-glpk](http://hackage.haskell.org/package/hmatrix-glpk)

- [Tutorial (old version)][tutorial]

- [Installation help][installation]

Contributions, suggestions, and bug reports are welcome!



[lapack]: http://www.netlib.org/lapack/
[blas]: http://www.netlib.org/blas/
[gsl]: http://www.gnu.org/software/gsl/
[sundials]: https://computation.llnl.gov/projects/sundials

[tutorial]: http://dis.um.es/profesores/alberto/material/hmatrix.pdf
[installation]: https://github.com/AlbertoRuiz/hmatrix/blob/master/INSTALL.md
[changes]: https://github.com/albertoruiz/hmatrix/tree/master/packages/base/CHANGELOG
[examples]: http://dis.um.es/~alberto/hmatrix/hmatrix.html


[hmatrix-static]: http://hackage.haskell.org/package/hmatrix-static
[hTensor]: https://github.com/AlbertoRuiz/hTensor
[hmatrix-gsl-stats]: http://hackage.haskell.org/package/hmatrix-gsl-stats
[hstatistics]: http://hackage.haskell.org/package/hstatistics
[hsignal]: http://hackage.haskell.org/package/hsignal
[pBLAS]: http://hackage.haskell.org/package/blas
[pLAPACK]: http://github.com/patperry/lapack
[aGSL]: http://hackage.haskell.org/package/bindings-gsl
[nprelude]: http://hackage.haskell.org/package/numeric-prelude
[mathHack]: http://hackage.haskell.org/packages/#cat:Math
[easyVision]: https://github.com/AlbertoRuiz/easyVision
[repa]: http://hackage.haskell.org/package/repa

