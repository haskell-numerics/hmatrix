A Haskell library for numerical computation
-------------------------------------------

A purely functional interface to linear algebra and other numerical algorithms, internally implemented using [LAPACK][lapack], [BLAS][blas], and [GSL][gsl].

This package includes standard matrix decompositions (eigensystems, singular values, Cholesky, QR, etc.), linear systems, numeric integration, root finding, etc.

- [Tutorial][tutorial]

- [Source code and documentation (Hackage)][source]

- [Installation help and known problems][Installation3]

- Source repository: [https://github.com/AlbertoRuiz/hmatrix][repo3]

- [Changelog][changes3]

Contributions, suggestions, and bug reports are welcome!

Related Projects
----------------

- [hmatrix-static][hmatrix-static], by Reiner Pope.
  A thin, lightweight wrapper over hmatrix to
  support static checking of matrix and vector sizes (for instance, addition
  of different-sized vectors will be disallowed at compile-time).

- [hmatrix-gsl-stats][hmatrix-gsl-stats], [hstatistics][hstatistics],
  and [hsignal][hsignal] by Vivian McPhail.

- [repa][repa], regular, multi-dimensional, shape polymorphic parallel arrays.

- [hTensor][hTensor], multidimensional arrays and simple tensor computations.

- [BLAS][pBLAS] and [LAPACK][pLAPACK] Haskell bindings by Patrick Perry.

- [GSL Haskell bindings][aGSL] by Mauricio C. Antunes.

- The alternative [numeric prelude][nprelude] by Dylan Thurston, Henning Thielemann,
  and Mikael Johansson.

- [Math packages][mathHack] in Hackage.

- [easyVision][easyVision]: image processing and computer vision.


[lapack]: http://www.netlib.org/lapack/
[blas]: http://www.netlib.org/blas/
[gsl]: http://www.gnu.org/software/gsl/

[source]: http://hackage.haskell.org/package/hmatrix
[tutorial]: http://dis.um.es/profesores/alberto/material/hmatrix.pdf
[installation]: http://code.haskell.org/hmatrix/install.html
[installation2]: http://perception.inf.um.es/hmatrix/install.html
[installation3]: https://github.com/AlbertoRuiz/hmatrix/blob/master/INSTALL.md
[repo]: http://perception.inf.um.es/cgi-bin/darcsweb.cgi?r=hmatrix;a=summary
[repo2]: http://patch-tag.com/r/aruiz/hmatrix/
[repo3]: https://github.com/AlbertoRuiz/hmatrix
[changes]: http://code.haskell.org/hmatrix/CHANGES
[changes2]: http://patch-tag.com/r/aruiz/hmatrix/snapshot/current/content/pretty/CHANGES
[changes3]: https://github.com/AlbertoRuiz/hmatrix/blob/master/CHANGES.md
[docum]:http://perception.inf.um.es/hmatrix/doc


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

