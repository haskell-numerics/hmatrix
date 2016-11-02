{-# LANGUAGE CPP #-}
{- |
Module      :  Numeric.GSL.Polynomials
Copyright   :  (c) Alberto Ruiz 2006
License     :  GPL
Maintainer  :  Alberto Ruiz
Stability   :  provisional

Polynomials.

<http://www.gnu.org/software/gsl/manual/html_node/General-Polynomial-Equations.html#General-Polynomial-Equations>

-}


module Numeric.GSL.Polynomials (
    polySolve
) where

import Numeric.LinearAlgebra.HMatrix
import Numeric.GSL.Internal
import System.IO.Unsafe (unsafePerformIO)

#if __GLASGOW_HASKELL__ >= 704
import Foreign.C.Types (CInt(..))
#endif

{- | Solution of general polynomial equations, using /gsl_poly_complex_solve/.

For example, the three solutions of x^3 + 8 = 0

>>> polySolve [8,0,0,1]
[(-2.0) :+ 0.0,1.0 :+ 1.7320508075688776,1.0 :+ (-1.7320508075688776)]


The example in the GSL manual: To find the roots of x^5 -1 = 0:

>>> polySolve [-1, 0, 0, 0, 0, 1]
[(-0.8090169943749472) :+ 0.5877852522924731,
(-0.8090169943749472) :+ (-0.5877852522924731),
0.30901699437494756 :+ 0.9510565162951535,
0.30901699437494756 :+ (-0.9510565162951535),
1.0000000000000002 :+ 0.0]

-}
polySolve :: [Double] -> [Complex Double]
polySolve = toList . polySolve' . fromList

polySolve' :: Vector Double -> Vector (Complex Double)
polySolve' v | size v > 1 = unsafePerformIO $ do
    r <- createVector (size v-1)
    (v `applyRaw` (r `applyRaw` id)) c_polySolve #| "polySolve"
    return r
             | otherwise = error "polySolve on a polynomial of degree zero"

foreign import ccall unsafe "gsl-aux.h polySolve" c_polySolve:: TV (TCV Res)

