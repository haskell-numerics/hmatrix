-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Packed.Vector
-- Copyright   :  (c) Alberto Ruiz 2007
-- License     :  GPL-style
--
-- Maintainer  :  Alberto Ruiz <aruiz@um.es>
-- Stability   :  provisional
-- Portability :  portable
--
-- Vectors
--
-----------------------------------------------------------------------------

module Data.Packed.Vector (
    Vector(dim), Field,
    fromList, toList,
    at,
    subVector, join,
    constant,
    toComplex, comp,
    conj,
    dot
) where

import Data.Packed.Internal
import Complex

-- | creates a complex vector from vectors with real and imaginary parts
toComplex :: (Vector Double, Vector Double) ->  Vector (Complex Double)
toComplex (r,i) = asComplex $ cdat $ fromColumns [r,i]

-- | obtains the complex conjugate of a complex vector
conj :: Vector (Complex Double) -> Vector (Complex Double)
conj v = asComplex $ cdat $ reshape 2 (asReal v) `mulC` diag (fromList [1,-1])
    where mulC = multiply RowMajor

comp v = toComplex (v,constant (dim v) 0)


