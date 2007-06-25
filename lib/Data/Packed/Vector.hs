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
    (@>),
    subVector, join,
    constant,
    toComplex, comp,
    conj,
    dot,
    linspace
) where

import Data.Packed.Internal
import Complex
import GSL.Vector

-- | creates a complex vector from vectors with real and imaginary parts
toComplex :: (Vector Double, Vector Double) ->  Vector (Complex Double)
toComplex (r,i) = asComplex $ cdat $ fromColumns [r,i]

-- | obtains the complex conjugate of a complex vector
conj :: Vector (Complex Double) -> Vector (Complex Double)
conj v = asComplex $ cdat $ reshape 2 (asReal v) `mulC` diag (fromList [1,-1])
    where mulC = multiply RowMajor

comp :: Vector Double -> Vector (Complex Double)
comp v = toComplex (v,constant 0 (dim v))

{- | Creates a real vector containing a range of values:

@\> linspace 5 (-3,7)
5 |> [-3.0,-0.5,2.0,4.5,7.0]@
-}
linspace :: Int -> (Double, Double) -> Vector Double
linspace n (a,b) = fromList [a::Double,a+delta .. b]
    where delta = (b-a)/(fromIntegral n -1)

-- | Reads a vector position.
(@>) :: Field t => Vector t -> Int -> t
infixl 9 @>
(@>) = at
