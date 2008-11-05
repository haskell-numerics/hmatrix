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
-- A representation of 1D arrays suitable for numeric computations using external libraries.
--
-----------------------------------------------------------------------------

module Data.Packed.Vector (
    Vector,
    fromList, (|>), toList,
    dim, (@>),
    subVector, join,
    constant, linspace,
    vectorMax, vectorMin, vectorMaxIndex, vectorMinIndex,
    liftVector, liftVector2
) where

import Data.Packed.Internal
import Numeric.GSL.Vector
import Data.Packed.ST

{- | Creates a real vector containing a range of values:

@\> linspace 5 (-3,7)
5 |> [-3.0,-0.5,2.0,4.5,7.0]@
-}
linspace :: Int -> (Double, Double) -> Vector Double
linspace n (a,b) = add a $ scale s  $ fromList [0 .. fromIntegral n-1]
    where scale = vectorMapValR Scale
          add   = vectorMapValR AddConstant
          s = (b-a)/fromIntegral (n-1)

vectorMax :: Vector Double -> Double
vectorMax = toScalarR Max

vectorMin :: Vector Double -> Double
vectorMin = toScalarR Min

vectorMaxIndex :: Vector Double -> Int
vectorMaxIndex = round . toScalarR MaxIdx

vectorMinIndex :: Vector Double -> Int
vectorMinIndex = round . toScalarR MinIdx

{- | creates a vector with a given number of equal components:

@> constant 2 7
7 |> [2.0,2.0,2.0,2.0,2.0,2.0,2.0]@
-}
constant :: Element a => a -> Int -> Vector a
constant x n = runSTVector (newVector x n)
