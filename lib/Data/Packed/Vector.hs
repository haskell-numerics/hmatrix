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
    Vector,
    fromList, (|>), toList,
    dim, (@>),
    subVector, join,
    constant, linspace,

    liftVector, liftVector2
) where

import Data.Packed.Internal
import Complex

{- | Creates a real vector containing a range of values:

@\> linspace 5 (-3,7)
5 |> [-3.0,-0.5,2.0,4.5,7.0]@
-}
linspace :: Int -> (Double, Double) -> Vector Double
linspace n (a,b) = fromList [a, a+delta .. b]
    where delta = (b-a)/(fromIntegral n -1)
