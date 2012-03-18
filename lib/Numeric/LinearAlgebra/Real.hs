-----------------------------------------------------------------------------
{- |
Module      :  Numeric.LinearAlgebra.Real
Copyright   :  (c) Alberto Ruiz 2012
License     :  GPL

Maintainer  :  Alberto Ruiz (aruiz at um dot es)
Stability   :  provisional

Additional functions for real arrays.

-}
-----------------------------------------------------------------------------

module Numeric.LinearAlgebra.Real(
    (<>), (<\>),
    vector,
    linspace,
    eye,
    zeros, ones,
    diagl,
    row,
    col,
    (#),(&), (//), blocks,
    rand, randn,
    module Numeric.LinearAlgebra
) where

import Numeric.LinearAlgebra hiding ((<>), (<\>), linspace)
import qualified Numeric.LinearAlgebra as LA
import System.Random(randomIO)

linspace :: Int -> (Double,Double) -> Vector Double
linspace = LA.linspace


infixl 7 <>
-- | Matrix product
(<>) ::Mul a b c => a Double -> b Double -> c Double
(<>) = (LA.<>)


infixl 7 <\>
-- | Least squares solution of a linear system
(<\>) ::LSDiv b c => Matrix Double -> b Double -> c Double
(<\>) = (LA.<\>)


-- | Pseudorandom matrix with uniform elements between 0 and 1.
randm :: RandDist
     -> Int -- ^ rows
     -> Int -- ^ columns
     -> IO (Matrix Double)
randm d r c = do
    seed <- randomIO
    return (reshape c $ randomVector seed d (r*c))

-- | Pseudorandom matrix with uniform elements between 0 and 1.
rand :: Int -> Int -> IO (Matrix Double)
rand = randm Uniform

-- | Pseudorandom matrix with normal elements
randn :: Int -> Int -> IO (Matrix Double)
randn = randm Gaussian

-- | Real identity matrix.
eye :: Int -> Matrix Double
eye = ident

-- | Create a real vector from a list.
vector :: [Double] -> Vector Double
vector = fromList

-- | Create a real diagonal matrix from a list.
diagl :: [Double] -> Matrix Double
diagl = diag . vector

-- | Create a matrix or zeros.
zeros :: Int -- ^ rows
      -> Int -- ^ columns
      -> Matrix Double
zeros r c = konst 0 (r,c)

-- | Create a matrix or ones.
ones :: Int -- ^ rows
     -> Int -- ^ columns
     -> Matrix Double
ones r c = konst 1 (r,c)

-- | Concatenation of real vectors.
infixl 3 #
(#) :: Vector Double -> Vector Double -> Vector Double
a # b = join [a,b]

-- | Horizontal concatenation of real matrices.
infixl 3 !
(!) :: Matrix Double -> Matrix Double -> Matrix Double
a ! b = fromBlocks [[a,b]]

-- | Vertical concatenation of real matrices.
(#) :: Matrix Double -> Matrix Double -> Matrix Double
infixl 2 #
a # b = fromBlocks [[a],[b]]


-- | Real block matrix from a rectangular list of lists.
blocks :: [[Matrix Double]] -> Matrix Double
blocks = fromBlocks

-- | A real matrix with a single row, created from a list of elements.
row :: [Double] -> Matrix Double
row = asRow . vector

-- | A real matrix with a single column, created from a list of elements.
col :: [Double] -> Matrix Double
col = asColumn . vector

