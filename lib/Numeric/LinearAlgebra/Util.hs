-----------------------------------------------------------------------------
{- |
Module      :  Numeric.LinearAlgebra.Util
Copyright   :  (c) Alberto Ruiz 2012
License     :  GPL

Maintainer  :  Alberto Ruiz (aruiz at um dot es)
Stability   :  provisional

-}
-----------------------------------------------------------------------------

module Numeric.LinearAlgebra.Util(
    disp,
    zeros, ones,
    diagl,
    row,
    col,
    (&),(!), (#),
    rand, randn,
    cross,
    norm
) where

import Numeric.LinearAlgebra
import System.Random(randomIO)


disp :: Int -> Matrix Double -> IO ()
-- ^ show a matrix with given number of digits after the decimal point
disp n = putStrLn . dispf n

-- | pseudorandom matrix with uniform elements between 0 and 1
randm :: RandDist
     -> Int -- ^ rows
     -> Int -- ^ columns
     -> IO (Matrix Double)
randm d r c = do
    seed <- randomIO
    return (reshape c $ randomVector seed d (r*c))

-- | pseudorandom matrix with uniform elements between 0 and 1
rand :: Int -> Int -> IO (Matrix Double)
rand = randm Uniform

-- | pseudorandom matrix with normal elements
randn :: Int -> Int -> IO (Matrix Double)
randn = randm Gaussian

-- | create a real diagonal matrix from a list
diagl :: [Double] -> Matrix Double
diagl = diag . fromList

-- | a real matrix of zeros
zeros :: Int -- ^ rows
      -> Int -- ^ columns
      -> Matrix Double
zeros r c = konst 0 (r,c)

-- | a real matrix of ones
ones :: Int -- ^ rows
     -> Int -- ^ columns
     -> Matrix Double
ones r c = konst 1 (r,c)

-- | concatenation of real vectors
infixl 3 &
(&) :: Vector Double -> Vector Double -> Vector Double
a & b = join [a,b]

-- | horizontal concatenation of real matrices
infixl 3 !
(!) :: Matrix Double -> Matrix Double -> Matrix Double
a ! b = fromBlocks [[a,b]]

-- | vertical concatenation of real matrices
(#) :: Matrix Double -> Matrix Double -> Matrix Double
infixl 2 #
a # b = fromBlocks [[a],[b]]

-- | create a single row real matrix from a list
row :: [Double] -> Matrix Double
row = asRow . fromList

-- | create a single column real matrix from a list
col :: [Double] -> Matrix Double
col = asColumn . fromList

cross :: Vector Double -> Vector Double -> Vector Double
-- ^ cross product of dimension 3 real vectors
cross x y | dim x == 3 && dim y == 3 = fromList [z1,z2,z3]
          | otherwise = error $ "cross ("++show x++") ("++show y++")"
  where
    [x1,x2,x3] = toList x
    [y1,y2,y3] = toList y
    z1 = x2*y3-x3*y2
    z2 = x3*y1-x1*y3
    z3 = x1*y2-x2*y1

norm :: Vector Double -> Double
-- ^ 2-norm of real vectors
norm = pnorm PNorm2


