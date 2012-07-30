{-# LANGUAGE FlexibleContexts #-}
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
    -- * Convenience functions for real elements
    disp,
    zeros, ones,
    diagl,
    row,
    col,
    (&),(!), (#),
    rand, randn,
    cross,
    norm,
    -- * Convolution
    -- ** 1D
    corr, conv, corrMin,
    -- ** 2D
    corr2, conv2, separable,
    -- * Tools for the Kronecker product
    --
    -- | (see A. Fusiello, A matter of notation: Several uses of the Kronecker product in
    --  3d computer vision, Pattern Recognition Letters 28 (15) (2007) 2127-2132)

    --
    -- | @`vec` (a \<> x \<> b) == ('trans' b ` 'kronecker' ` a) \<> 'vec' x@
    vec,
    vech,
    dup,
    vtrans
) where

import Numeric.LinearAlgebra hiding (i)
import System.Random(randomIO)
import Numeric.LinearAlgebra.Util.Convolution


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
-- ^ cross product (for three-element real vectors)
cross x y | dim x == 3 && dim y == 3 = fromList [z1,z2,z3]
          | otherwise = error $ "cross ("++show x++") ("++show y++")"
  where
    [x1,x2,x3] = toList x
    [y1,y2,y3] = toList y
    z1 = x2*y3-x3*y2
    z2 = x3*y1-x1*y3
    z3 = x1*y2-x2*y1

norm :: Vector Double -> Double
-- ^ 2-norm of real vector
norm = pnorm PNorm2

--------------------------------------------------------------------------------

vec :: Element t => Matrix t -> Vector t
-- ^ stacking of columns
vec = flatten . trans


vech :: Element t => Matrix t -> Vector t
-- ^ half-vectorization (of the lower triangular part)
vech m = join . zipWith f [0..] . toColumns $ m
  where
    f k v = subVector k (dim v - k) v


dup :: (Num t, Num (Vector t), Element t) => Int -> Matrix t
-- ^ duplication matrix (@'dup' k \<> 'vech' m == 'vec' m@, for symmetric m of 'dim' k)
dup k = trans $ fromRows $ map f es
  where
    rs = zip [0..] (toRows (ident (k^(2::Int))))
    es = [(i,j) | j <- [0..k-1], i <- [0..k-1], i>=j ]
    f (i,j) | i == j = g (k*j + i)
            | otherwise = g (k*j + i) + g (k*i + j)
    g j = v
      where
        Just v = lookup j rs


vtrans :: Element t => Int -> Matrix t -> Matrix t
-- ^ generalized \"vector\" transposition: @'vtrans' 1 == 'trans'@, and @'vtrans' ('rows' m) m == 'asColumn' ('vec' m)@
vtrans p m | r == 0 = fromBlocks . map (map asColumn . takesV (replicate q p)) . toColumns $ m
           | otherwise = error $ "vtrans " ++ show p ++ " of matrix with " ++ show (rows m) ++ " rows"
  where
    (q,r) = divMod (rows m) p

