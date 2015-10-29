-----------------------------------------------------------------------------
-- |
-- Module      :  Numeric.LinearAlgebra.Random
-- Copyright   :  (c) Alberto Ruiz 2009-14
-- License     :  BSD3
-- Maintainer  :  Alberto Ruiz
-- Stability   :  provisional
--
-- Random vectors and matrices.
--
-----------------------------------------------------------------------------

module Internal.Random (
    Seed,
    RandDist(..),
    randomVector,
    gaussianSample,
    uniformSample,
    rand, randn
) where

import Internal.Vectorized
import Internal.Vector
import Internal.Matrix
import Internal.Numeric
import Internal.Algorithms
import System.Random(randomIO)

-- | Obtains a matrix whose rows are pseudorandom samples from a multivariate
-- Gaussian distribution.
gaussianSample :: Seed
               -> Int -- ^ number of rows
               -> Vector Double -- ^ mean vector
               -> Herm Double   -- ^ covariance matrix
               -> Matrix Double -- ^ result
gaussianSample seed n med cov = m where
    c = dim med
    meds = konst' 1 n `outer` med
    rs = reshape c $ randomVector seed Gaussian (c * n)
    m = rs `mXm` chol cov `add` meds

-- | Obtains a matrix whose rows are pseudorandom samples from a multivariate
-- uniform distribution.
uniformSample :: Seed
               -> Int -- ^ number of rows
               -> [(Double,Double)] -- ^ ranges for each column
               -> Matrix Double -- ^ result
uniformSample seed n rgs = m where
    (as,bs) = unzip rgs
    a = fromList as
    cs = zipWith subtract as bs
    d = dim a
    dat = toRows $ reshape n $ randomVector seed Uniform (n*d)
    am = konst' 1 n `outer` a
    m = fromColumns (zipWith scale cs dat) `add` am

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

{- | pseudorandom matrix with normal elements

>>> disp 3 =<< randn 3 5
3x5
0.386  -1.141   0.491  -0.510   1.512
0.069  -0.919   1.022  -0.181   0.745
0.313  -0.670  -0.097  -1.575  -0.583

-}
randn :: Int -> Int -> IO (Matrix Double)
randn = randm Gaussian

