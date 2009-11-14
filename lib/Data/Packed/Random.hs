-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Packed.Vector
-- Copyright   :  (c) Alberto Ruiz 2009
-- License     :  GPL
--
-- Maintainer  :  Alberto Ruiz <aruiz@um.es>
-- Stability   :  provisional
--
-- Random vectors and matrices.
--
-----------------------------------------------------------------------------

module Data.Packed.Random (
    RandDist(..),
    randomVector,
    gaussianSample,
    meanCov,
) where

import Numeric.GSL.Vector
import Data.Packed
import Numeric.LinearAlgebra.Algorithms
import Numeric.LinearAlgebra.Instances()
import Numeric.LinearAlgebra.Interface

-- | Obtains a matrix whose rows are pseudorandom samples from a multivariante
-- Gaussian distribution.
gaussianSample :: Int -- ^ seed
               -> Int -- ^ number of rows
               -> Vector Double -- ^ mean vector
               -> Matrix Double -- ^ covariance matrix
               -> Matrix Double -- ^ result
gaussianSample seed n med cov = m where
    (l,v) = eigSH' cov
    c = dim l
    meds = constant 1 n `outer` med
    rs = reshape c $ randomVector seed Gaussian (c * n)
    ds = sqrt (abs l)
    m = rs <> (diag ds <> trans v) + meds

------------ utilities -------------------------------

-- | Compute mean vector and covariance matrix of the rows of a matrix.
meanCov :: Matrix Double -> (Vector Double, Matrix Double)
meanCov x = (med,cov) where
    r    = rows x
    k    = 1 / fromIntegral r
    med  = constant k r <> x
    meds = constant 1 r `outer` med
    xc   = x - meds
    cov  = (trans xc <> xc) / fromIntegral (r-1)
