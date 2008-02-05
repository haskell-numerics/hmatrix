{-# OPTIONS #-}
-----------------------------------------------------------------------------
{- |
Module      :  Numeric.LinearAlgebra.Tests.Instances
Copyright   :  (c) Alberto Ruiz 2008
License     :  GPL-style

Maintainer  :  Alberto Ruiz (aruiz at um dot es)
Stability   :  provisional
Portability :  portable

Arbitrary instances for vectors, matrices.

-}

module Numeric.LinearAlgebra.Tests.Instances(
    Sq(..),
    Rot(..),
    Her(..),
    WC(..)
) where

import Numeric.LinearAlgebra
import Test.QuickCheck
import Control.Monad(replicateM)

instance (Arbitrary a, RealFloat a) => Arbitrary (Complex a) where
    arbitrary = do
        r <- arbitrary
        i <- arbitrary
        return (r:+i)
    coarbitrary = undefined

chooseDim = sized $ \m -> choose (1,max 1 m)

instance (Field a, Arbitrary a) => Arbitrary (Vector a) where 
   arbitrary = do m <- chooseDim
                  l <- vector m
                  return $ fromList l
   coarbitrary = undefined

instance (Element a, Arbitrary a) => Arbitrary (Matrix a) where 
    arbitrary = do
        m <- chooseDim
        n <- chooseDim
        l <- vector (m*n)
        return $ (m><n) l
    coarbitrary = undefined

-- a square matrix
newtype (Sq a) = Sq (Matrix a) deriving Show
instance (Element a, Arbitrary a) => Arbitrary (Sq a) where
    arbitrary = do
        n <- chooseDim
        l <- vector (n*n)
        return $ Sq $ (n><n) l
    coarbitrary = undefined

-- a unitary matrix
newtype (Rot a) = Rot (Matrix a) deriving Show
instance (Field a, Arbitrary a) => Arbitrary (Rot a) where
    arbitrary = do
        Sq m <- arbitrary
        let (q,_) = qr m
        return (Rot q)
    coarbitrary = undefined

-- a complex hermitian or real symmetric matrix
newtype (Her a) = Her (Matrix a) deriving Show
instance (Field a, Arbitrary a) => Arbitrary (Her a) where
    arbitrary = do
        Sq m <- arbitrary
        let m' = m/2
        return $ Her (m' + ctrans m')
    coarbitrary = undefined

-- a well-conditioned matrix (the singular values are between 1 and 100)
newtype (WC a) = WC (Matrix a) deriving Show
instance (Field a, Arbitrary a) => Arbitrary (WC a) where
    arbitrary = do
        m <- arbitrary
        let (u,_,v) = svd m
            r = rows m
            c = cols m
            n = min r c
        sv <- replicateM n (choose (1,100))
        let s = diagRect (fromList sv) r c
        return $ WC (u <> real s <> trans v)
    coarbitrary = undefined
