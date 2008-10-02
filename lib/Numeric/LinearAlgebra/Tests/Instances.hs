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
    Sq(..),     rSq,cSq,
    Rot(..),    rRot,cRot,
    Her(..),    rHer,cHer,
    WC(..),     rWC,cWC,
    SqWC(..),   rSqWC, cSqWC,
    PosDef(..), rPosDef, cPosDef,
    Consistent(..), rConsist, cConsist,
    RM,CM, rM,cM
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

-- a well-conditioned general matrix (the singular values are between 1 and 100)
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

-- a well-conditioned square matrix (the singular values are between 1 and 100)
newtype (SqWC a) = SqWC (Matrix a) deriving Show
instance (Field a, Arbitrary a) => Arbitrary (SqWC a) where
    arbitrary = do
        Sq m <- arbitrary
        let (u,_,v) = svd m
            n = rows m
        sv <- replicateM n (choose (1,100))
        let s = diag (fromList sv)
        return $ SqWC (u <> real s <> trans v)
    coarbitrary = undefined

-- a positive definite square matrix (the eigenvalues are between 0 and 100)
newtype (PosDef a) = PosDef (Matrix a) deriving Show
instance (Field a, Arbitrary a) => Arbitrary (PosDef a) where
    arbitrary = do
        Her m <- arbitrary
        let (_,v) = eigSH m
            n = rows m
        l <- replicateM n (choose (0,100))
        let s = diag (fromList l)
            p = v <> real s <> ctrans v
        return $ PosDef (0.5 .* p + 0.5 .* ctrans p)
    coarbitrary = undefined

-- a pair of matrices that can be multiplied
newtype (Consistent a) = Consistent (Matrix a, Matrix a) deriving Show
instance (Field a, Arbitrary a) => Arbitrary (Consistent a) where
    arbitrary = do
        n <- chooseDim
        k <- chooseDim
        m <- chooseDim
        la <- vector (n*k)
        lb <- vector (k*m)
        return $ Consistent ((n><k) la, (k><m) lb)
    coarbitrary = undefined


type RM = Matrix Double
type CM = Matrix (Complex Double)

rM m = m :: RM
cM m = m :: CM

rHer (Her m) = m :: RM
cHer (Her m) = m :: CM

rRot (Rot m) = m :: RM
cRot (Rot m) = m :: CM

rSq  (Sq m)  = m :: RM
cSq  (Sq m)  = m :: CM

rWC (WC m) = m :: RM
cWC (WC m) = m :: CM

rSqWC (SqWC m) = m :: RM
cSqWC (SqWC m) = m :: CM

rPosDef (PosDef m) = m :: RM
cPosDef (PosDef m) = m :: CM

rConsist (Consistent (a,b)) = (a,b::RM)
cConsist (Consistent (a,b)) = (a,b::CM)
