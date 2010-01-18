{-# LANGUAGE FlexibleContexts, UndecidableInstances, CPP #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
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
import Control.Monad(replicateM)
#include "quickCheckCompat.h"


#if MIN_VERSION_QuickCheck(2,0,0)
shrinkListElementwise :: (Arbitrary a) => [a] -> [[a]]
shrinkListElementwise []     = []
shrinkListElementwise (x:xs) = [ y:xs | y  <- shrink x                 ]
                            ++ [ x:ys | ys <- shrinkListElementwise xs ]

shrinkPair :: (Arbitrary a, Arbitrary b) => (a,b) -> [(a,b)]
shrinkPair (a,b) = [ (a,x) | x <- shrink b ] ++ [ (x,b) | x <- shrink a ]
#endif


instance (Arbitrary a, RealFloat a) => Arbitrary (Complex a) where
    arbitrary = do
        re <- arbitrary
        im <- arbitrary
        return (re :+ im)

#if MIN_VERSION_QuickCheck(2,0,0)
    shrink (re :+ im) = 
        [ u :+ v | (u,v) <- shrinkPair (re,im) ]
#else
    -- this has been moved to the 'Coarbitrary' class in QuickCheck 2
    coarbitrary = undefined 
#endif

chooseDim = sized $ \m -> choose (1,max 1 m)

instance (Field a, Arbitrary a) => Arbitrary (Vector a) where 
    arbitrary = do m <- chooseDim
                   l <- vector m
                   return $ fromList l

#if MIN_VERSION_QuickCheck(2,0,0)
    -- shrink any one of the components
    shrink = map fromList . shrinkListElementwise . toList
                              
#else
    coarbitrary = undefined
#endif

instance (Element a, Arbitrary a) => Arbitrary (Matrix a) where 
    arbitrary = do
        m <- chooseDim
        n <- chooseDim
        l <- vector (m*n)
        return $ (m><n) l

#if MIN_VERSION_QuickCheck(2,0,0)
    -- shrink any one of the components
    shrink a = map ((rows a) >< (cols a))
               . shrinkListElementwise
               . concat . toLists 
                     $ a
#else
    coarbitrary = undefined
#endif


-- a square matrix
newtype (Sq a) = Sq (Matrix a) deriving Show
instance (Element a, Arbitrary a) => Arbitrary (Sq a) where
    arbitrary = do
        n <- chooseDim
        l <- vector (n*n)
        return $ Sq $ (n><n) l

#if MIN_VERSION_QuickCheck(2,0,0)
    shrink (Sq a) = [ Sq b | b <- shrink a ]
#else
    coarbitrary = undefined
#endif


-- a unitary matrix
newtype (Rot a) = Rot (Matrix a) deriving Show
instance (Field a, Arbitrary a) => Arbitrary (Rot a) where
    arbitrary = do
        Sq m <- arbitrary
        let (q,_) = qr m
        return (Rot q)

#if MIN_VERSION_QuickCheck(2,0,0)
#else
    coarbitrary = undefined
#endif


-- a complex hermitian or real symmetric matrix
newtype (Her a) = Her (Matrix a) deriving Show
instance (Field a, Arbitrary a, Num (Vector a)) => Arbitrary (Her a) where
    arbitrary = do
        Sq m <- arbitrary
        let m' = m/2
        return $ Her (m' + ctrans m')

#if MIN_VERSION_QuickCheck(2,0,0)
#else
    coarbitrary = undefined
#endif


-- a well-conditioned general matrix (the singular values are between 1 and 100)
newtype (WC a) = WC (Matrix a) deriving Show
instance (Field a, Arbitrary a) => Arbitrary (WC a) where
    arbitrary = do
        m <- arbitrary
        let (u,_,v) = svd m
            r = rows m
            c = cols m
            n = min r c
        sv' <- replicateM n (choose (1,100))
        let s = diagRect (fromList sv') r c
        return $ WC (u <> real s <> trans v)

#if MIN_VERSION_QuickCheck(2,0,0)
#else
    coarbitrary = undefined
#endif


-- a well-conditioned square matrix (the singular values are between 1 and 100)
newtype (SqWC a) = SqWC (Matrix a) deriving Show
instance (Field a, Arbitrary a) => Arbitrary (SqWC a) where
    arbitrary = do
        Sq m <- arbitrary
        let (u,_,v) = svd m
            n = rows m
        sv' <- replicateM n (choose (1,100))
        let s = diag (fromList sv')
        return $ SqWC (u <> real s <> trans v)

#if MIN_VERSION_QuickCheck(2,0,0)
#else
    coarbitrary = undefined
#endif


-- a positive definite square matrix (the eigenvalues are between 0 and 100)
newtype (PosDef a) = PosDef (Matrix a) deriving Show
instance (Field a, Arbitrary a, Num (Vector a)) => Arbitrary (PosDef a) where
    arbitrary = do
        Her m <- arbitrary
        let (_,v) = eigSH m
            n = rows m
        l <- replicateM n (choose (0,100))
        let s = diag (fromList l)
            p = v <> real s <> ctrans v
        return $ PosDef (0.5 * p + 0.5 * ctrans p)

#if MIN_VERSION_QuickCheck(2,0,0)
#else
    coarbitrary = undefined
#endif


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

#if MIN_VERSION_QuickCheck(2,0,0)
    shrink (Consistent (x,y)) = [ Consistent (u,v) | (u,v) <- shrinkPair (x,y) ]
#else
    coarbitrary = undefined
#endif



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
