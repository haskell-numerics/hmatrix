{-# LANGUAGE FlexibleContexts, UndecidableInstances, FlexibleInstances, ScopedTypeVariables #-}
-----------------------------------------------------------------------------
{- |
Module      :  Numeric.LinearAlgebra.Tests.Instances
Copyright   :  (c) Alberto Ruiz 2008
License     :  BSD3
Maintainer  :  Alberto Ruiz
Stability   :  provisional

Arbitrary instances for vectors, matrices.

-}

module Numeric.LinearAlgebra.Tests.Instances(
    Sq(..),     rSq,cSq,
    Rot(..),    rRot,cRot,
                rHer,cHer,
    WC(..),     rWC,cWC,
    SqWC(..),   rSqWC, cSqWC, rSymWC, cSymWC,
    PosDef(..), rPosDef, cPosDef,
    Consistent(..), rConsist, cConsist,
    RM,CM, rM,cM,
    FM,ZM, fM,zM
) where

import System.Random

import Numeric.LinearAlgebra.HMatrix hiding (vector)
import Control.Monad(replicateM)
import Test.QuickCheck(Arbitrary,arbitrary,choose,vector,sized,shrink)

import GHC.TypeLits
import Data.Proxy (Proxy(..))
import qualified Numeric.LinearAlgebra.Static as Static


shrinkListElementwise :: (Arbitrary a) => [a] -> [[a]]
shrinkListElementwise []     = []
shrinkListElementwise (x:xs) = [ y:xs | y  <- shrink x                 ]
                            ++ [ x:ys | ys <- shrinkListElementwise xs ]

shrinkPair :: (Arbitrary a, Arbitrary b) => (a,b) -> [(a,b)]
shrinkPair (a,b) = [ (a,x) | x <- shrink b ] ++ [ (x,b) | x <- shrink a ]

chooseDim = sized $ \m -> choose (1,max 1 m)

instance (Field a, Arbitrary a) => Arbitrary (Vector a) where
    arbitrary = do m <- chooseDim
                   l <- vector m
                   return $ fromList l
    -- shrink any one of the components
    shrink = map fromList . shrinkListElementwise . toList

instance KnownNat n => Arbitrary (Static.R n) where
    arbitrary = do
      l <- vector n
      return (Static.fromList l)

      where
        n :: Int
        n = fromIntegral (natVal (Proxy :: Proxy n))

    shrink v = []

instance (Element a, Arbitrary a) => Arbitrary (Matrix a) where
    arbitrary = do
        m <- chooseDim
        n <- chooseDim
        l <- vector (m*n)
        return $ (m><n) l

    -- shrink any one of the components
    shrink a = map (rows a >< cols a)
               . shrinkListElementwise
               . concat . toLists
                     $ a

instance (KnownNat n, KnownNat m) => Arbitrary (Static.L m n) where
    arbitrary = do
      l <- vector (m * n)
      return (Static.fromList l)

      where
        m :: Int
        m = fromIntegral (natVal (Proxy :: Proxy m))

        n :: Int
        n = fromIntegral (natVal (Proxy :: Proxy n))

    shrink mat = []

-- a square matrix
newtype (Sq a) = Sq (Matrix a) deriving Show
instance (Element a, Arbitrary a) => Arbitrary (Sq a) where
    arbitrary = do
        n <- chooseDim
        l <- vector (n*n)
        return $ Sq $ (n><n) l

    shrink (Sq a) = [ Sq b | b <- shrink a ]


-- a unitary matrix
newtype (Rot a) = Rot (Matrix a) deriving Show
instance (Field a, Arbitrary a) => Arbitrary (Rot a) where
    arbitrary = do
        Sq m <- arbitrary
        let (q,_) = qr m
        return (Rot q)


-- a complex hermitian or real symmetric matrix
instance (Field a, Arbitrary a, Num (Vector a)) => Arbitrary (Herm a) where
    arbitrary = do
        Sq m <- arbitrary
        let m' = m/2
        return $ sym m'


class (Field a, Arbitrary a, Element (RealOf a), Random (RealOf a)) => ArbitraryField a
instance ArbitraryField Double
instance ArbitraryField (Complex Double)


-- a well-conditioned general matrix (the singular values are between 1 and 100)
newtype (WC a) = WC (Matrix a) deriving Show
instance (Numeric a, ArbitraryField a) => Arbitrary (WC a) where
    arbitrary = do
        m <- arbitrary
        let (u,_,v) = svd m
            r = rows m
            c = cols m
            n = min r c
        sv' <- replicateM n (choose (1,100))
        let s = diagRect 0 (fromList sv') r c
        return $ WC (u <> real s <> tr v)


-- a well-conditioned square matrix (the singular values are between 1 and 100)
newtype (SqWC a) = SqWC (Matrix a) deriving Show
instance (ArbitraryField a, Numeric a) => Arbitrary (SqWC a) where
    arbitrary = do
        Sq m <- arbitrary
        let (u,_,v) = svd m
            n = rows m
        sv' <- replicateM n (choose (1,100))
        let s = diag (fromList sv')
        return $ SqWC (u <> real s <> tr v)


-- a positive definite square matrix (the eigenvalues are between 0 and 100)
newtype (PosDef a) = PosDef (Matrix a) deriving Show
instance (Numeric a, ArbitraryField a, Num (Vector a))
    => Arbitrary (PosDef a) where
    arbitrary = do
        m <- arbitrary
        let (_,v) = eigSH m
            n = rows (unSym m)
        l <- replicateM n (choose (0,100))
        let s = diag (fromList l)
            p = v <> real s <> tr v
        return $ PosDef (0.5 * p + 0.5 * tr p)


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

    shrink (Consistent (x,y)) = [ Consistent (u,v) | (u,v) <- shrinkPair (x,y) ]



type RM = Matrix Double
type CM = Matrix (Complex Double)
type FM = Matrix Float
type ZM = Matrix (Complex Float)


rM m = m :: RM
cM m = m :: CM
fM m = m :: FM
zM m = m :: ZM


rHer m = unSym m :: RM
cHer m = unSym m :: CM

rRot (Rot m) = m :: RM
cRot (Rot m) = m :: CM

rSq  (Sq m)  = m :: RM
cSq  (Sq m)  = m :: CM

rWC (WC m) = m :: RM
cWC (WC m) = m :: CM

rSqWC (SqWC m) = m :: RM
cSqWC (SqWC m) = m :: CM

rSymWC (SqWC m) = sym m :: Herm R
cSymWC (SqWC m) = sym m :: Herm C

rPosDef (PosDef m) = m :: RM
cPosDef (PosDef m) = m :: CM

rConsist (Consistent (a,b)) = (a,b::RM)
cConsist (Consistent (a,b)) = (a,b::CM)

