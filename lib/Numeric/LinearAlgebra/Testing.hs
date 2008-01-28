{-# OPTIONS_GHC -XPatternSignatures #-}
-----------------------------------------------------------------------------
{- |
Module      :  Numeric.LinearAlgebra.Testing
Copyright   :  (c) Alberto Ruiz 2007
License     :  GPL-style

Maintainer  :  Alberto Ruiz (aruiz at um dot es)
Stability   :  provisional
Portability :  portable

Some consistency tests.

-}

module Numeric.LinearAlgebra.Testing(
    runTests, runBigTests
) where

import Numeric.LinearAlgebra
import Test.QuickCheck
import Debug.Trace

qCheck n = check defaultConfig {configSize = const n}

debug x = trace (show x) x

type RM = Matrix Double
type CM = Matrix (Complex Double)

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


newtype (Sq a) = Sq (Matrix a) deriving Show
sq (Sq m) = m

instance (Element a, Arbitrary a) => Arbitrary (Sq a) where
    arbitrary = do
        n <- chooseDim
        l <- vector (n*n)
        return $ Sq $ (n><n) l
    coarbitrary = undefined

newtype (Rot a) = Rot (Matrix a) deriving Show
rot (Rot a) = a

instance (Field a, Arbitrary a) => Arbitrary (Rot a) where
    arbitrary = do
        Sq m <- arbitrary
        let (q,_) = qr m
        return (Rot q)
    coarbitrary = undefined

newtype (Her a) = Her (Matrix a) deriving Show
her (Her a) = a

instance (Field a, Arbitrary a) => Arbitrary (Her a) where
    arbitrary = do
        Sq m <- arbitrary
        let m' = m/2
        return $ Her (m' + ctrans m')
    coarbitrary = undefined

-------------------------------------------------------------------

herR x = her x :: RM

-- | It runs all the tests.
runTests :: Int  -- ^ maximum dimension
         -> IO ()
runTests n = do
    qCheck n (\(Her (m::CM))-> m == ctrans m)
    qCheck n $ (\m->m==ctrans m) . herR

-- | Some additional tests on big matrices. They take a few minutes.
runBigTests :: IO ()
runBigTests = undefined
