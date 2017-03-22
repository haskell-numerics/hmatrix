{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-unused-imports -fno-warn-incomplete-patterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ViewPatterns #-}

-----------------------------------------------------------------------------
{- |
Module      :  Numeric.LinearAlgebra.Tests
Copyright   :  (c) Alberto Ruiz 2007-14
License     :  BSD3
Maintainer  :  Alberto Ruiz
Stability   :  provisional

Some tests.

-}

module Numeric.LinearAlgebra.Tests(
--  module Numeric.LinearAlgebra.Tests.Instances,
--  module Numeric.LinearAlgebra.Tests.Properties,
   qCheck,
   utest,
   runTests,
   runBenchmarks
 , binaryTests
-- , findNaN
--, runBigTests
) where

import Numeric.LinearAlgebra hiding (unitary)
import Numeric.LinearAlgebra.Devel
import Numeric.LinearAlgebra.Static(L)
import Numeric.LinearAlgebra.Tests.Instances
import Numeric.LinearAlgebra.Tests.Properties
import Test.HUnit hiding ((~:),test,Testable,State)
import System.Info
import Data.List(foldl1')
import Prelude hiding ((^))
import qualified Prelude
import System.CPUTime
import System.Exit
import Text.Printf
import Numeric.LinearAlgebra.Devel(unsafeFromForeignPtr,unsafeToForeignPtr)
import Control.Arrow((***))
import Debug.Trace
import Control.Monad(when)
import Control.Applicative
import Control.Monad(ap)
import Control.DeepSeq ( NFData(..) )

import Test.QuickCheck(Arbitrary,arbitrary,coarbitrary,choose,vector
                      ,sized,classify,Testable,Property
                      ,quickCheckWithResult,maxSize,stdArgs,shrink)
import qualified Test.QuickCheck as T

import Test.QuickCheck.Test(isSuccess)

--eps = peps :: Double
--i = 0:+1 :: Complex Double

qCheck n x = do
    r <- quickCheckWithResult stdArgs {maxSize = n} x
    when (not $ isSuccess r) (exitFailure)

a ^ b = a Prelude.^ (b :: Int)

utest str b = TestCase $ assertBool str b

feye n = flipud (ident n) :: Matrix Double


-----------------------------------------------------------

detTest1 = det m == 26
        && det mc == 38 :+ (-3)
        && det (feye 2) == -1
    where
        m = (3><3)
            [ 1, 2, 3
            , 4, 5, 7
            , 2, 8, 4 :: Double
            ]
        mc = (3><3)
            [ 1, 2, 3
            , 4, 5, 7
            , 2, 8, iC
            ]

detTest2 = inv1 |~| inv2 && [det1] ~~ [det2]
  where
    m = complex (feye 6)
    inv1 = inv m
    det1 = det m
    (inv2,(lda,sa)) = invlndet m
    det2 = sa * exp lda

---------------------------------------------------------------------

nd1 = (3><3) [ 1/2, 1/4, 1/4
             , 0/1, 1/2, 1/4
             , 1/2, 1/4, 1/2 :: Double]

nd2 = (2><2) [1, 0, 1, 1:: Complex Double]

expmTest1 = expm nd1 :~14~: (3><3)
 [ 1.762110887278176
 , 0.478085470590435
 , 0.478085470590435
 , 0.104719410945666
 , 1.709751181805343
 , 0.425725765117601
 , 0.851451530235203
 , 0.530445176063267
 , 1.814470592751009 ]

expmTest2 = expm nd2 :~15~: (2><2)
 [ 2.718281828459045
 , 0.000000000000000
 , 2.718281828459045
 , 2.718281828459045 ]

-----------------------------------------------------

mbCholTest = utest "mbCholTest" (ok1 && ok2) where
    m1 = (2><2) [2,5,5,8 :: Double]
    m2 = (2><2) [3,5,5,9 :: Complex Double]
    ok1 = mbChol (trustSym m1) == Nothing
    ok2 = mbChol (trustSym m2) == Just (chol $ trustSym m2)

-----------------------------------------------------

triTest = utest "triTest" ok1 where

  a :: Matrix R
  a = (4><4)
    [
       4.30,  0.00,  0.00, 0.00,
      -3.96, -4.87,  0.00, 0.00,
       0.40,  0.31, -8.02, 0.00,
      -0.27,  0.07, -5.95, 0.12
    ]

  w :: Matrix R
  w = (4><2)
    [
      -12.90, -21.50,
       16.75,  14.93,
      -17.55,   6.33,
      -11.04,   8.09
    ]

  v :: Matrix R
  v = triSolve Lower a w

  e :: Matrix R
  e = (4><2)
    [
      -3.0000, -5.0000,
      -1.0000,  1.0000,
       2.0000, -1.0000,
       1.0000,  6.0000
    ]

  ok1 = (maximum $ map abs $ concat $ toLists $ e - v) <= 1e-14

-----------------------------------------------------

triDiagTest = utest "triDiagTest" (ok1 && ok2) where

  dL, d, dU :: Vector Double
  dL =  fromList [3.0, 3.0, 3.0, 3.0, 3.0, 3.0, 3.0, 3.0]
  d  =  fromList [1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0]
  dU =  fromList [4.0, 4.0, 4.0, 4.0, 4.0, 4.0, 4.0, 4.0]

  b :: Matrix R
  b = (9><3)
    [
      1.0,   1.0,   1.0,
      1.0,  -1.0,   2.0,
      1.0,   1.0,   3.0,
      1.0,  -1.0,   4.0,
      1.0,   1.0,   5.0,
      1.0,  -1.0,   6.0,
      1.0,   1.0,   7.0,
      1.0,  -1.0,   8.0,
      1.0,   1.0,   9.0
    ]

  y :: Matrix R
  y = (9><9)
    [
      1.0, 4.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0,
      3.0, 1.0, 4.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0,
      0.0, 3.0, 1.0, 4.0, 0.0, 0.0, 0.0, 0.0, 0.0,
      0.0, 0.0, 3.0, 1.0, 4.0, 0.0, 0.0, 0.0, 0.0,
      0.0, 0.0, 0.0, 3.0, 1.0, 4.0, 0.0, 0.0, 0.0,
      0.0, 0.0, 0.0, 0.0, 3.0, 1.0, 4.0, 0.0, 0.0,
      0.0, 0.0, 0.0, 0.0, 0.0, 3.0, 1.0, 4.0, 0.0,
      0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 3.0, 1.0, 4.0,
      0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 3.0, 1.0
    ]

  x :: Matrix R
  x = triDiagSolve dL d dU b

  z :: Matrix C
  z = (4><4)
    [
      1.0 :+ 1.0, 4.0 :+ 4.0, 0.0 :+ 0.0, 0.0 :+ 0.0,
      3.0 :+ 3.0, 1.0 :+ 1.0, 4.0 :+ 4.0, 0.0 :+ 0.0,
      0.0 :+ 0.0, 3.0 :+ 3.0, 1.0 :+ 1.0, 4.0 :+ 4.0,
      0.0 :+ 0.0, 0.0 :+ 0.0, 3.0 :+ 3.0, 1.0 :+ 1.0
    ]

  zDL, zD, zDu :: Vector C
  zDL = fromList [3.0 :+ 3.0, 3.0 :+ 3.0, 3.0 :+ 3.0]
  zD  = fromList [1.0 :+ 1.0, 1.0 :+ 1.0, 1.0 :+ 1.0, 1.0 :+ 1.0]
  zDu = fromList [4.0 :+ 4.0, 4.0 :+ 4.0, 4.0 :+ 4.0]

  zB :: Matrix C
  zB = (4><3)
    [
      1.0 :+ 1.0,   1.0  :+   1.0,  1.0 :+ (-1.0),
      1.0 :+ 1.0, (-1.0) :+ (-1.0), 1.0 :+ (-1.0),
      1.0 :+ 1.0,   1.0  :+   1.0,  1.0 :+ (-1.0),
      1.0 :+ 1.0, (-1.0) :+ (-1.0), 1.0 :+ (-1.0)
    ]

  u :: Matrix C
  u = triDiagSolve zDL zD zDu zB

  ok1 = (maximum $ map abs $ concat $ toLists $ b - (y <> x)) <= 1e-15
  ok2 = (maximum $ map magnitude $ concat $ toLists $ zB - (z <> u)) <= 1e-15

---------------------------------------------------------------------

randomTestGaussian = (unSym c) :~3~: unSym (snd (meanCov dat))
  where
    a = (3><3) [1,2,3,
                2,4,0,
               -2,2,1]
    m = 3 |> [1,2,3]
    c = mTm a
    dat = gaussianSample 7 (10^6) m c

randomTestUniform = c :~2~: unSym (snd (meanCov dat))
  where
    c = diag $ 3 |> map ((/12).(^2)) [1,2,3]
    dat = uniformSample 7 (10^6) [(0,1),(1,3),(3,6)]

---------------------------------------------------------------------

rot :: Double -> Matrix Double
rot a = (3><3) [ c,0,s
               , 0,1,0
               ,-s,0,c ]
    where c = cos a
          s = sin a

rotTest = fun (10^5) :~11~: rot 5E4
    where fun n = foldl1' (<>) (map rot angles)
              where angles = toList $ linspace n (0,1)

---------------------------------------------------------------------
-- vector <= 0.6.0.2 bug discovered by Patrick Perry
-- http://trac.haskell.org/vector/ticket/31

offsetTest = y == y' where
    x = fromList [0..3 :: Double]
    y = subVector 1 3 x
    (f,o,n) = unsafeToForeignPtr y
    y' = unsafeFromForeignPtr f o n

---------------------------------------------------------------------

normsVTest = TestList [
    utest "normv2CD" $ norm2PropC v
--  , utest "normv2CF" $ norm2PropC (single v)
#ifndef NONORMVTEST
  , utest "normv2D"  $ norm2PropR x
--  , utest "normv2F"  $ norm2PropR (single x)
#endif
  , utest "normv1CD" $ norm_1 v          == 8
--  , utest "normv1CF" $ norm_1 (single v) == 8
  , utest "normv1D"  $ norm_1 x          == 6
--  , utest "normv1F"  $ norm_1 (single x) == 6

  , utest "normvInfCD" $ norm_Inf v          == 5
--  , utest "normvInfCF" $ norm_Inf (single v) == 5
  , utest "normvInfD"  $ norm_Inf x          == 3
--  , utest "normvInfF"  $ norm_Inf (single x) == 3

 ] where v = fromList [1,-2,3:+4] :: Vector (Complex Double)
         x = fromList [1,2,-3] :: Vector Double
#ifndef NONORMVTEST
         norm2PropR a = norm_2 a =~= sqrt (udot a a)
#endif
         norm2PropC a = norm_2 a =~= realPart (sqrt (a `dot` a))
         a =~= b = fromList [a] |~| fromList [b]

normsMTest = TestList [
    utest "norm2mCD" $ norm_2 v          =~= 8.86164970498005
--  , utest "norm2mCF" $ norm_2 (single v) =~= 8.86164970498005
  , utest "norm2mD"  $ norm_2 x          =~= 5.96667765076216
--  , utest "norm2mF"  $ norm_2 (single x) =~= 5.96667765076216

  , utest "norm1mCD" $ norm_1 v          == 9
--  , utest "norm1mCF" $ norm_1 (single v) == 9
  , utest "norm1mD"  $ norm_1 x          == 7
--  , utest "norm1mF"  $ norm_1 (single x) == 7

  , utest "normmInfCD" $ norm_Inf v          == 12
--  , utest "normmInfCF" $ norm_Inf (single v) == 12
  , utest "normmInfD"  $ norm_Inf x          == 8
--  , utest "normmInfF"  $ norm_Inf (single x) == 8

  , utest "normmFroCD" $ norm_Frob v          =~= 8.88819441731559
--  , utest "normmFroCF" $ norm_Frob (single v) =~~= 8.88819441731559
  , utest "normmFroD"  $ norm_Frob x          =~= 6.24499799839840
--  , utest "normmFroF"  $ norm_Frob (single x) =~~= 6.24499799839840

 ] where v = (2><2) [1,-2*iC,3:+4,7] :: Matrix (Complex Double)
         x = (2><2) [1,2,-3,5] :: Matrix Double
         a =~= b = fromList [a] :~10~: fromList [b]
--       a =~~= b = fromList [a] :~5~: fromList [b]

---------------------------------------------------------------------

sumprodTest = TestList [
    utest "sumCD" $ sumElements z            == 6
  , utest "sumCF" $ sumElements (single z)   == 6
  , utest "sumD"  $ sumElements v            == 6
  , utest "sumF"  $ sumElements (single v)   == 6

  , utest "prodCD" $ prodProp z
  , utest "prodCF" $ prodProp (single z)
  , utest "prodD"  $ prodProp v
  , utest "prodF"  $ prodProp (single v)
 ] where v = fromList [1,2,3] :: Vector Double
         z = fromList [1,2-iC,3+iC]
         prodProp x = prodElements x == product (toList x)

---------------------------------------------------------------------

chainTest = utest "chain" $ foldl1' (<>) ms |~| optimiseMult ms where
    ms = [ diag (fromList [1,2,3 :: Double])
         , konst 3 (3,5)
         , (5><10) [1 .. ]
         , konst 5 (10,2)
         ]

---------------------------------------------------------------------

conjuTest m = cmap conjugate (flatten (conj (tr m))) == flatten (tr m)

---------------------------------------------------------------------

newtype State s a = State { runState :: s -> (a,s) }

instance Functor (State s)
  where
    fmap f x = pure f <*> x

instance Applicative (State s)
  where
    pure = return
    (<*>) = ap

instance Monad (State s) where
    return a = State $ \s -> (a,s)
    m >>= f = State $ \s -> let (a,s') = runState m s
                            in runState (f a) s'

state_get :: State s s
state_get = State $ \s -> (s,s)

state_put :: s -> State s ()
state_put s = State $ \_ -> ((),s)

evalState :: State s a -> s -> a
evalState m s = let (a,s') = runState m s
                in seq s' a

newtype MaybeT m a = MaybeT { runMaybeT :: m (Maybe a) }

instance Monad m => Functor (MaybeT m)
  where
    fmap f x = pure f <*> x

instance Monad m => Applicative (MaybeT m)
  where
    pure = return
    (<*>) = ap

instance Monad m => Monad (MaybeT m) where
    return a = MaybeT $ return $ Just a
    m >>= f  = MaybeT $ do
                        res <- runMaybeT m
                        case res of
                                 Nothing -> return Nothing
                                 Just r  -> runMaybeT (f r)
    fail _   = MaybeT $ return Nothing

lift_maybe m = MaybeT $ do
                        res <- m
                        return $ Just res

-- apply a test to successive elements of a vector, evaluates to true iff test passes for all pairs
--successive_ :: Storable a => (a -> a -> Bool) -> Vector a -> Bool
successive_ t v = maybe False (\_ -> True) $ evalState (runMaybeT (mapVectorM_ stp (subVector 1 (size v - 1) v))) (v ! 0)
   where stp e  = do
                  ep <- lift_maybe $ state_get
                  if t e ep
                     then lift_maybe $ state_put e
                     else (fail "successive_ test failed")

-- operate on successive elements of a vector and return the resulting vector, whose length 1 less than that of the input
--successive :: (Storable a, Storable b) => (a -> a -> b) -> Vector a -> Vector b
successive f v = evalState (mapVectorM stp (subVector 1 (size v - 1) v)) (v ! 0)
   where stp  e = do
                  ep <- state_get
                  state_put e
                  return $ f ep e


succTest = utest "successive" $
       successive_ (>) (fromList [1 :: Double,2,3,4]) == True
    && successive_ (>) (fromList [1 :: Double,3,2,4]) == False
    && successive (+) (fromList [1..10 :: Double]) == 9 |> [3,5,7,9,11,13,15,17,19]

---------------------------------------------------------------------

findAssocTest = utest "findAssoc" ok
  where
    ok = m1 == m2
    m1 = assoc (6,6) 7 $ zip (find (>0) (ident 5 :: Matrix Float)) [10 ..] :: Matrix Double
    m2 = diagRect 7 (fromList[10..14]) 6 6

---------------------------------------------------------------------

condTest = utest "cond" ok
  where
    ok = step v * v == cond v 0 0 0 v
    v = fromList [-7 .. 7 ] :: Vector Float

---------------------------------------------------------------------

conformTest = utest "conform" ok
  where
    ok = 1 + row [1,2,3] + col [10,20,30,40] + (4><3) [1..]
         == (4><3) [13,15,17
                   ,26,28,30
                   ,39,41,43
                   ,52,54,56]

---------------------------------------------------------------------

accumTest = utest "accum" ok
  where
    x = ident 3 :: Matrix Double
    ok = accum x (+) [((1,2),7), ((2,2),3)]
         == (3><3) [1,0,0
                   ,0,1,7
                   ,0,0,4]
         &&
         toList (flatten x) == [1,0,0,0,1,0,0,0,1]

--------------------------------------------------------------------------------

convolutionTest = utest "convolution" ok
  where
--    a = fromList [1..10]               :: Vector Double
    b = fromList [1..3]                :: Vector Double
    c = (5><7) [1..]                   :: Matrix Double
--    d = (3><3) [0,-1,0,-1,4,-1,0,-1,0] :: Matrix Double
    ok =  separable (corr b) c == corr2 (outer b b) c
       && separable (conv b) c == conv2 (outer b b) c

--------------------------------------------------------------------------------

sparseTest = utest "sparse" (fst $ checkT (undefined :: GMatrix))

--------------------------------------------------------------------------------

staticTest = utest "static" (fst $ checkT (undefined :: L 3 5))

--------------------------------------------------------------------------------

intTest = utest "int ops" (fst $ checkT (undefined :: Matrix I))

--------------------------------------------------------------------------------

modularTest = utest "modular ops" (fst $ checkT (undefined :: Matrix (Mod 13 I)))

--------------------------------------------------------------------------------

indexProp g f x = a1 == g a2 && a2 == a3 && b1 == g b2 && b2 == b3
  where
    l = map g (toList (f x))
    a1 = maximum l
    b1 = minimum l
    a2 = x `atIndex` maxIndex x
    b2 = x `atIndex` minIndex x
    a3 = maxElement x
    b3 = minElement x

--------------------------------------------------------------------------------

sliceTest = utest "slice test" $ and
    [ testSlice (chol . trustSym)  (gen 5 :: Matrix R)
    , testSlice (chol . trustSym)  (gen 5 :: Matrix C)
    , testSlice qr    (rec :: Matrix R)
    , testSlice qr    (rec :: Matrix C)
    , testSlice hess  (agen 5 :: Matrix R)
    , testSlice hess  (agen 5 :: Matrix C)
    , testSlice schur (agen 5 :: Matrix R)
    , testSlice schur (agen 5 :: Matrix C)
    , testSlice lu    (agen 5 :: Matrix R)
    , testSlice lu    (agen 5 :: Matrix C)
    , testSlice (luSolve (luPacked (agen 5 :: Matrix R))) (agen 5)
    , testSlice (luSolve (luPacked (agen 5 :: Matrix C))) (agen 5)
    , test_lus (agen 5 :: Matrix R)
    , test_lus (agen 5 :: Matrix C)

    , testSlice eig   (agen 5 :: Matrix R)
    , testSlice eig   (agen 5 :: Matrix C)
    , testSlice (eigSH . trustSym) (gen 5 :: Matrix R)
    , testSlice (eigSH . trustSym) (gen 5 :: Matrix C)
    , testSlice eigenvalues   (agen 5 :: Matrix R)
    , testSlice eigenvalues   (agen 5 :: Matrix C)
    , testSlice (eigenvaluesSH . trustSym) (gen 5 :: Matrix R)
    , testSlice (eigenvaluesSH . trustSym) (gen 5 :: Matrix C)

    , testSlice svd           (rec :: Matrix R)
    , testSlice thinSVD       (rec :: Matrix R)
    , testSlice compactSVD     (rec :: Matrix R)
    , testSlice leftSV        (rec :: Matrix R)
    , testSlice rightSV       (rec :: Matrix R)
    , testSlice singularValues (rec :: Matrix R)

    , testSlice svd           (rec :: Matrix C)
    , testSlice thinSVD       (rec :: Matrix C)
    , testSlice compactSVD     (rec :: Matrix C)
    , testSlice leftSV        (rec :: Matrix C)
    , testSlice rightSV       (rec :: Matrix C)
    , testSlice singularValues (rec :: Matrix C)

    , testSlice (linearSolve (agen 5:: Matrix R)) (agen 5)
    , testSlice (flip linearSolve (agen 5:: Matrix R)) (agen 5)

    , testSlice (linearSolve (agen 5:: Matrix C)) (agen 5)
    , testSlice (flip linearSolve (agen 5:: Matrix C)) (agen 5)

    , testSlice (linearSolveLS (ogen 5:: Matrix R)) (ogen 5)
    , testSlice (flip linearSolveLS (ogen 5:: Matrix R)) (ogen 5)

    , testSlice (linearSolveLS (ogen 5:: Matrix C)) (ogen 5)
    , testSlice (flip linearSolveLS (ogen 5:: Matrix C)) (ogen 5)

    , testSlice (linearSolveSVD (ogen 5:: Matrix R)) (ogen 5)
    , testSlice (flip linearSolveSVD (ogen 5:: Matrix R)) (ogen 5)

    , testSlice (linearSolveSVD (ogen 5:: Matrix C)) (ogen 5)
    , testSlice (flip linearSolveSVD (ogen 5:: Matrix C)) (ogen 5)

    , testSlice (linearSolveLS (ugen 5:: Matrix R)) (ugen 5)
    , testSlice (flip linearSolveLS (ugen 5:: Matrix R)) (ugen 5)

    , testSlice (linearSolveLS (ugen 5:: Matrix C)) (ugen 5)
    , testSlice (flip linearSolveLS (ugen 5:: Matrix C)) (ugen 5)

    , testSlice (linearSolveSVD (ugen 5:: Matrix R)) (ugen 5)
    , testSlice (flip linearSolveSVD (ugen 5:: Matrix R)) (ugen 5)

    , testSlice (linearSolveSVD (ugen 5:: Matrix C)) (ugen 5)
    , testSlice (flip linearSolveSVD (ugen 5:: Matrix C)) (ugen 5)

    , testSlice ((<>) (ogen 5:: Matrix R)) (gen 5)
    , testSlice (flip (<>) (gen 5:: Matrix R)) (ogen 5)
    , testSlice ((<>) (ogen 5:: Matrix C)) (gen 5)
    , testSlice (flip (<>) (gen 5:: Matrix C)) (ogen 5)
    , testSlice ((<>) (ogen 5:: Matrix Float)) (gen 5)
    , testSlice (flip (<>) (gen 5:: Matrix Float)) (ogen 5)
    , testSlice ((<>) (ogen 5:: Matrix (Complex Float))) (gen 5)
    , testSlice (flip (<>) (gen 5:: Matrix (Complex Float))) (ogen 5)
    , testSlice ((<>) (ogen 5:: Matrix I)) (gen 5)
    , testSlice (flip (<>) (gen 5:: Matrix I)) (ogen 5)
    , testSlice ((<>) (ogen 5:: Matrix Z)) (gen 5)
    , testSlice (flip (<>) (gen 5:: Matrix Z)) (ogen 5)

    , testSlice ((<>) (ogen 5:: Matrix (I ./. 7))) (gen 5)
    , testSlice (flip (<>) (gen 5:: Matrix (I ./. 7))) (ogen 5)
    , testSlice ((<>) (ogen 5:: Matrix (Z ./. 7))) (gen 5)
    , testSlice (flip (<>) (gen 5:: Matrix (Z ./. 7))) (ogen 5)

    , testSlice (flip cholSolve (agen 5:: Matrix R)) (chol $ trustSym $ gen 5)
    , testSlice (flip cholSolve (agen 5:: Matrix C)) (chol $ trustSym $ gen 5)
    , testSlice (cholSolve (chol $ trustSym $ gen 5:: Matrix R)) (agen 5)
    , testSlice (cholSolve (chol $ trustSym $ gen 5:: Matrix C)) (agen 5)

    , ok_qrgr        (rec :: Matrix R)
    , ok_qrgr        (rec :: Matrix C)
    , testSlice (test_qrgr 4 tau1) qrr1
    , testSlice (test_qrgr 4 tau2) qrr2
    ]
  where
    QR qrr1 tau1 = qrRaw (rec :: Matrix R)
    QR qrr2 tau2 = qrRaw (rec :: Matrix C)

    test_qrgr n t x = qrgr n (QR x t)

    ok_qrgr x = simeq 1E-15 q q'
      where
        (q,_) = qr x
        atau = qrRaw x
        q' = qrgr (rows q) atau

    simeq eps a b =  not $ magnit eps (norm_1 $ flatten (a-b))

    test_lus m = testSlice f lup
      where
        f x = luSolve (LU x p) m
        (LU lup p) = luPacked m

    gen :: Numeric t => Int -> Matrix t
    gen n = diagRect 1 (konst 5 n) n n

    agen :: (Numeric t, Num (Vector t))=> Int -> Matrix t
    agen n = gen n + fromInt ((n><n)[0..])

    ogen :: (Numeric t, Num (Vector t))=> Int -> Matrix t
    ogen n = gen n === gen n

    ugen :: (Numeric t, Num (Vector t))=> Int -> Matrix t
    ugen n = takeRows 3 (gen n)


    rec :: Numeric t => Matrix t
    rec = subMatrix (0,0) (4,5) (gen 5)

    testSlice f x@(size->sz@(r,c)) = all (==f x) (map f (g y1 ++ g y2))
      where
        subm = subMatrix
        g y = [ subm (a*r,b*c) sz y | a <-[0..2], b <- [0..2]]
        h z = fromBlocks (replicate 3 (replicate 3 z))
        y1  = h x
        y2  = (tr . h . tr) x



--------------------------------------------------------------------------------

-- | All tests must pass with a maximum dimension of about 20
--  (some tests may fail with bigger sizes due to precision loss).
runTests :: Int  -- ^ maximum dimension
         -> IO ()
runTests n = do
    let test :: forall t . T.Testable t => t -> IO ()
        test p = qCheck n p
    putStrLn "------ index"
    test( \m -> indexProp id flatten (single (m :: RM)) )
    test( \v -> indexProp id id (single (v :: Vector Double)) )
    test( \m -> indexProp id flatten (m :: RM) )
    test( \v -> indexProp id id (v :: Vector Double) )
    test( \m -> indexProp magnitude flatten (single (m :: CM)) )
    test( \v -> indexProp magnitude id (single (v :: Vector (Complex Double))) )
    test( \m -> indexProp magnitude flatten (m :: CM) )
    test( \v -> indexProp magnitude id (v :: Vector (Complex Double)) )
    putStrLn "------ mult Double"
    test (multProp1 10 . rConsist)
    test (multProp1 10 . cConsist)
    test (multProp2 10 . rConsist)
    test (multProp2 10 . cConsist)
--    putStrLn "------ mult Float"
--    test (multProp1  6 . (single *** single) . rConsist)
--    test (multProp1  6 . (single *** single) . cConsist)
--    test (multProp2  6 . (single *** single) . rConsist)
--    test (multProp2  6 . (single *** single) . cConsist)
    putStrLn "------ sub-trans"
    test (subProp . rM)
    test (subProp . cM)
    putStrLn "------ ctrans"
    test (conjuTest . cM)
    test (conjuTest . zM)
    putStrLn "------ lu"
    test (luProp    . rM)
    test (luProp    . cM)
    putStrLn "------ inv (linearSolve)"
    test (invProp   . rSqWC)
    test (invProp   . cSqWC)
    putStrLn "------ luSolve"
    test (linearSolveProp (luSolve.luPacked) . rSqWC)
    test (linearSolveProp (luSolve.luPacked) . cSqWC)
    putStrLn "------ ldlSolve"
    test (linearSolvePropH (ldlSolve.ldlPacked) . rSymWC)
    test (linearSolvePropH (ldlSolve.ldlPacked) . cSymWC)
    putStrLn "------ cholSolve"
    test (linearSolveProp (cholSolve.chol.trustSym) . rPosDef)
    test (linearSolveProp (cholSolve.chol.trustSym) . cPosDef)
    putStrLn "------ luSolveLS"
    test (linearSolveProp linearSolveLS . rSqWC)
    test (linearSolveProp linearSolveLS . cSqWC)
    test (linearSolveProp2 linearSolveLS . rConsist)
    test (linearSolveProp2 linearSolveLS . cConsist)
    putStrLn "------ pinv (linearSolveSVD)"
    test (pinvProp  . rM)
    test (pinvProp  . cM)
    putStrLn "------ det"
    test (detProp   . rSqWC)
    test (detProp   . cSqWC)
    putStrLn "------ svd"
    test (svdProp1  . rM)
    test (svdProp1  . cM)
    test (svdProp1a svd . rM)
    test (svdProp1a svd . cM)
--    test (svdProp1a svdRd)
    test (svdProp1b svd . rM)
    test (svdProp1b svd . cM)
--    test (svdProp1b svdRd)
    test (svdProp2 thinSVD . rM)
    test (svdProp2 thinSVD . cM)
--    test (svdProp2 thinSVDRd)
--    test (svdProp2 thinSVDCd)
    test (svdProp3  . rM)
    test (svdProp3  . cM)
    test (svdProp4  . rM)
    test (svdProp4  . cM)
    test (svdProp5a)
    test (svdProp5b)
    test (svdProp6a)
    test (svdProp6b)
    test (svdProp7  . rM)
    test (svdProp7  . cM)
--    putStrLn "------ svdCd"
#ifdef NOZGESDD
--    putStrLn "Omitted"
#else
--    test (svdProp1a svdCd)
--    test (svdProp1b svdCd)
#endif
    putStrLn "------ eig"
    test (eigSHProp . rHer)
    test (eigSHProp . cHer)
    test (eigProp   . rSq)
    test (eigProp   . cSq)
    test (eigSHProp2 . rHer)
    test (eigSHProp2 . cHer)
    test (eigProp2   . rSq)
    test (eigProp2   . cSq)
    putStrLn "------ nullSpace"
    test (nullspaceProp . rM)
    test (nullspaceProp . cM)
    putStrLn "------ qr"
    test (qrProp     . rM)
    test (qrProp     . cM)
    test (rqProp     . rM)
--    test (rqProp     . cM)
    test (rqProp1     . cM)
    test (rqProp2     . cM)
--    test (rqProp3     . cM)
    putStrLn "------ hess"
    test (hessProp   . rSq)
    test (hessProp   . cSq)
    putStrLn "------ schur"
    test (schurProp2 . rSq)
    test (schurProp1 . cSq)
    putStrLn "------ chol"
    test (cholProp   . rPosDef)
    test (cholProp   . cPosDef)
--    test (exactProp  . rPosDef)
--    test (exactProp  . cPosDef)
    putStrLn "------ expm"
    test (expmDiagProp . complex. rSqWC)
    test (expmDiagProp . cSqWC)
    putStrLn "------ vector operations - Double"
    test (\u -> sin u ^ 2 + cos u ^ 2 |~| (1::RM))
    test $ (\u -> sin u ^ 2 + cos u ^ 2 |~| (1::CM)) . liftMatrix makeUnitary
    test (\u -> sin u ** 2 + cos u ** 2 |~| (1::RM))
    test (\u -> cos u * tan u |~| sin (u::RM))
    test $ (\u -> cos u * tan u |~| sin (u::CM)) . liftMatrix makeUnitary
--    putStrLn "------ vector operations - Float"
--    test (\u -> sin u ^ 2 + cos u ^ 2 |~~| (1::FM))
--    test $ (\u -> sin u ^ 2 + cos u ^ 2 |~~| (1::ZM)) . liftMatrix makeUnitary
--    test (\u -> sin u ** 2 + cos u ** 2 |~~| (1::FM))
--    test (\u -> cos u * tan u |~~| sin (u::FM))
--    test $ (\u -> cos u * tan u |~~| sin (u::ZM)) . liftMatrix makeUnitary
    putStrLn "------ read . show"
    test (\m -> (m::RM) == read (show m))
    test (\m -> (m::CM) == read (show m))
    test (\m -> toRows (m::RM) == read (show (toRows m)))
    test (\m -> toRows (m::CM) == read (show (toRows m)))
    test (\m -> (m::FM) == read (show m))
    test (\m -> (m::ZM) == read (show m))
    test (\m -> toRows (m::FM) == read (show (toRows m)))
    test (\m -> toRows (m::ZM) == read (show (toRows m)))
    putStrLn "------ some unit tests"
    c <- runTestTT $ TestList
        [ utest "1E5 rots" rotTest
        , utest "det1" detTest1
        , utest "invlndet" detTest2
        , utest "expm1" (expmTest1)
        , utest "expm2" (expmTest2)
        , utest "arith1" $ ((ones (100,100) * 5 + 2)/0.5 - 7)**2 |~| (49 :: RM)
        , utest "arith2" $ ((scalar (1+iC) * ones (100,100) * 5 + 2)/0.5 - 7)**2 |~| ( scalar (140*iC-51) :: CM)
        , utest "arith3" $ exp (scalar iC * ones(10,10)*pi) + 1 |~| 0
        , utest "<\\>"   $ (3><2) [2,0,0,3,1,1::Double] <\> 3|>[4,9,5] |~| 2|>[2,3]
--        , utest "gamma" (gamma 5 == 24.0)
--        , besselTest
--        , exponentialTest
        , utest "randomGaussian" randomTestGaussian
        , utest "randomUniform" randomTestUniform
        , utest "buildVector/Matrix" $
                        complex (10 |> [0::Double ..]) == build 10 id
                     && ident 5 == build (5,5) (\r c -> if r==c then 1::Double else 0)
        , utest "rank" $  rank ((2><3)[1,0,0,1,5*peps,0::Double]) == 1
                       && rank ((2><3)[1,0,0,1,7*peps,0::Double]) == 2
        , utest "block" $ fromBlocks [[ident 3,0],[0,ident 4]] == (ident 7 :: CM)
        , mbCholTest
        , triTest
        , triDiagTest
        , utest "offset" offsetTest
        , normsVTest
        , normsMTest
        , sumprodTest
        , chainTest
        , succTest
        , findAssocTest
        , condTest
        , conformTest
        , accumTest
        , convolutionTest
        , sparseTest
        , staticTest
        , intTest
        , modularTest
        , sliceTest
        ]
    when (errors c + failures c > 0) exitFailure
    return ()


-- single precision approximate equality
-- infixl 4 |~~|
-- a |~~| b = a :~6~: b

makeUnitary v | realPart n > 1    = v / scalar n
              | otherwise = v
    where n = sqrt (v `dot` v)

binaryTests :: IO ()
binaryTests = do
  let test :: forall t . T.Testable t => t -> IO ()
      test = qCheck 100
  test vectorBinaryRoundtripProp
  test staticVectorBinaryRoundtripProp
  qCheck 30 matrixBinaryRoundtripProp
  qCheck 30 staticMatrixBinaryRoundtripProp

-- -- | Some additional tests on big matrices. They take a few minutes.
-- runBigTests :: IO ()
-- runBigTests = undefined

{-
-- | testcase for nonempty fpu stack
findNaN :: Int -> Bool
findNaN n = all (bugProp . eye) (take n $ cycle [1..20])
  where eye m = ident m :: Matrix ( Double)
-}

--------------------------------------------------------------------------------

-- | Performance measurements.
runBenchmarks :: IO ()
runBenchmarks = do
    solveBench
    subBench
    mkVecBench
    multBench
    cholBench
    luBench
    luBench_2
    svdBench
    eigBench
    putStrLn ""

--------------------------------

time msg act = do
    putStr (msg++" ")
    t0 <- getCPUTime
    act `seq` putStr " "
    t1 <- getCPUTime
    printf "%6.2f s CPU\n" $ (fromIntegral (t1 - t0) / (10^12 :: Double)) :: IO ()
    return ()

timeR msg act = do
    putStr (msg++" ")
    t0 <- getCPUTime
    putStr (show act)
    t1 <- getCPUTime
    printf "%6.2f s CPU\n" $ (fromIntegral (t1 - t0) / (10^12 :: Double)) :: IO ()
    return ()

--------------------------------

manymult n = foldl1' (<>) (map rot2 angles) where
    angles = toList $ linspace n (0,1)
    rot2 :: Double -> Matrix Double
    rot2 a = (3><3) [ c,0,s
                    , 0,1,0
                    ,-s,0,c ]
        where c = cos a
              s = sin a

multb n = foldl1' (<>) (replicate (10^6) (ident n :: Matrix Double))

--------------------------------

manyvec0 xs = sum $ map (\x -> x + x**2 + x**3) xs
manyvec1 xs = sumElements $ fromRows $ map (\x -> fromList [x,x**2,x**3]) xs
manyvec5 xs = sumElements $ fromRows $ map (\x -> vec3 x (x**2) (x**3)) xs


manyvec2 xs = sum $ map (\x -> sqrt(x^2 + (x**2)^2 +(x**3)^2)) xs
manyvec3 xs = sum $ map (norm_2 . (\x -> fromList [x,x**2,x**3])) xs

manyvec4 xs = sum $ map (norm_2 . (\x -> vec3 x (x**2) (x**3))) xs

vec3 :: Double -> Double -> Double -> Vector Double
vec3 a b c = runSTVector $ do
    v <- newUndefinedVector 3
    writeVector v 0 a
    writeVector v 1 b
    writeVector v 2 c
    return v

mkVecBench = do
    let n = 1000000
        xs = toList $ linspace n (0,1::Double)
    putStr "\neval data... "; print (sum xs)
    timeR "listproc        " $ manyvec0 xs
    timeR "fromList matrix " $ manyvec1 xs
    timeR "vec3 matrix     " $ manyvec5 xs
    timeR "listproc norm   " $ manyvec2 xs
    timeR "norm fromList   " $ manyvec3 xs
    timeR "norm vec3       " $ manyvec4 xs

--------------------------------

subBench = do
    putStrLn ""
    let g = foldl1' (.) (replicate (10^5) (\v -> subVector 1 (size v -1) v))
    time "0.1M subVector   " (g (konst 1 (1+10^5) :: Vector Double) ! 0)
    let f = foldl1' (.) (replicate (10^5) (fromRows.toRows))
    time "subVector-join  3" (f (ident  3 :: Matrix Double) `atIndex` (0,0))
    time "subVector-join 10" (f (ident 10 :: Matrix Double) `atIndex` (0,0))

--------------------------------

multBench = do
    let a = ident 1000 :: Matrix Double
    let b = ident 2000 :: Matrix Double
    a `seq` b `seq` putStrLn ""
    time "product of 1M different 3x3 matrices" (manymult (10^6))
    putStrLn ""
    time "product of 1M constant  1x1 matrices" (multb 1)
    time "product of 1M constant  3x3 matrices" (multb 3)
    --time "product of 1M constant  5x5 matrices" (multb 5)
    time "product of 1M const.  10x10 matrices" (multb 10)
    --time "product of 1M const.  15x15 matrices" (multb 15)
    time "product of 1M const.  20x20 matrices" (multb 20)
    --time "product of 1M const.  25x25 matrices" (multb 25)
    putStrLn ""
    time "product (1000 x 1000)<>(1000 x 1000)" (a<>a)
    time "product (2000 x 2000)<>(2000 x 2000)" (b<>b)

--------------------------------

eigBench = do
    let m = reshape 1000 (randomVector 777 Uniform (1000*1000))
        s = m + tr m
    m `seq` s `seq` putStrLn ""
    time "eigenvalues  symmetric 1000x1000" (eigenvaluesSH (trustSym m))
    time "eigenvectors symmetric 1000x1000" (snd $ eigSH (trustSym m))
    time "eigenvalues  general   1000x1000" (eigenvalues m)
    time "eigenvectors general   1000x1000" (snd $ eig m)

--------------------------------

svdBench = do
    let a = reshape 500  (randomVector 777 Uniform (3000*500))
        b = reshape 1000 (randomVector 777 Uniform (1000*1000))
        fv (_,_,v) = v `atIndex` (0,0)
    a `seq` b `seq` putStrLn ""
    time "singular values  3000x500" (singularValues a)
    time "thin svd         3000x500" (fv $ thinSVD a)
    time "full svd         3000x500" (fv $ svd a)
    time "singular values 1000x1000" (singularValues b)
    time "full svd        1000x1000" (fv $ svd b)

--------------------------------

solveBenchN n = do
    let x = uniformSample 777 (2*n) (replicate n (-1,1))
        a = tr x <> x
        b = asColumn $ randomVector 666 Uniform n
    a `seq` b `seq` putStrLn ""
    time ("svd solve " ++ show n) (linearSolveSVD a b)
    time (" ls solve " ++ show n) (linearSolveLS a b)
    time ("    solve " ++ show n) (linearSolve a b)
--    time (" LU solve " ++ show n) (luSolve (luPacked a) b)
    time ("LDL solve " ++ show n) (ldlSolve (ldlPacked (trustSym a)) b)
    time ("cholSolve " ++ show n) (cholSolve (chol $ trustSym a) b)

solveBench = do
    solveBenchN 500
    solveBenchN 1000
    solveBenchN 1500

--------------------------------

cholBenchN n = do
    let x = uniformSample 777 (2*n) (replicate n (-1,1))
        a = tr x <> x
    a `seq` putStr ""
    time ("chol " ++ show n) (chol $ trustSym a)

cholBench = do
    putStrLn ""
    cholBenchN 1200
    cholBenchN 600
    cholBenchN 300
--    cholBenchN 150
--    cholBenchN 50

--------------------------------------------------------------------------------

luBenchN f n x msg = do
    let m = diagRect 1 (fromList (replicate n x)) n n
    m `seq` putStr ""
    time (msg ++ " "++ show n) (rnf $ f m)

luBench = do
    putStrLn ""
    luBenchN luPacked  1000 (5::R)          "luPacked  Double    "
    luBenchN luPacked' 1000 (5::R)          "luPacked' Double    "
    luBenchN luPacked' 1000 (5::Mod 9973 I) "luPacked' I mod 9973"
    luBenchN luPacked' 1000 (5::Mod 9973 Z) "luPacked' Z mod 9973"

luBenchN_2 f g n x msg = do
    let m = diagRect 1 (fromList (replicate n x)) n n
        b = flipud m
    m `seq` b `seq` putStr ""
    time (msg ++ " "++ show n) (f (g m) b)

luBench_2 = do
    putStrLn ""
    luBenchN_2 luSolve  luPacked  500 (5::R)          "luSolve .luPacked  Double    "
    luBenchN_2 luSolve' luPacked' 500 (5::R)          "luSolve'.luPacked' Double    "
    luBenchN_2 luSolve' luPacked' 500 (5::Mod 9973 I) "luSolve'.luPacked' I mod 9973"
    luBenchN_2 luSolve' luPacked' 500 (5::Mod 9973 Z) "luSolve'.luPacked' Z mod 9973"
