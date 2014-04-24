{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-unused-imports -fno-warn-incomplete-patterns #-}
-----------------------------------------------------------------------------
{- |
Module      :  Numeric.LinearAlgebra.Tests
Copyright   :  (c) Alberto Ruiz 2007-11
License     :  GPL-style

Maintainer  :  Alberto Ruiz (aruiz at um dot es)
Stability   :  provisional
Portability :  portable

Some tests.

-}

module Numeric.LinearAlgebra.Tests(
--  module Numeric.LinearAlgebra.Tests.Instances,
--  module Numeric.LinearAlgebra.Tests.Properties,
--  qCheck, 
   runTests,
   runBenchmarks
-- , findNaN
--, runBigTests
) where

--import Data.Packed.Random
import Numeric.LinearAlgebra
import Numeric.LinearAlgebra.LAPACK
import Numeric.LinearAlgebra.Tests.Instances
import Numeric.LinearAlgebra.Tests.Properties
import Test.HUnit hiding ((~:),test,Testable,State)
import System.Info
import Data.List(foldl1')
import Numeric.GSL
import Prelude hiding ((^))
import qualified Prelude
import System.CPUTime
import System.Exit
import Text.Printf
import Data.Packed.Development(unsafeFromForeignPtr,unsafeToForeignPtr)
import Control.Arrow((***))
import Debug.Trace
import Control.Monad(when)
import Numeric.LinearAlgebra.Util hiding (ones,row,col)
import Control.Applicative
import Control.Monad(ap)

import Data.Packed.ST

import Test.QuickCheck(Arbitrary,arbitrary,coarbitrary,choose,vector
                      ,sized,classify,Testable,Property
                      ,quickCheckWithResult,maxSize,stdArgs,shrink)

import Test.QuickCheck.Test(isSuccess)

qCheck n x = do
    r <- quickCheckWithResult stdArgs {maxSize = n} x
    when (not $ isSuccess r) (exitFailure)

a ^ b = a Prelude.^ (b :: Int)

utest str b = TestCase $ assertBool str b

a ~~ b = fromList a |~| fromList b

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
            , 2, 8, i
            ]

detTest2 = inv1 |~| inv2 && [det1] ~~ [det2]
  where
    m = complex (feye 6)
    inv1 = inv m
    det1 = det m
    (inv2,(lda,sa)) = invlndet m
    det2 = sa * exp lda

--------------------------------------------------------------------

polyEval cs x = foldr (\c ac->ac*x+c) 0 cs

polySolveProp p = length p <2 || last p == 0|| 1E-8 > maximum (map magnitude $ map (polyEval (map (:+0) p)) (polySolve p))

---------------------------------------------------------------------

quad f a b = fst $ integrateQAGS 1E-9 100 f a b

-- A multiple integral can be easily defined using partial application
quad2 f a b g1 g2 = quad h a b
    where h x = quad (f x) (g1 x) (g2 x)

volSphere r = 8 * quad2 (\x y -> sqrt (r*r-x*x-y*y)) 
                        0 r (const 0) (\x->sqrt (r*r-x*x))

---------------------------------------------------------------------

derivTest = abs (d (\x-> x * d (\y-> x+y) 1) 1 - 1) < 1E-10
    where d f x = fst $ derivCentral 0.01 f x

---------------------------------------------------------------------

-- besselTest = utest "bessel_J0_e" ( abs (r-expected) < e )
--     where (r,e) = bessel_J0_e 5.0
--           expected = -0.17759677131433830434739701

-- exponentialTest = utest "exp_e10_e" ( abs (v*10^e - expected) < 4E-2 )
--     where (v,e,_err) = exp_e10_e 30.0
--           expected = exp 30.0

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

---------------------------------------------------------------------

minimizationTest = TestList
    [ utest "minimization conjugatefr" (minim1 f df [5,7] ~~ [1,2])
    , utest "minimization nmsimplex2"  (minim2 f [5,7] `elem` [24,25])
    ]
    where f [x,y] = 10*(x-1)^2 + 20*(y-2)^2 + 30
          df [x,y] = [20*(x-1), 40*(y-2)]
          minim1 g dg ini = fst $ minimizeD ConjugateFR 1E-3 30 1E-2 1E-4 g dg ini
          minim2 g ini = rows $ snd $ minimize NMSimplex2 1E-2 30 [1,1] g ini

---------------------------------------------------------------------

rootFindingTest = TestList [ utest "root Hybrids" (fst sol1 ~~ [1,1])
                           , utest "root Newton"  (rows (snd sol2) == 2)
                           ]
    where sol1 = root Hybrids 1E-7 30 (rosenbrock 1 10) [-10,-5]
          sol2 = rootJ Newton 1E-7 30 (rosenbrock 1 10) (jacobian 1 10) [-10,-5]
          rosenbrock a b [x,y] = [ a*(1-x), b*(y-x^2) ]
          jacobian a b [x,_y] = [ [-a    , 0]
                                , [-2*b*x, b] ]

---------------------------------------------------------------------

odeTest = utest "ode" (last (toLists sol) ~~ newsol)
  where
    sol = odeSolveV RK8pd 1E-6 1E-6 0 (l2v $ vanderpol 10) (fromList [1,0]) ts
    ts = linspace 101 (0,100)
    l2v f = \t -> fromList  . f t . toList
    vanderpol mu _t [x,y] = [y, -x + mu * y * (1-x^2) ]
    newsol = [-1.758888036617841,  8.364349410519058e-2]
    -- oldsol = [-1.7588880332411019, 8.364348908711941e-2]

---------------------------------------------------------------------

fittingTest = utest "levmar" (ok1 && ok2)
    where
    xs = map return [0 .. 39]
    sigma = 0.1
    ys = map return $ toList $ fromList (map (head . expModel [5,0.1,1]) xs)
                    + scalar sigma * (randomVector 0 Gaussian 40)
    dats = zip xs (zip ys (repeat sigma))
    dat = zip xs ys

    expModel [a,lambda,b] [t] = [a * exp (-lambda * t) + b]
    expModelDer [a,lambda,_b] [t] = [[exp (-lambda * t), -t * a * exp(-lambda*t) , 1]]

    sols = fst $ fitModelScaled 1E-4 1E-4 20 (expModel, expModelDer) dats [1,0,0]
    sol = fst $ fitModel 1E-4 1E-4 20 (expModel, expModelDer) dat [1,0,0]

    ok1 = and (zipWith f sols [5,0.1,1]) where f (x,d) r = abs (x-r)<2*d
    ok2 = norm2 (fromList (map fst sols) - fromList sol) < 1E-5

-----------------------------------------------------

mbCholTest = utest "mbCholTest" (ok1 && ok2) where
    m1 = (2><2) [2,5,5,8 :: Double]
    m2 = (2><2) [3,5,5,9 :: Complex Double]
    ok1 = mbCholSH m1 == Nothing
    ok2 = mbCholSH m2 == Just (chol m2)

---------------------------------------------------------------------

randomTestGaussian = c :~1~: snd (meanCov dat) where
    a = (3><3) [1,2,3,
                2,4,0,
               -2,2,1]
    m = 3 |> [1,2,3]
    c = a <> trans a
    dat = gaussianSample 7 (10^6) m c

randomTestUniform = c :~1~: snd (meanCov dat) where
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
  , utest "normv2CF" $ norm2PropC (single v)
#ifndef NONORMVTEST
  , utest "normv2D"  $ norm2PropR x
  , utest "normv2F"  $ norm2PropR (single x)
#endif
  , utest "normv1CD" $ norm1 v          == 8
  , utest "normv1CF" $ norm1 (single v) == 8
  , utest "normv1D"  $ norm1 x          == 6
  , utest "normv1F"  $ norm1 (single x) == 6

  , utest "normvInfCD" $ normInf v          == 5
  , utest "normvInfCF" $ normInf (single v) == 5
  , utest "normvInfD"  $ normInf x          == 3
  , utest "normvInfF"  $ normInf (single x) == 3

 ] where v = fromList [1,-2,3:+4] :: Vector (Complex Double)
         x = fromList [1,2,-3] :: Vector Double
#ifndef NONORMVTEST
         norm2PropR a = norm2 a =~= sqrt (udot a a)
#endif
         norm2PropC a = norm2 a =~= realPart (sqrt (cdot a a))
         a =~= b = fromList [a] |~| fromList [b]

normsMTest = TestList [
    utest "norm2mCD" $ pnorm PNorm2 v          =~= 8.86164970498005
  , utest "norm2mCF" $ pnorm PNorm2 (single v) =~= 8.86164970498005
  , utest "norm2mD"  $ pnorm PNorm2 x          =~= 5.96667765076216
  , utest "norm2mF"  $ pnorm PNorm2 (single x) =~= 5.96667765076216

  , utest "norm1mCD" $ pnorm PNorm1 v          == 9
  , utest "norm1mCF" $ pnorm PNorm1 (single v) == 9
  , utest "norm1mD"  $ pnorm PNorm1 x          == 7
  , utest "norm1mF"  $ pnorm PNorm1 (single x) == 7

  , utest "normmInfCD" $ pnorm Infinity v          == 12
  , utest "normmInfCF" $ pnorm Infinity (single v) == 12
  , utest "normmInfD"  $ pnorm Infinity x          == 8
  , utest "normmInfF"  $ pnorm Infinity (single x) == 8

  , utest "normmFroCD" $ pnorm Frobenius v          =~= 8.88819441731559
  , utest "normmFroCF" $ pnorm Frobenius (single v) =~~= 8.88819441731559
  , utest "normmFroD"  $ pnorm Frobenius x          =~= 6.24499799839840
  , utest "normmFroF"  $ pnorm Frobenius (single x) =~~= 6.24499799839840

 ] where v = (2><2) [1,-2*i,3:+4,7] :: Matrix (Complex Double)
         x = (2><2) [1,2,-3,5] :: Matrix Double
         a =~= b = fromList [a] :~10~: fromList [b]
         a =~~= b = fromList [a] :~5~: fromList [b]

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
         z = fromList [1,2-i,3+i]
         prodProp x = prodElements x == product (toList x)

---------------------------------------------------------------------

chainTest = utest "chain" $ foldl1' (<>) ms |~| optimiseMult ms where
    ms = [ diag (fromList [1,2,3 :: Double])
         , konst 3 (3,5)
         , (5><10) [1 .. ]
         , konst 5 (10,2)
         ]

---------------------------------------------------------------------

conjuTest m = mapVector conjugate (flatten (trans m)) == flatten (ctrans m)

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
successive_ t v = maybe False (\_ -> True) $ evalState (runMaybeT (mapVectorM_ stp (subVector 1 (dim v - 1) v))) (v @> 0)
   where stp e  = do
                  ep <- lift_maybe $ state_get
                  if t e ep
                     then lift_maybe $ state_put e
                     else (fail "successive_ test failed")

-- operate on successive elements of a vector and return the resulting vector, whose length 1 less than that of the input
--successive :: (Storable a, Storable b) => (a -> a -> b) -> Vector a -> Vector b
successive f v = evalState (mapVectorM stp (subVector 1 (dim v - 1) v)) (v @> 0)
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
    row = asRow . fromList
    col = asColumn . fromList :: [Double] -> Matrix Double

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

kroneckerTest = utest "kronecker" ok
  where
    a,x,b :: Matrix Double
    a = (3><4) [1..]
    x = (4><2) [3,5..]
    b = (2><5) [0,5..]
    v1 = vec (a <> x <> b)
    v2 = (trans b `kronecker` a) <> vec x
    s = trans b <> b
    v3 = vec s
    v4 = dup 5 <> vech s
    ok = v1 == v2 && v3 == v4
      && vtrans 1 a == trans a
      && vtrans (rows a) a == asColumn (vec a)

--------------------------------------------------------------------------------



-- | All tests must pass with a maximum dimension of about 20
--  (some tests may fail with bigger sizes due to precision loss).
runTests :: Int  -- ^ maximum dimension
         -> IO ()
runTests n = do
    setErrorHandlerOff
    let test p = qCheck n p
    putStrLn "------ mult Double"
    test (multProp1 10 . rConsist)
    test (multProp1 10 . cConsist)
    test (multProp2 10 . rConsist)
    test (multProp2 10 . cConsist)
    putStrLn "------ mult Float"
    test (multProp1  6 . (single *** single) . rConsist)
    test (multProp1  6 . (single *** single) . cConsist)
    test (multProp2  6 . (single *** single) . rConsist)
    test (multProp2  6 . (single *** single) . cConsist)
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
    putStrLn "------ cholSolve"
    test (linearSolveProp (cholSolve.chol) . rPosDef)
    test (linearSolveProp (cholSolve.chol) . cPosDef)
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
    test (svdProp1a svdR)
    test (svdProp1a svdC)
    test (svdProp1a svdRd)
    test (svdProp1b svdR)
    test (svdProp1b svdC)
    test (svdProp1b svdRd)
    test (svdProp2 thinSVDR)
    test (svdProp2 thinSVDC)
    test (svdProp2 thinSVDRd)
    test (svdProp2 thinSVDCd)
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
    putStrLn "------ svdCd"
#ifdef NOZGESDD
    putStrLn "Omitted"
#else
    test (svdProp1a svdCd)
    test (svdProp1b svdCd)
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
    test (rqProp     . cM)
    test (rqProp1     . cM)
    test (rqProp2     . cM)
    test (rqProp3     . cM)
    putStrLn "------ hess"
    test (hessProp   . rSq)
    test (hessProp   . cSq)
    putStrLn "------ schur"
    test (schurProp2 . rSq)
    test (schurProp1 . cSq)
    putStrLn "------ chol"
    test (cholProp   . rPosDef)
    test (cholProp   . cPosDef)
    test (exactProp  . rPosDef)
    test (exactProp  . cPosDef)
    putStrLn "------ expm"
    test (expmDiagProp . complex. rSqWC)
    test (expmDiagProp . cSqWC)
    putStrLn "------ fft"
    test (\v -> ifft (fft v) |~| v)
    putStrLn "------ vector operations - Double"
    test (\u -> sin u ^ 2 + cos u ^ 2 |~| (1::RM))
    test $ (\u -> sin u ^ 2 + cos u ^ 2 |~| (1::CM)) . liftMatrix makeUnitary
    test (\u -> sin u ** 2 + cos u ** 2 |~| (1::RM))
    test (\u -> cos u * tan u |~| sin (u::RM))
    test $ (\u -> cos u * tan u |~| sin (u::CM)) . liftMatrix makeUnitary
    putStrLn "------ vector operations - Float"
    test (\u -> sin u ^ 2 + cos u ^ 2 |~~| (1::FM))
    test $ (\u -> sin u ^ 2 + cos u ^ 2 |~~| (1::ZM)) . liftMatrix makeUnitary
    test (\u -> sin u ** 2 + cos u ** 2 |~~| (1::FM))
    test (\u -> cos u * tan u |~~| sin (u::FM))
    test $ (\u -> cos u * tan u |~~| sin (u::ZM)) . liftMatrix makeUnitary
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
        , utest "arith2" $ ((scalar (1+i) * ones (100,100) * 5 + 2)/0.5 - 7)**2 |~| ( scalar (140*i-51) :: CM)
        , utest "arith3" $ exp (scalar i * ones(10,10)*pi) + 1 |~| 0
        , utest "<\\>"   $ (3><2) [2,0,0,3,1,1::Double] <\> 3|>[4,9,5] |~| 2|>[2,3]
--        , utest "gamma" (gamma 5 == 24.0)
--        , besselTest
--        , exponentialTest
        , utest "deriv" derivTest
        , utest "integrate" (abs (volSphere 2.5 - 4/3*pi*2.5^3) < 1E-8)
        , utest "polySolve" (polySolveProp [1,2,3,4])
        , minimizationTest
        , rootFindingTest
        , utest "randomGaussian" randomTestGaussian
        , utest "randomUniform" randomTestUniform
        , utest "buildVector/Matrix" $
                        complex (10 |> [0::Double ..]) == buildVector 10 fromIntegral
                     && ident 5 == buildMatrix 5 5 (\(r,c) -> if r==c then 1::Double else 0)
        , utest "rank" $  rank ((2><3)[1,0,0,1,6*eps,0]) == 1
                       && rank ((2><3)[1,0,0,1,7*eps,0]) == 2
        , utest "block" $ fromBlocks [[ident 3,0],[0,ident 4]] == (ident 7 :: CM)
        , odeTest
        , fittingTest
        , mbCholTest
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
        , kroneckerTest
        ]
    when (errors c + failures c > 0) exitFailure
    return ()


-- single precision approximate equality
infixl 4 |~~|
a |~~| b = a :~6~: b

makeUnitary v | realPart n > 1    = v / scalar n
              | otherwise = v
    where n = sqrt (v `cdot` v)

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
manyvec3 xs = sum $ map (pnorm PNorm2 . (\x -> fromList [x,x**2,x**3])) xs

manyvec4 xs = sum $ map (pnorm PNorm2 . (\x -> vec3 x (x**2) (x**3))) xs

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
    let g = foldl1' (.) (replicate (10^5) (\v -> subVector 1 (dim v -1) v))
    time "0.1M subVector   " (g (constant 1 (1+10^5) :: Vector Double) @> 0)
    let f = foldl1' (.) (replicate (10^5) (fromRows.toRows))
    time "subVector-join  3" (f (ident  3 :: Matrix Double) @@>(0,0))
    time "subVector-join 10" (f (ident 10 :: Matrix Double) @@>(0,0))

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
        s = m + trans m
    m `seq` s `seq` putStrLn ""
    time "eigenvalues  symmetric 1000x1000" (eigenvaluesSH' m)
    time "eigenvectors symmetric 1000x1000" (snd $ eigSH' m)
    time "eigenvalues  general   1000x1000" (eigenvalues m)
    time "eigenvectors general   1000x1000" (snd $ eig m)

--------------------------------

svdBench = do
    let a = reshape 500  (randomVector 777 Uniform (3000*500))
        b = reshape 1000 (randomVector 777 Uniform (1000*1000))
        fv (_,_,v) = v@@>(0,0)
    a `seq` b `seq` putStrLn ""
    time "singular values  3000x500" (singularValues a)
    time "thin svd         3000x500" (fv $ thinSVD a)
    time "full svd         3000x500" (fv $ svd a)
    time "singular values 1000x1000" (singularValues b)
    time "full svd        1000x1000" (fv $ svd b)

--------------------------------

solveBenchN n = do
    let x = uniformSample 777 (2*n) (replicate n (-1,1))
        a = trans x <> x
        b = asColumn $ randomVector 666 Uniform n
    a `seq` b `seq` putStrLn ""
    time ("svd solve " ++ show n) (linearSolveSVD a b)
    time (" ls solve " ++ show n) (linearSolveLS a b)
    time ("    solve " ++ show n) (linearSolve a b)
    time ("cholSolve " ++ show n) (cholSolve (chol a) b)

solveBench = do
    solveBenchN 500
    solveBenchN 1000
    -- solveBenchN 1500

--------------------------------

cholBenchN n = do
    let x = uniformSample 777 (2*n) (replicate n (-1,1))
        a = trans x <> x
    a `seq` putStr ""
    time ("chol " ++ show n) (chol a)

cholBench = do
    putStrLn ""
    cholBenchN 1200
    cholBenchN 600
    cholBenchN 300
--    cholBenchN 150
--    cholBenchN 50
