{-# OPTIONS_GHC -fglasgow-exts -fallow-undecidable-instances #-}

module Main where

import Data.Packed.Internal((>|<), fdat, cdat, multiply', multiplyG, MatrixOrder(..),debug)
import Numeric.GSL hiding (sin,cos,exp,choose)
import Numeric.LinearAlgebra
import Numeric.LinearAlgebra.Linear(Linear)
import Numeric.LinearAlgebra.LAPACK
import Numeric.GSL.Matrix(svdg)
import qualified Numeric.GSL.Matrix as GSL
import Test.QuickCheck hiding (test)
import Test.HUnit hiding ((~:),test)
import System.Random(randomRs,mkStdGen)
import System.Info

type RM = Matrix Double
type CM = Matrix (Complex Double)

dist :: (Normed t, Num t) => t -> t -> Double
dist a b = pnorm Infinity (a-b)

infixl 4 |~|
a |~| b = a :~8~: b

data Aprox a = (:~) a Int

(~:) :: (Normed a, Num a) => Aprox a -> a -> Bool
a :~n~: b = dist a b < 10^^(-n)


maxdim = 10

instance (Arbitrary a, RealFloat a) => Arbitrary (Complex a) where
    arbitrary = do
        r <- arbitrary
        i <- arbitrary
        return (r:+i)
    coarbitrary = undefined

instance (Field a, Arbitrary a) => Arbitrary (Matrix a) where 
   arbitrary = do --m <- sized $ \max -> choose (1,1+3*max)
                  m <- choose (1,maxdim)
                  n <- choose (1,maxdim)
                  l <- vector (m*n)
                  ctype <- arbitrary
                  let h = if ctype then (m><n) else (m>|<n)
                  trMode <- arbitrary
                  let tr = if trMode then trans else id
                  return $ tr (h l)
   coarbitrary = undefined

data PairM a = PairM (Matrix a) (Matrix a) deriving Show
instance (Num a, Field a, Arbitrary a) => Arbitrary (PairM a) where
    arbitrary = do
        a <- choose (1,maxdim)
        b <- choose (1,maxdim)
        c <- choose (1,maxdim)
        l1 <- vector (a*b)
        l2 <- vector (b*c)
        return $ PairM ((a><b) (map fromIntegral (l1::[Int]))) ((b><c) (map fromIntegral (l2::[Int])))
        --return $ PairM ((a><b) l1) ((b><c) l2)
    coarbitrary = undefined

data SqM a = SqM (Matrix a) deriving Show
sqm (SqM a) = a
instance (Field a, Arbitrary a) => Arbitrary (SqM a) where
    arbitrary = do
        n <- choose (1,maxdim)
        l <- vector (n*n)
        return $ SqM $ (n><n) l
    coarbitrary = undefined

data Sym a = Sym (Matrix a) deriving Show
sym (Sym a) = a
instance (Linear Vector a, Arbitrary a) => Arbitrary (Sym a) where
    arbitrary = do
        SqM m <- arbitrary
        return $ Sym (m + trans m)
    coarbitrary = undefined

data Her = Her (Matrix (Complex Double)) deriving Show
her (Her a) = a
instance {-(Field a, Arbitrary a, Num a) =>-} Arbitrary Her where
    arbitrary = do
        SqM m <- arbitrary
        return $ Her (m + ctrans m)
    coarbitrary = undefined

data PairSM a = PairSM (Matrix a) (Matrix a) deriving Show
instance (Num a, Field a, Arbitrary a) => Arbitrary (PairSM a) where
    arbitrary = do
        a <- choose (1,maxdim)
        c <- choose (1,maxdim)
        l1 <- vector (a*a)
        l2 <- vector (a*c)
        return $ PairSM ((a><a) (map fromIntegral (l1::[Int]))) ((a><c) (map fromIntegral (l2::[Int])))
        --return $ PairSM ((a><a) l1) ((a><c) l2)
    coarbitrary = undefined

instance (Field a, Arbitrary a) => Arbitrary (Vector a) where 
   arbitrary = do --m <- sized $ \max -> choose (1,1+3*max)
                  m <- choose (1,maxdim^2)
                  l <- vector m
                  return $ fromList l
   coarbitrary = undefined

data PairV a = PairV (Vector a) (Vector a)
instance (Field a, Arbitrary a) => Arbitrary (PairV a) where 
   arbitrary = do --m <- sized $ \max -> choose (1,1+3*max)
                  m <- choose (1,maxdim^2)
                  l1 <- vector m
                  l2 <- vector m
                  return $ PairV (fromList l1) (fromList l2)
   coarbitrary = undefined

----------------------------------------------------------------------

test str b = TestCase $ assertBool str b

----------------------------------------------------------------------

pseudorandomR seed (n,m) = reshape m $ fromList $ take (n*m) $ randomRs (-100,100) $ mkStdGen seed 

pseudorandomC seed (n,m) = toComplex (pseudorandomR seed (n,m), pseudorandomR (seed+1) (n,m))

bigmat = m + trans m :: RM
    where m = pseudorandomR 18 (1000,1000)
bigmatc = mc + ctrans mc ::CM
    where mc = pseudorandomC 19 (1000,1000)

----------------------------------------------------------------------


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


mr = (3><4)
 [ 1, 2, 3, 4,
   2, 4, 6, 8,
   1, 1, 1, 2:: Double
 ]

mrc = (3><4)
 [ 1, 2, 3, 4,
   2, 4, 6, 8,
   i, i, i, 2
 ]

a = (3><4)
 [ 1, 0, 0, 0
 , 0, 2, 0, 0
 , 0, 0, 0, 0 :: Double
 ]

b = (3><4)
 [ 1, 0, 0, 0
 , 0, 2, 3, 0
 , 0, 0, 4, 0 :: Double
 ]

ac = (2><3) [1 .. 6::Double]
bc = (3><4) [7 .. 18::Double]

af = (2>|<3) [1,4,2,5,3,6::Double]
bf = (3>|<4) [7,11,15,8,12,16,9,13,17,10,14,18::Double]

-------------------------------------------------------

detTest = det m == 26 && det mc == 38 :+ (-3)

invTest m = degenerate m || m <> inv m |~| ident (rows m)

pinvTest m =  m <> p <> m |~| m
           && p <> m <> p |~| p
           && hermitian (m<>p)
           && hermitian (p<>m)
    where p = pinv m

square m = rows m == cols m

orthonormal m = square m && m <> ctrans m |~| ident (rows m)

hermitian m = m |~| ctrans m

svdTest svd m = u <> real d <> trans v |~| m
          && orthonormal u && orthonormal v
    where (u,d,v) = full svd m

svdTest' svd m = m |~| 0 || u <> real (diag s) <> trans v |~| m
    where (u,s,v) = economy svd m

eigTest m = complex m <> v |~| v <> diag s
    where (s, v) = eig m

eigTestSH m = m <> v |~| v <> real (diag s)
              && orthonormal v
              && m |~| v <> real (diag s) <> ctrans v
    where (s, v) = eigSH m

rank m | m |~| 0   = 0
       | otherwise = dim s where (_,s,_) = economy svd m

zeros (r,c) = reshape c (constant 0 (r*c))

ones (r,c) = zeros (r,c) + 1

degenerate m = rank m < min (rows m) (cols m) 

prec = 1E-15

singular m = s1 < prec || s2/s1 < prec
    where (_,ss,_) = svd m
          s = toList ss
          s1 = maximum s
          s2 = minimum s

nullspaceTest m = null nl || m <> n |~| zeros (r,c) -- 0
    where nl = nullspacePrec 1 m
          n = fromColumns nl
          r = rows m
          c = cols m - rank m

--------------------------------------------------------------------

polyEval cs x = foldr (\c ac->ac*x+c) 0 cs

polySolveTest' p = length p <2 || last p == 0|| 1E-8 > maximum (map magnitude $ map (polyEval (map (:+0) p)) (polySolve p))


polySolveTest = test "polySolve" (polySolveTest' [1,2,3,4])

---------------------------------------------------------------------

quad f a b = fst $ integrateQAGS 1E-9 100 f a b

-- A multiple integral can be easily defined using partial application
quad2 f a b g1 g2 = quad h a b
    where h x = quad (f x) (g1 x) (g2 x)

volSphere r = 8 * quad2 (\x y -> sqrt (r*r-x*x-y*y)) 
                        0 r (const 0) (\x->sqrt (r*r-x*x))

epsTol = 1E-8::Double

integrateTest = test "integrate" (abs (volSphere 2.5 - 4/3*pi*2.5^3) < epsTol)

---------------------------------------------------------------------

besselTest = test "bessel_J0_e" ( abs (r-expected) < e )
    where (r,e) = bessel_J0_e 5.0
          expected = -0.17759677131433830434739701

exponentialTest = test "exp_e10_e" ( abs (v*10^e - expected) < 4E-2 )
    where (v,e,err) = exp_e10_e 30.0
          expected = exp 30.0

gammaTest = test "gamma" (gamma 5 == 24.0)

---------------------------------------------------------------------

cholRTest = chol ((2><2) [1,2,2,9::Double]) == (2><2) [1,2,0,2.23606797749979]
cholCTest = chol ((2><2) [1,2,2,9::Complex Double]) == (2><2) [1,2,0,2.23606797749979]

---------------------------------------------------------------------

qrTest qr m = q <> r |~| m && q <> ctrans q |~| ident (rows m)
    where (q,r) = qr m

---------------------------------------------------------------------

asFortran m = (rows m >|< cols m) $ toList (fdat m)
asC m = (rows m >< cols m) $ toList (cdat m)

mulC a b = multiply' RowMajor a b
mulF a b = multiply' ColumnMajor a b

---------------------------------------------------------------------

tests = do
    putStrLn "--------- internal -----"
    quickCheck ((\m -> m == trans m).sym :: Sym Double -> Bool)
    quickCheck ((\m -> m == trans m).sym :: Sym (Complex Double) -> Bool)
    quickCheck $ \l -> null l || (toList . fromList) l == (l :: [Double])
    quickCheck $ \l -> null l || (toList . fromList) l == (l :: [Complex Double])
    quickCheck $ \m -> m == asC (m :: RM)
    quickCheck $ \m -> m == asC (m :: CM)
    quickCheck $ \m -> m == asFortran (m :: RM)
    quickCheck $ \m -> m == asFortran (m :: CM)
    quickCheck $ \m -> m == (asC . asFortran) (m :: RM)
    quickCheck $ \m -> m == (asC . asFortran) (m :: CM)
    putStrLn "--------- multiply ----"
    quickCheck $ \(PairM m1 m2) -> mulC m1 m2 == mulF m1 (m2 :: RM)
    quickCheck $ \(PairM m1 m2) -> mulC m1 m2 == mulF m1 (m2 :: CM)
    quickCheck $ \(PairM m1 m2) -> mulC m1 m2 == trans (mulF (trans m2) (trans m1 :: RM))
    quickCheck $ \(PairM m1 m2) -> mulC m1 m2 == trans (mulF (trans m2) (trans m1 :: CM))
    quickCheck $ \(PairM m1 m2) -> mulC m1 m2 == multiplyG m1 (m2 :: RM)
    quickCheck $ \(PairM m1 m2) -> mulC m1 m2 == multiplyG m1 (m2 :: CM)
    putStrLn "--------- svd ---------"
    quickCheck (svdTest svdR)
    quickCheck (svdTest svdRdd)
    quickCheck (svdTest svdC)
    quickCheck (svdTest' svdR)
    quickCheck (svdTest' svdRdd)
    quickCheck (svdTest' svdC)
    quickCheck (svdTest' svdg)
    putStrLn "--------- eig ---------"
    quickCheck (eigTest   . sqm :: SqM Double -> Bool)
    quickCheck (eigTest   . sqm :: SqM (Complex Double) -> Bool)
    quickCheck (eigTestSH . sym :: Sym Double -> Bool)
    quickCheck (eigTestSH . her :: Her -> Bool)
    putStrLn "--------- inv ------"
    quickCheck (invTest . sqm   :: SqM Double -> Bool)
    quickCheck (invTest . sqm   :: SqM (Complex Double) -> Bool)
    putStrLn "--------- pinv ------"
    quickCheck (pinvTest . sqm   :: SqM Double -> Bool)
    if os == "mingw32"
        then putStrLn "complex pinvTest skipped in this OS"
        else quickCheck (pinvTest . sqm   :: SqM (Complex Double) -> Bool)
    putStrLn "--------- chol ------"
    runTestTT $ TestList
     [ test "cholR" cholRTest
     , test "cholC" cholRTest
     ]
    putStrLn "--------- qr ---------"
    quickCheck (qrTest GSL.qr)
    quickCheck (qrTest (GSL.unpackQR . GSL.qrPacked))
    quickCheck (qrTest (    unpackQR . GSL.qrPacked))
    quickCheck (qrTest qr ::RM->Bool)
    quickCheck (qrTest qr ::CM->Bool)
    putStrLn "--------- nullspace ------"
    quickCheck (nullspaceTest :: RM -> Bool)
    quickCheck (nullspaceTest :: CM -> Bool)
    putStrLn "--------- vector operations ------"
    quickCheck $ (\u -> sin u ^ 2 + cos u ^ 2 |~| (1::RM))
    quickCheck $ (\u -> sin u ** 2 + cos u ** 2 |~| (1::RM))
    quickCheck $ (\u -> cos u * tan u |~| sin (u::RM))
    quickCheck $ (\u -> (cos u * tan u) :~6~: sin (u::CM))
    runTestTT $ TestList
     [ test "arith1" $ ((ones (100,100) * 5 + 2)/0.5 - 7)**2 |~| (49 :: RM)
     , test "arith2" $ (((1+i) .* ones (100,100) * 5 + 2)/0.5 - 7)**2 |~| ( (140*i-51).*1 :: CM)
     , test "arith3" $ exp (i.*ones(10,10)*pi) + 1 |~| 0
     , test "<\\>"   $ (3><2) [2,0,0,3,1,1::Double] <\> 3|>[4,9,5] |~| 2|>[2,3]
     ]
    putStrLn "--------- GSL ------"
    quickCheck $ \v -> ifft (fft v) |~| v
    runTestTT $ TestList
     [ gammaTest
     , besselTest
     , exponentialTest
     , integrateTest
     , polySolveTest
     , test "det" detTest
     ]

bigtests = do
    putStrLn "--------- big matrices -----"
    runTestTT $ TestList
     [ test "eigS" $ eigTestSH bigmat
     , test "eigH" $ eigTestSH bigmatc
     , test "eigR" $ eigTest   bigmat
     , test "eigC" $ eigTest   bigmatc
     ]

main = tests
