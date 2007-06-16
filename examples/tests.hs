--
-- QuickCheck tests
--

-----------------------------------------------------------------------------

import Data.Packed.Internal
import Data.Packed.Vector
import Data.Packed.Matrix
import Data.Packed.Internal.Matrix
import GSL.Vector
import GSL.Integration
import GSL.Differentiation
import GSL.Special
import GSL.Fourier
import GSL.Polynomials
import LAPACK
import Test.QuickCheck
import Test.HUnit
import Complex


{-
-- Bravo por quickCheck!    

pinvProp1 tol m = (rank m == cols m) ==> pinv m <> m  ~~ ident (cols m)
    where infix 2 ~~
          (~~) = approxEqual tol 

pinvProp2 tol m = 0 < r && r <= c ==> (r==c) `trivial` (m <> pinv m <> m  ~~ m)
    where r = rank m
          c = cols m
          infix 2 ~~
          (~~) = approxEqual tol 
        
nullspaceProp tol m = cr > 0 ==> m <> nt ~~ zeros
    where nt    = trans (nullspace m)
          cr    = corank m
          r     = rows m
          zeros = create [r,cr] $ replicate (r*cr) 0  

-}

ac = (2><3) [1 .. 6::Double]
bc = (3><4) [7 .. 18::Double]

mz = (2 >< 3) [1,2,3,4,5,6:+(1::Double)]

af = (2>|<3) [1,4,2,5,3,6::Double]
bf = (3>|<4) [7,11,15,8,12,16,9,13,17,10,14,18::Double]

a |=| b = rows a == rows b &&
          cols a == cols b &&
          toList (cdat a) == toList (cdat b) &&
          toList (fdat a) == toList (fdat b)

aprox fun a b = rows a == rows b &&
          cols a == cols b &&
          eps > aproxL fun (toList (t a)) (toList (t b))
    where t = if (order a == RowMajor) `xor` isTrans a then cdat else fdat

aproxL fun v1 v2 = sum (zipWith (\a b-> fun (a-b)) v1 v2) / fromIntegral (length v1)

normVR a b = toScalarR AbsSum (vectorZipR Sub a b)

a |~| b = rows a == rows b && cols a == cols b && eps > normVR (t a) (t b)
    where t = if (order a == RowMajor) `xor` isTrans a then cdat else fdat

(|~~|) = aprox magnitude

v1 ~~ v2 = reshape 1 v1 |~~| reshape 1 v2

u ~|~ v = normVR u v < eps


eps = 1E-8::Double

asFortran m = (rows m >|< cols m) $ toList (fdat m)
asC m = (rows m >< cols m) $ toList (cdat m)

mulC a b = multiply RowMajor a b
mulF a b = multiply ColumnMajor a b

cc = mulC ac bf
cf = mulF af bc

r = mulC cc (trans cf)

rd = (2><2)
 [ 27736.0,  65356.0
 , 65356.0, 154006.0 ::Double]

instance (Arbitrary a, RealFloat a) => Arbitrary (Complex a) where
    arbitrary = do
        r <- arbitrary
        i <- arbitrary
        return (r:+i)
    coarbitrary = undefined

instance (Field a, Arbitrary a) => Arbitrary (Matrix a) where 
   arbitrary = do --m <- sized $ \max -> choose (1,1+3*max)
                  m <- choose (1,10)
                  n <- choose (1,10)
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
        a <- choose (1,10)
        b <- choose (1,10)
        c <- choose (1,10)
        l1 <- vector (a*b)
        l2 <- vector (b*c)
        return $ PairM ((a><b) (map fromIntegral (l1::[Int]))) ((b><c) (map fromIntegral (l2::[Int])))
        --return $ PairM ((a><b) l1) ((b><c) l2)
    coarbitrary = undefined

data SqM a = SqM (Matrix a) deriving Show
instance (Field a, Arbitrary a) => Arbitrary (SqM a) where
    arbitrary = do
        n <- choose (1,10)
        l <- vector (n*n)
        return $ SqM $ (n><n) l
    coarbitrary = undefined

data Sym a = Sym (Matrix a) deriving Show
instance (Field a, Arbitrary a, Num a) => Arbitrary (Sym a) where
    arbitrary = do
        SqM m <- arbitrary
        return $ Sym (m `addM` trans m)
    coarbitrary = undefined

data Her = Her (Matrix (Complex Double)) deriving Show
instance {-(Field a, Arbitrary a, Num a) =>-} Arbitrary Her where
    arbitrary = do
        SqM m <- arbitrary
        return $ Her (m `addM` (liftMatrix conj) (trans m))
    coarbitrary = undefined

data PairSM a = PairSM (Matrix a) (Matrix a) deriving Show
instance (Num a, Field a, Arbitrary a) => Arbitrary (PairSM a) where
    arbitrary = do
        a <- choose (1,10)
        c <- choose (1,10)
        l1 <- vector (a*a)
        l2 <- vector (a*c)
        return $ PairSM ((a><a) (map fromIntegral (l1::[Int]))) ((a><c) (map fromIntegral (l2::[Int])))
        --return $ PairSM ((a><a) l1) ((a><c) l2)
    coarbitrary = undefined

instance (Field a, Arbitrary a) => Arbitrary (Vector a) where 
   arbitrary = do --m <- sized $ \max -> choose (1,1+3*max)
                  m <- choose (1,100)
                  l <- vector m
                  return $ fromList l
   coarbitrary = undefined

data PairV a = PairV (Vector a) (Vector a)
instance (Field a, Arbitrary a) => Arbitrary (PairV a) where 
   arbitrary = do --m <- sized $ \max -> choose (1,1+3*max)
                  m <- choose (1,100)
                  l1 <- vector m
                  l2 <- vector m
                  return $ PairV (fromList l1) (fromList l2)
   coarbitrary = undefined


addM m1 m2 = liftMatrix2 add m1 m2


type BaseType = Double

svdTestR fun prod m = u <> s <> trans v |~| m
                  && u <> trans u |~| ident (rows m)
                  && v <> trans v |~| ident (cols m)
    where (u,s,v) = fun m
          (<>) = prod


svdTestC prod m = u <> s' <> (trans v) |~~| m
                  && u <> (liftMatrix conj) (trans u) |~~| ident (rows m)
                  && v <> (liftMatrix conj) (trans v) |~~| ident (cols m)
    where (u,s,v) = svdC m
          (<>) = prod
          s' = liftMatrix comp s

eigTestC prod (SqM m) = (m <> v) |~~| (v <> diag s)
                        && takeDiag ((liftMatrix conj (trans v)) <> v) ~~ constant (rows m) 1 --normalized
    where (s,v) = eigC m
          (<>) = prod

eigTestR prod (SqM m) = (liftMatrix comp m <> v) |~~| (v <> diag s)
                      -- && takeDiag ((liftMatrix conj (trans v)) <> v) ~~ constant (rows m) 1 --normalized ???
    where (s,v) = eigR m
          (<>) = prod

eigTestS prod (Sym m) = (m <> v) |~| (v <> diag s)
                       && v <> trans v |~| ident (cols m)
    where (s,v) = eigS m
          (<>) = prod

eigTestH prod (Her m) = (m <> v) |~~| (v <> diag (comp s))
                        && v <> (liftMatrix conj) (trans v) |~~| ident (cols m)
    where (s,v) = eigH m
          (<>) = prod

linearSolveSQTest fun eqfun singu prod (PairSM a b) = singu a || (a <> fun a b) ==== b
    where (<>) = prod
          (====) = eqfun


prec = 1E-15

singular fun m = s1 < prec || s2/s1 < prec
    where (_,ss,v) = fun m
          s = toList ss
          s1 = maximum s
          s2 = minimum s

{-
invTest msg m = do
    assertBool msg $ m <> inv m =~= ident (rows m)

invComplexTest msg m = do
    assertBool msg $ m <> invC m =~= identC (rows m)

invC m = linearSolveC m (identC (rows m))

identC n = toComplex(ident n, (0::Double) <>ident n)
-}

--------------------------------------------------------------------

pinvTest f feq m = (m <> f m <> m) `feq` m
    where (<>) = mulF

pinvR m = linearSolveLSR m (ident (rows m))
pinvC m = linearSolveLSC m (ident (rows m))

pinvSVDR m = linearSolveSVDR Nothing m (ident (rows m))

pinvSVDC m = linearSolveSVDC Nothing m (ident (rows m))

--------------------------------------------------------------------

polyEval cs x = foldr (\c ac->ac*x+c) 0 cs

polySolveTest' p = length p <2 || last p == 0|| 1E-8 > maximum (map magnitude $ map (polyEval (map (:+0) p)) (polySolve p))
    where l1 |~~| l2 = eps > aproxL magnitude l1 l2

polySolveTest = assertBool "polySolve" (polySolveTest' [1,2,3,4])

---------------------------------------------------------------------

quad f a b = fst $ integrateQAGS 1E-9 100 f a b  

-- A multiple integral can be easily defined using partial application
quad2 f a b g1 g2 = quad h a b
    where h x = quad (f x) (g1 x) (g2 x)

volSphere r = 8 * quad2 (\x y -> sqrt (r*r-x*x-y*y)) 
                        0 r (const 0) (\x->sqrt (r*r-x*x))

integrateTest = assertBool "integrate" (abs (volSphere 2.5 - 4/3*pi*2.5^3) < eps)


---------------------------------------------------------------------

arit1 u = vectorMapValR PowVS 2 (vectorMapR Sin u)
          `add` vectorMapValR PowVS 2 (vectorMapR Cos u)
          ~|~ constant (dim u) 1

arit2 u = (vectorMapR Cos u) `mul` (vectorMapR Tan u)
          ~|~ vectorMapR Sin u


--arit3 (PairV u v) = vectorMap Sin . VectorMap Cos

---------------------------------------------------------------------

besselTest = do
    let (r,e) = bessel_J0_e 5.0
    let expected = -0.17759677131433830434739701
    assertBool "bessel_J0_e" ( abs (r-expected) < e ) 

exponentialTest = do
    let (v,e,err) = exp_e10_e 30.0
    let expected = exp 30.0
    assertBool "exp_e10_e" ( abs (v*10^e - expected) < 4E-2 ) 

tests = TestList
    [ TestCase $ besselTest
    , TestCase $ exponentialTest
    , TestCase $ polySolveTest
    , TestCase $ integrateTest
    ]

----------------------------------------------------------------------

main = do
    putStrLn "--------- general -----"
    quickCheck (\(Sym m) -> m |=| (trans m:: Matrix BaseType))    
    quickCheck $ \l -> null l || (toList . fromList) l == (l :: [BaseType])

    quickCheck $ \m -> m |=| asC (m :: Matrix BaseType)
    quickCheck $ \m -> m |=| asFortran (m :: Matrix BaseType)
    quickCheck $ \m -> m |=| (asC . asFortran) (m :: Matrix BaseType)
    putStrLn "--------- MULTIPLY ----"
    quickCheck $ \(PairM m1 m2) -> mulC m1 m2 |=| mulF m1 (m2 :: Matrix BaseType)
    quickCheck $ \(PairM m1 m2) -> mulC m1 m2 |=| trans (mulF (trans m2) (trans m1 :: Matrix BaseType))
    quickCheck $ \(PairM m1 m2) -> mulC m1 m2 |=| multiplyG m1 (m2 :: Matrix BaseType)
    putStrLn "--------- SVD ---------"
    quickCheck (svdTestR svdR mulC)
    quickCheck (svdTestR svdR mulF)
    quickCheck (svdTestR svdRdd mulC)
    quickCheck (svdTestR svdRdd mulF)
    quickCheck (svdTestC mulC)
    quickCheck (svdTestC mulF)
    putStrLn "--------- EIG ---------"
    quickCheck (eigTestC mulC)
    quickCheck (eigTestC mulF)
    quickCheck (eigTestR mulC)
    quickCheck (eigTestR mulF)
    quickCheck (eigTestS mulC)
    quickCheck (eigTestS mulF)
    quickCheck (eigTestH mulC)
    quickCheck (eigTestH mulF)
    putStrLn "--------- SOLVE ---------"
    quickCheck (linearSolveSQTest linearSolveR (|~|) (singular svdR') mulC)
    quickCheck (linearSolveSQTest linearSolveC (|~~|) (singular svdC') mulF)
    quickCheck (pinvTest pinvR (|~|))
    quickCheck (pinvTest pinvC (|~~|))
    quickCheck (pinvTest pinvSVDR (|~|))
    quickCheck (pinvTest pinvSVDC (|~~|))
    putStrLn "--------- VEC OPER ------"
    quickCheck arit1
    quickCheck arit2
    putStrLn "--------- GSL ------"
    runTestTT tests
    quickCheck $ \v -> ifft (fft v) ~~ v

kk = (2><2)
 [  1.0, 0.0
 , -1.5, 1.0 ::Double]

v = 11 # [0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0::Double]

pol =  [14.125,-7.666666666666667,-14.3,-13.0,-7.0,-9.6,4.666666666666666,13.0,0.5]