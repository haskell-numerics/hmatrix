--
-- QuickCheck tests
--

-----------------------------------------------------------------------------

import Data.Packed.Internal.Vector
import Data.Packed.Internal.Matrix
import LAPACK
import Test.QuickCheck
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

r >< c = f where
    f l | dim v == r*c = matrixFromVector RowMajor c v
        | otherwise    = error $ "inconsistent list size = "
                                 ++show (dim v) ++"in ("++show r++"><"++show c++")"
        where v = fromList l

r >|< c = f where
    f l | dim v == r*c = matrixFromVector ColumnMajor c v
        | otherwise    = error $ "inconsistent list size = "
                                 ++show (dim v) ++"in ("++show r++"><"++show c++")"
        where v = fromList l

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

(|~|) = aprox abs
(|~~|) = aprox magnitude

eps = 1E-8::Double

asFortran m = (rows m >|< cols m) $ toList (fdat m)
asC m = (rows m >< cols m) $ toList (cdat m)

mulC a b = multiply RowMajor a b
mulF a b = multiply ColumnMajor a b

cc = mulC ac bf
cf = mulF af bc

r = mulC cc (trans cf)

ident n = diag (constant n 1)

rd = (2><2)
 [  43492.0,  50572.0
 , 102550.0, 119242.0 :: Double]

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

type BaseType = Double


svdTestR fun prod m = u <> s <> trans v |~| m
                  && u <> trans u |~| ident (rows m)
                  && v <> trans v |~| ident (cols m)
    where (u,s,v) = fun m
          (<>) = prod


svdTestC fun prod m = u <> s' <> (trans v) |~~| m
                  && u <> (liftMatrix conj) (trans u) |~~| ident (rows m)
                  && v <> (liftMatrix conj) (trans v) |~~| ident (cols m)
    where (u,s,v) = fun m
          (<>) = prod
          s' = liftMatrix comp s

comp v = toComplex (v,constant (dim v) 0)

main = do
    quickCheck $ \l -> null l || (toList . fromList) l == (l :: [BaseType])
    quickCheck $ \m -> m |=| asC (m :: Matrix BaseType)
    quickCheck $ \m -> m |=| asFortran (m :: Matrix BaseType)
    quickCheck $ \m -> m |=| (asC . asFortran) (m :: Matrix BaseType)
    quickCheck $ \(PairM m1 m2) -> mulC m1 m2 |=| mulF m1 (m2 :: Matrix BaseType)
    quickCheck $ \(PairM m1 m2) -> mulC m1 m2 |=| trans (mulF (trans m2) (trans m1 :: Matrix BaseType))
    quickCheck $ \(PairM m1 m2) -> mulC m1 m2 |=| multiplyG m1 (m2 :: Matrix BaseType)
    quickCheck (svdTestR svdR mulC)
    quickCheck (svdTestR svdR mulF)
    quickCheck (svdTestC svdC mulC)
    quickCheck (svdTestC svdC mulF)
