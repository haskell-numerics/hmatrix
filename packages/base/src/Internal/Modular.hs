{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies  #-}
{-# LANGUAGE TypeOperators #-}

{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

{- |
Module      :  Internal.Modular
Copyright   :  (c) Alberto Ruiz 2015
License     :  BSD3
Stability   :  experimental

Proof of concept of statically checked modular arithmetic.

-}

module Internal.Modular(
    Mod, type (./.)
) where

import Internal.Vector
import Internal.Matrix hiding (size)
import Internal.Numeric
import Internal.Element
import Internal.Container
import Internal.Vectorized (prodI,sumI,prodL,sumL)
import Internal.LAPACK (multiplyI, multiplyL)
import Internal.Algorithms(luFact,LU(..))
import Internal.Util(Normed(..),Indexable(..),
                     gaussElim, gaussElim_1, gaussElim_2,
                     luST, luSolve', luPacked', magnit, invershur)
import Internal.ST(mutable)
#if MIN_VERSION_base(4,11,0)
import GHC.TypeLits hiding (Mod)
#else
import GHC.TypeLits
#endif
import Data.Proxy(Proxy)
import Foreign.ForeignPtr(castForeignPtr)
import Foreign.Storable
import Data.Ratio
import Data.Complex
import Control.DeepSeq ( NFData(..) )
#if MIN_VERSION_base(4,11,0)
import Prelude hiding ((<>))
#endif



-- | Wrapper with a phantom integer for statically checked modular arithmetic.
newtype Mod (n :: Nat) t = Mod {unMod:: t}
  deriving (Storable)

instance (NFData t) => NFData (Mod n t)
  where
    rnf (Mod x) = rnf x

infixr 5 ./.
type (./.) x n = Mod n x

instance (Integral t, Enum t, KnownNat m) => Enum (Mod m t)
  where
    toEnum = l0 (\m x -> fromIntegral $ x `mod` (fromIntegral m))
    fromEnum = fromIntegral . unMod

instance (Eq t, KnownNat m) => Eq (Mod m t)
  where
    a == b = (unMod a) == (unMod b)

instance (Ord t, KnownNat m) => Ord (Mod m t)
  where
    compare a b = compare (unMod a) (unMod b)

instance (Integral t, Real t, KnownNat m) => Real (Mod m t)
  where
    toRational x = toInteger x % 1

instance (Integral t, KnownNat m) => Integral (Mod m t)
  where
    toInteger = toInteger . unMod
    quotRem a b = (Mod q, Mod r)
      where
         (q,r) = quotRem (unMod a) (unMod b)

-- | this instance is only valid for prime m
instance (Integral t, Show t, Eq t, KnownNat m) => Fractional (Mod m t)
  where
    recip x
        | x*r == 1  = r
        | otherwise = error $ show x ++" does not have a multiplicative inverse mod "++show m'
      where
        r = x^(m'-2 :: Integer)
        m' = fromIntegral . natVal $ (undefined :: Proxy m)
    fromRational x = fromInteger (numerator x) / fromInteger (denominator x)

l2 :: forall m a b c. (Num c, KnownNat m) => (c -> a -> b -> c) -> Mod m a -> Mod m b -> Mod m c
l2 f (Mod u) (Mod v) = Mod (f m' u v)
  where
    m' = fromIntegral . natVal $ (undefined :: Proxy m)

l1 :: forall m a b . (Num b, KnownNat m) => (b -> a -> b) -> Mod m a -> Mod m b
l1 f (Mod u) = Mod (f m' u)
  where
    m' = fromIntegral . natVal $ (undefined :: Proxy m)

l0 :: forall m a b . (Num b, KnownNat m) => (b -> a -> b) -> a -> Mod m b
l0 f u = Mod (f m' u)
  where
    m' = fromIntegral . natVal $ (undefined :: Proxy m)


instance Show t => Show (Mod n t)
  where
    show = show . unMod

instance (Integral t, KnownNat n) => Num (Mod n t)
  where
    (+) = l2 (\m a b -> (a + b) `mod` (fromIntegral m))
    (*) = l2 (\m a b -> (a * b) `mod` (fromIntegral m))
    (-) = l2 (\m a b -> (a - b) `mod` (fromIntegral m))
    abs = l1 (const abs)
    signum = l1 (const signum)
    fromInteger = l0 (\m x -> fromInteger x `mod` (fromIntegral m))


instance KnownNat m => Element (Mod m I)
  where
    constantD x n = i2f (constantD (unMod x) n)
    extractR ord m mi is mj js = i2fM <$> extractR ord (f2iM m) mi is mj js
    setRect i j m x = setRect i j (f2iM m) (f2iM x)
    sortI = sortI . f2i
    sortV = i2f . sortV . f2i
    compareV u v = compareV (f2i u) (f2i v)
    selectV c l e g = i2f (selectV c (f2i l) (f2i e) (f2i g))
    remapM i j m = i2fM (remap i j (f2iM m))
    rowOp c a i1 i2 j1 j2 x = rowOpAux (c_rowOpMI m') c (unMod a) i1 i2 j1 j2 (f2iM x)
      where
        m' = fromIntegral . natVal $ (undefined :: Proxy m)
    gemm u a b c = gemmg (c_gemmMI m') (f2i u) (f2iM a) (f2iM b) (f2iM c)
      where
        m' = fromIntegral . natVal $ (undefined :: Proxy m)
    reorderV strides dims = i2f . reorderAux c_reorderI strides dims . f2i

instance KnownNat m => Element (Mod m Z)
  where
    constantD x n = i2f (constantD (unMod x) n)
    extractR ord m mi is mj js = i2fM <$> extractR ord (f2iM m) mi is mj js
    setRect i j m x = setRect i j (f2iM m) (f2iM x)
    sortI = sortI . f2i
    sortV = i2f . sortV . f2i
    compareV u v = compareV (f2i u) (f2i v)
    selectV c l e g = i2f (selectV c (f2i l) (f2i e) (f2i g))
    remapM i j m = i2fM (remap i j (f2iM m))
    rowOp c a i1 i2 j1 j2 x = rowOpAux (c_rowOpML m') c (unMod a) i1 i2 j1 j2 (f2iM x)
      where
        m' = fromIntegral . natVal $ (undefined :: Proxy m)
    gemm u a b c = gemmg (c_gemmML m') (f2i u) (f2iM a) (f2iM b) (f2iM c)
      where
        m' = fromIntegral . natVal $ (undefined :: Proxy m)
    reorderV strides dims = i2f . reorderAux c_reorderL strides dims . f2i


instance KnownNat m => CTrans (Mod m I)
instance KnownNat m => CTrans (Mod m Z)


instance KnownNat m => Container Vector (Mod m I)
  where
    conj' = id
    size' = dim
    scale' s x = vmod (scale (unMod s) (f2i x))
    addConstant c x = vmod (addConstant (unMod c) (f2i x))
    add' a b = vmod (add' (f2i a) (f2i b))
    sub a b = vmod (sub (f2i a) (f2i b))
    mul a b = vmod (mul (f2i a) (f2i b))
    equal u v = equal (f2i u) (f2i v)
    scalar' x = fromList [x]
    konst' x = i2f . konst (unMod x)
    build' n f = build n (fromIntegral . f)
    cmap' = mapVector
    atIndex' x k = fromIntegral (atIndex (f2i x) k)
    minIndex'     = minIndex . f2i
    maxIndex'     = maxIndex . f2i
    minElement'   = Mod . minElement . f2i
    maxElement'   = Mod . maxElement . f2i
    sumElements'  = fromIntegral . sumI m' . f2i
      where
        m' = fromIntegral . natVal $ (undefined :: Proxy m)
    prodElements' = fromIntegral . prodI m' . f2i
      where
        m' = fromIntegral . natVal $ (undefined :: Proxy m)
    step'         = i2f . step . f2i
    find' = findV
    assoc' = assocV
    accum' = accumV
    ccompare' a b = ccompare (f2i a) (f2i b)
    cselect' c l e g = i2f $ cselect c (f2i l) (f2i e) (f2i g)
    scaleRecip s x = scale' s (cmap recip x)
    divide x y = mul x (cmap recip y)
    arctan2' = undefined
    cmod' m = vmod . cmod' (unMod m) . f2i
    fromInt' = vmod
    toInt'   = f2i
    fromZ'   = vmod . fromZ'
    toZ'     = toZ' . f2i

instance KnownNat m => Container Vector (Mod m Z)
  where
    conj' = id
    size' = dim
    scale' s x = vmod (scale (unMod s) (f2i x))
    addConstant c x = vmod (addConstant (unMod c) (f2i x))
    add' a b = vmod (add' (f2i a) (f2i b))
    sub a b = vmod (sub (f2i a) (f2i b))
    mul a b = vmod (mul (f2i a) (f2i b))
    equal u v = equal (f2i u) (f2i v)
    scalar' x = fromList [x]
    konst' x = i2f . konst (unMod x)
    build' n f = build n (fromIntegral . f)
    cmap' = mapVector
    atIndex' x k = fromIntegral (atIndex (f2i x) k)
    minIndex'     = minIndex . f2i
    maxIndex'     = maxIndex . f2i
    minElement'   = Mod . minElement . f2i
    maxElement'   = Mod . maxElement . f2i
    sumElements'  = fromIntegral . sumL m' . f2i
      where
        m' = fromIntegral . natVal $ (undefined :: Proxy m)
    prodElements' = fromIntegral . prodL m' . f2i
      where
        m' = fromIntegral . natVal $ (undefined :: Proxy m)
    step'         = i2f . step . f2i
    find' = findV
    assoc' = assocV
    accum' = accumV
    ccompare' a b = ccompare (f2i a) (f2i b)
    cselect' c l e g = i2f $ cselect c (f2i l) (f2i e) (f2i g)
    scaleRecip s x = scale' s (cmap recip x)
    divide x y = mul x (cmap recip y)
    arctan2' = undefined
    cmod' m = vmod . cmod' (unMod m) . f2i
    fromInt' = vmod . fromInt'
    toInt'   = toInt . f2i
    fromZ'   = vmod
    toZ'     = f2i


instance (Storable t, Indexable (Vector t) t) => Indexable (Vector (Mod m t)) (Mod m t)
  where
    (!) = (@>)

type instance RealOf (Mod n I) = I
type instance RealOf (Mod n Z) = Z

instance KnownNat m => Product (Mod m I) where
    norm2      = undefined
    absSum     = undefined
    norm1      = undefined
    normInf    = undefined
    multiply   = lift2m (multiplyI m')
      where
        m' = fromIntegral . natVal $ (undefined :: Proxy m)

instance KnownNat m => Product (Mod m Z) where
    norm2      = undefined
    absSum     = undefined
    norm1      = undefined
    normInf    = undefined
    multiply   = lift2m (multiplyL m')
      where
        m' = fromIntegral . natVal $ (undefined :: Proxy m)

instance KnownNat m => Normed (Vector (Mod m I))
  where
    norm_0 = norm_0 . toInt
    norm_1 = norm_1 . toInt
    norm_2 = norm_2 . toInt
    norm_Inf = norm_Inf . toInt

instance KnownNat m => Normed (Vector (Mod m Z))
  where
    norm_0 = norm_0 . toZ
    norm_1 = norm_1 . toZ
    norm_2 = norm_2 . toZ
    norm_Inf = norm_Inf . toZ


instance KnownNat m => Numeric (Mod m I)
instance KnownNat m => Numeric (Mod m Z)

i2f :: Storable t => Vector t -> Vector (Mod n t)
i2f v = unsafeFromForeignPtr (castForeignPtr fp) (i) (n)
    where (fp,i,n) = unsafeToForeignPtr v

f2i :: Storable t => Vector (Mod n t) -> Vector t
f2i v = unsafeFromForeignPtr (castForeignPtr fp) (i) (n)
    where (fp,i,n) = unsafeToForeignPtr v

f2iM :: (Element t, Element (Mod n t)) => Matrix (Mod n t) -> Matrix t
f2iM m = m { xdat = f2i (xdat m) }

i2fM :: (Element t, Element (Mod n t)) => Matrix t -> Matrix (Mod n t)
i2fM m = m { xdat = i2f (xdat m) }

vmod :: forall m t. (KnownNat m, Storable t, Integral t, Numeric t) => Vector t -> Vector (Mod m t)
vmod = i2f . cmod' m'
  where
    m' = fromIntegral . natVal $ (undefined :: Proxy m)

lift1 f a   = vmod (f (f2i a))
lift2 f a b = vmod (f (f2i a) (f2i b))

lift2m f a b = liftMatrix vmod (f (f2iM a) (f2iM b))

instance KnownNat m => Num (Vector (Mod m I))
  where
    (+) = lift2 (+)
    (*) = lift2 (*)
    (-) = lift2 (-)
    abs = lift1 abs
    signum = lift1 signum
    negate = lift1 negate
    fromInteger x = fromInt (fromInteger x)

instance KnownNat m => Num (Vector (Mod m Z))
  where
    (+) = lift2 (+)
    (*) = lift2 (*)
    (-) = lift2 (-)
    abs = lift1 abs
    signum = lift1 signum
    negate = lift1 negate
    fromInteger x = fromZ (fromInteger x)

--------------------------------------------------------------------------------

instance (KnownNat m) => Testable (Matrix (Mod m I))
  where
    checkT _ = test

test = (ok, info)
  where
    v = fromList [3,-5,75] :: Vector (Mod 11 I)
    m = (3><3) [1..]   :: Matrix (Mod 11 I)

    a = (3><3) [1,2 , 3
               ,4,5 , 6
               ,0,10,-3] :: Matrix I

    b = (3><2) [0..] :: Matrix I

    am = fromInt a :: Matrix (Mod 13 I)
    bm = fromInt b :: Matrix (Mod 13 I)
    ad = fromInt a :: Matrix Double
    bd = fromInt b :: Matrix Double

    g = (3><3) (repeat (40000)) :: Matrix I
    gm = fromInt g :: Matrix (Mod 100000 I)

    lg = (3><3) (repeat (3*10^(9::Int))) :: Matrix Z
    lgm = fromZ lg :: Matrix (Mod 10000000000 Z)

    gen  n = diagRect 1 (konst 5 n) n n :: Numeric t => Matrix t
    
    rgen n = gen n :: Matrix R
    cgen n = complex (rgen n) + fliprl (complex (rgen n)) * scalar (0:+1) :: Matrix C
    sgen n = single (cgen n)
    
    checkGen x = norm_Inf $ flatten $ invg x <> x - ident (rows x)
    
    invg t = gaussElim t (ident (rows t))

    checkLU okf t = norm_Inf $ flatten (l <> u <> p - t)
      where
        (l,u,p,_) = luFact (LU x' p')
          where
            (x',p') = mutable (luST okf) t

    checkSolve aa = norm_Inf $ flatten (aa <> x - bb)
       where
         bb = flipud aa
         x = luSolve' (luPacked' aa) bb

    tmm = diagRect 1 (fromList [2..6]) 5 5 :: Matrix (Mod 19 I)

    info = do
        print v
        print m
        print (tr m)
        print $ v+v
        print $ m+m
        print $ m <> m
        print $ m #> v

        print $ am <> gaussElim am bm - bm
        print $ ad <> gaussElim ad bd - bd

        print g
        print $ g <> g
        print gm
        print $ gm <> gm

        print lg
        print $ lg <> lg
        print lgm
        print $ lgm <> lgm
        
        putStrLn "checkGen"
        print (checkGen (gen 5 :: Matrix R))
        print (checkGen (gen 5 :: Matrix Float))
        print (checkGen (cgen 5 :: Matrix C))
        print (checkGen (sgen 5 :: Matrix (Complex Float)))
        print (invg (gen 5) :: Matrix (Mod 7 I))
        print (invg (gen 5) :: Matrix (Mod 7 Z))
        
        print $ mutable (luST (const True)) (gen 5 :: Matrix R)
        print $ mutable (luST (const True)) (gen 5 :: Matrix (Mod 11 Z))

        putStrLn "checkLU"
        print $ checkLU (magnit 0) (gen 5 :: Matrix R)
        print $ checkLU (magnit 0) (gen 5 :: Matrix Float)
        print $ checkLU (magnit 0) (cgen 5 :: Matrix C)
        print $ checkLU (magnit 0) (sgen 5 :: Matrix (Complex Float))
        print $ checkLU (magnit 0) (gen 5 :: Matrix (Mod 7 I))
        print $ checkLU (magnit 0) (gen 5 :: Matrix (Mod 7 Z))

        putStrLn "checkSolve"
        print $ checkSolve (gen 5 :: Matrix R)
        print $ checkSolve (gen 5 :: Matrix Float)
        print $ checkSolve (cgen 5 :: Matrix C)
        print $ checkSolve (sgen 5 :: Matrix (Complex Float))
        print $ checkSolve (gen 5 :: Matrix (Mod 7 I))
        print $ checkSolve (gen 5 :: Matrix (Mod 7 Z))
        
        putStrLn "luSolve'"
        print $ luSolve' (luPacked' tmm) (ident (rows tmm))
        print $ invershur tmm


    ok = and
      [ toInt (m #> v) == cmod 11 (toInt m #> toInt v )
      , am <> gaussElim_1 am bm == bm
      , am <> gaussElim_2 am bm == bm
      , am <> gaussElim   am bm == bm
      , (checkGen (gen 5 :: Matrix R)) < 1E-15
      , (checkGen (gen 5 :: Matrix Float)) < 2E-7
      , (checkGen (cgen 5 :: Matrix C)) < 1E-15
      , (checkGen (sgen 5 :: Matrix (Complex Float))) < 3E-7
      , (checkGen (gen 5 :: Matrix (Mod 7 I))) == 0
      , (checkGen (gen 5 :: Matrix (Mod 7 Z))) == 0
      , (checkLU (magnit 1E-10) (gen 5 :: Matrix R)) < 2E-15
      , (checkLU (magnit 1E-5) (gen 5 :: Matrix Float)) < 1E-6
      , (checkLU (magnit 1E-10) (cgen 5 :: Matrix C)) < 5E-15
      , (checkLU (magnit 1E-5) (sgen 5 :: Matrix (Complex Float))) < 1E-6
      , (checkLU (magnit 0) (gen 5 :: Matrix (Mod 7 I))) == 0
      , (checkLU (magnit 0) (gen 5 :: Matrix (Mod 7 Z))) == 0
      , checkSolve (gen 5 :: Matrix R) < 2E-15
      , checkSolve (gen 5 :: Matrix Float) < 1E-6
      , checkSolve (cgen 5 :: Matrix C) < 4E-15
      , checkSolve (sgen 5 :: Matrix (Complex Float)) < 1E-6
      , checkSolve (gen 5 :: Matrix (Mod 7 I)) == 0
      , checkSolve (gen 5 :: Matrix (Mod 7 Z)) == 0
      , prodElements (konst (9:: Mod 10 I) (12::Int)) == product (replicate 12 (9:: Mod 10 I))
      , gm <> gm == konst 0 (3,3)
      , lgm <> lgm == konst 0 (3,3)
      , invershur tmm == luSolve' (luPacked' tmm) (ident (rows tmm))
      , luSolve' (luPacked' (tr $ ident 5 :: Matrix (I ./. 2))) (ident 5) == ident 5
      ]


