{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}


{- |
Module      :  Internal.Modular
Copyright   :  (c) Alberto Ruiz 2015
License     :  BSD3
Stability   :  experimental

Proof of concept of statically checked modular arithmetic.

-}

module Internal.Modular(
    Mod, F
) where

import Internal.Vector
import Internal.Matrix hiding (mat,size)
import Internal.Numeric
import Internal.Element
import Internal.Container
import Internal.Vectorized (prodI,sumI)
import Internal.LAPACK (multiplyI)
import Internal.Util(Indexable(..),gaussElim)
import GHC.TypeLits
import Data.Proxy(Proxy)
import Foreign.ForeignPtr(castForeignPtr)
import Foreign.Storable
import Data.Ratio



-- | Wrapper with a phantom integer for statically checked modular arithmetic.
newtype Mod (n :: Nat) t = Mod {unMod:: t}
  deriving (Storable)

instance KnownNat m => Enum (F m)
  where
    toEnum = l0 (\m x -> fromIntegral $ x `mod` (fromIntegral m))
    fromEnum = fromIntegral . unMod

instance KnownNat m => Eq (F m)
  where
    a == b = (unMod a) == (unMod b)

instance KnownNat m => Ord (F m)
  where
    compare a b = compare (unMod a) (unMod b)

instance KnownNat m => Real (F m)
  where
    toRational x = toInteger x % 1

instance KnownNat m => Integral (F m)
  where
    toInteger = toInteger . unMod
    quotRem a b = (Mod q, Mod r)
      where
         (q,r) = quotRem (unMod a) (unMod b)

-- | this instance is only valid for prime m
instance KnownNat m => Fractional (F m)
  where
    recip x
        | x*r == 1  = r
        | otherwise = error $ show x ++" does not have a multiplicative inverse mod "++show m'
      where
        r = x^(m'-2 :: Integer)
        m' = fromIntegral . natVal $ (undefined :: Proxy m)
    fromRational x = fromInteger (numerator x) / fromInteger (denominator x)

l2 :: forall m a b c. (KnownNat m) => (Int -> a -> b -> c) -> Mod m a -> Mod m b -> Mod m c
l2 f (Mod u) (Mod v) = Mod (f m' u v)
  where
    m' = fromIntegral . natVal $ (undefined :: Proxy m)

l1 :: forall m a b . (KnownNat m) => (Int -> a -> b) -> Mod m a -> Mod m b
l1 f (Mod u) = Mod (f m' u)
  where
    m' = fromIntegral . natVal $ (undefined :: Proxy m)

l0 :: forall m a b . (KnownNat m) => (Int -> a -> b) -> a -> Mod m b
l0 f u = Mod (f m' u)
  where
    m' = fromIntegral . natVal $ (undefined :: Proxy m)


instance Show (F n)
  where
    show = show . unMod

instance forall n . KnownNat n => Num (F n)
  where
    (+) = l2 (\m a b -> (a + b) `mod` (fromIntegral m))
    (*) = l2 (\m a b -> (a * b) `mod` (fromIntegral m))
    (-) = l2 (\m a b -> (a - b) `mod` (fromIntegral m))
    abs = l1 (const abs)
    signum = l1 (const signum)
    fromInteger = l0 (\m x -> fromInteger x `mod` (fromIntegral m))


-- | Integer modulo n
type F n = Mod n I

type V n = Vector (F n)
type M n = Matrix (F n)


instance Element (F n)
  where
    transdata n v m = i2f (transdata n (f2i v) m)
    constantD x n = i2f (constantD (unMod x) n)
    extractR m mi is mj js = i2fM (extractR (f2iM m) mi is mj js)
    sortI = sortI . f2i
    sortV = i2f . sortV . f2i
    compareV u v = compareV (f2i u) (f2i v)
    selectV c l e g = i2f (selectV c (f2i l) (f2i e) (f2i g))
    remapM i j m = i2fM (remap i j (f2iM m))

instance forall m . KnownNat m => Container Vector (F m)
  where
    conj' = id
    size' = dim
    scale' s x = vmod (scale (unMod s) (f2i x))
    addConstant c x = vmod (addConstant (unMod c) (f2i x))
    add a b = vmod (add (f2i a) (f2i b))
    sub a b = vmod (sub (f2i a) (f2i b))
    mul a b = vmod (mul (f2i a) (f2i b))
    equal u v = equal (f2i u) (f2i v)
    scalar' x = fromList [x]
    konst' x = i2f . konst (unMod x)
    build' n f = build n (fromIntegral . f)
    cmap' = cmap
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


instance Indexable (Vector (F m)) (F m)
  where
    (!) = (@>)


type instance RealOf (F n) = I

instance KnownNat m => Product (F m) where
    norm2      = undefined
    absSum     = undefined
    norm1      = undefined
    normInf    = undefined
    multiply   = lift2 (multiplyI m')
      where
        m' = fromIntegral . natVal $ (undefined :: Proxy m)

instance KnownNat m => Numeric (F m)

i2f :: Storable t => Vector t -> Vector (Mod n t)
i2f v = unsafeFromForeignPtr (castForeignPtr fp) (i) (n)
    where (fp,i,n) = unsafeToForeignPtr v

f2i :: Storable t => Vector (Mod n t) -> Vector t
f2i v = unsafeFromForeignPtr (castForeignPtr fp) (i) (n)
    where (fp,i,n) = unsafeToForeignPtr v

f2iM :: Storable t => Matrix (Mod n t) -> Matrix t
f2iM = liftMatrix f2i

i2fM :: Storable t => Matrix t -> Matrix (Mod n t)
i2fM = liftMatrix i2f

vmod :: forall m t. (KnownNat m, Storable t, Integral t, Numeric t) => Vector t -> Vector (Mod m t)
vmod = i2f . cmod' m'
  where
    m' = fromIntegral . natVal $ (undefined :: Proxy m)

lift1 f a   = fromInt (f (toInt a))
lift2 f a b = fromInt (f (toInt a) (toInt b))

instance forall m . KnownNat m => Num (V m)
  where
    (+) = lift2 (+)
    (*) = lift2 (*)
    (-) = lift2 (-)
    abs = lift1 abs
    signum = lift1 signum
    negate = lift1 negate
    fromInteger x = fromInt (fromInteger x)


--------------------------------------------------------------------------------

instance (KnownNat m) => Testable (M m)
  where
    checkT _ = test

test = (ok, info)
  where
    v = fromList [3,-5,75] :: V 11
    m = (3><3) [1..]   :: M 11

    a = (3><3) [1,2 , 3
               ,4,5 , 6
               ,0,10,-3] :: Matrix I

    b = (3><2) [0..] :: Matrix I

    am = fromInt a :: Matrix (F 13)
    bm = fromInt b :: Matrix (F 13)
    ad = fromInt a :: Matrix Double
    bd = fromInt b :: Matrix Double

    g = (3><3) (repeat (40000)) :: Matrix I
    gm = fromInt g :: Matrix (F 100000)

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

    ok = and
      [ toInt (m #> v) == cmod 11 (toInt m #> toInt v )
      , am <> gaussElim am bm == bm
      , prodElements (konst (9:: F 10) (12::Int)) == product (replicate 12 (9:: F 10))
      , gm <> gm == konst 0 (3,3)
      ]


