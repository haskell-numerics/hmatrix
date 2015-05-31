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
Module      :  Numeric.LinearAlgebra.Util.Modular
Copyright   :  (c) Alberto Ruiz 2015
License     :  BSD3
Stability   :  experimental

Proof of concept of statically checked modular arithmetic.

-}

module Numeric.LinearAlgebra.Util.Modular(
    Mod, F
) where

import Data.Packed.Numeric
import Numeric.LinearAlgebra.Util(Indexable(..),size)
import GHC.TypeLits
import Data.Proxy(Proxy)
import Foreign.ForeignPtr(castForeignPtr)
import Data.Vector.Storable(unsafeToForeignPtr, unsafeFromForeignPtr)
import Foreign.Storable
import Data.Ratio
import Data.Packed.Internal.Matrix hiding (mat,size)
import Data.Packed.Internal.Numeric


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

-- | this instance is only valid for prime m (not checked)
instance KnownNat m => Fractional (F m)
  where
    recip x = x^(m'-2)
      where
        m' = fromIntegral . natVal $ (undefined :: Proxy m) :: Int
    fromRational x = fromInteger (numerator x) / fromInteger (denominator x)

l2 :: forall m a b c. (KnownNat m) => (Int -> a -> b -> c) -> Mod m a -> Mod m b -> Mod m c
l2 f (Mod u) (Mod v) = Mod (f m' u v)
  where
    m' = fromIntegral . natVal $ (undefined :: Proxy m) :: Int

l1 :: forall m a b . (KnownNat m) => (Int -> a -> b) -> Mod m a -> Mod m b
l1 f (Mod u) = Mod (f m' u)
  where
    m' = fromIntegral . natVal $ (undefined :: Proxy m) :: Int

l0 :: forall m a b . (KnownNat m) => (Int -> a -> b) -> a -> Mod m b
l0 f u = Mod (f m' u)
  where
    m' = fromIntegral . natVal $ (undefined :: Proxy m) :: Int


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
    size' = size
    scale' s x = fromInt (scale (unMod s) (toInt x))
    addConstant c x = fromInt (addConstant (unMod c) (toInt x))
    add a b = fromInt (add (toInt a) (toInt b))
    sub a b = fromInt (sub (toInt a) (toInt b))
    mul a b = fromInt (mul (toInt a) (toInt b))
    equal u v = equal (toInt u) (toInt v)
    scalar' x = fromList [x]
    konst' x = i2f . konst (unMod x)
    build' n f = build n (fromIntegral . f)
    cmap' = cmap
    atIndex' x k = fromIntegral (atIndex (toInt x) k)
    minIndex'     = minIndex . toInt
    maxIndex'     = maxIndex . toInt
    minElement'   = Mod . minElement . toInt
    maxElement'   = Mod . maxElement . toInt
    sumElements'  = fromIntegral . sumElements . toInt
    prodElements' = fromIntegral . sumElements . toInt
    step'         = i2f . step . toInt
    find' = findV
    assoc' = assocV
    accum' = accumV
    cond' x y l e g = cselect (ccompare x y) l e g
    ccompare' a b = ccompare (toInt a) (toInt b)
    cselect' c l e g = i2f $ cselect c (toInt l) (toInt e) (toInt g)
    scaleRecip s x = scale' s (cmap recip x)
    divide x y = mul x (cmap recip y)
    arctan2' = undefined
    cmod' m = fromInt' . cmod' (unMod m) . toInt'
    fromInt' v = i2f $ cmod' (fromIntegral m') (fromInt' v)
      where
        m' = fromIntegral . natVal $ (undefined :: Proxy m) :: Int
    toInt'   = f2i


instance Indexable (Vector (F m)) (F m)
  where
    (!) = (@>)


type instance RealOf (F n) = I


instance KnownNat m => Product (F m) where
    norm2      = undefined
    absSum     = undefined
    norm1      = undefined
    normInf    = undefined
    multiply   = lift2 multiply


instance KnownNat m => Numeric (F m)

i2f :: Vector I -> Vector (F n)
i2f v = unsafeFromForeignPtr (castForeignPtr fp) (i) (n)
    where (fp,i,n) = unsafeToForeignPtr v

f2i :: Vector (F n) -> Vector I
f2i v = unsafeFromForeignPtr (castForeignPtr fp) (i) (n)
    where (fp,i,n) = unsafeToForeignPtr v

f2iM :: Matrix (F n) -> Matrix I
f2iM = liftMatrix f2i

i2fM :: Matrix I -> Matrix (F n)
i2fM = liftMatrix i2f


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
    info = do
        print v
        print m
        print (tr m)
        print $ v+v
        print $ m+m
        print $ m <> m
        print $ m #> v


    ok = and
      [ toInt (m #> v) == cmod 11 (toInt m #> toInt v )
      ]


