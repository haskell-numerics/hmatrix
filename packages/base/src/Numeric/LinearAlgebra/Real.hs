{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE GADTs #-}


{- |
Module      :  Numeric.LinearAlgebra.Real
Copyright   :  (c) Alberto Ruiz 2006-14
License     :  BSD3
Stability   :  provisional

Experimental interface for real arrays with statically checked dimensions.

-}

module Numeric.LinearAlgebra.Real(
    -- * Vector
    R,
    vec2, vec3, vec4, 𝕧, (&),
    -- * Matrix
    L, Sq,
    row, col, (¦),(——),
    Konst(..),
    eye,
    diagR, diag,
    blockAt,
    -- * Products
    (<>),(#>),(<·>),
    -- * Pretty printing
    Disp(..),
    -- * Misc
    Dim, unDim,
    module Numeric.HMatrix
) where


import GHC.TypeLits
import Numeric.HMatrix hiding ((<>),(#>),(<·>),Konst(..),diag, disp,(¦),(——),row,col)
import qualified Numeric.HMatrix as LA
import Data.Packed.ST
import Data.Proxy(Proxy)

newtype Dim (n :: Nat) t = Dim t
  deriving Show

unDim :: Dim n t -> t
unDim (Dim x) = x

-- data Proxy :: Nat -> *


lift1F
  :: (c t -> c t)
  -> Dim n (c t) -> Dim n (c t)
lift1F f (Dim v) = Dim (f v)

lift2F
  :: (c t -> c t -> c t)
  -> Dim n (c t) -> Dim n (c t) -> Dim n (c t)
lift2F f (Dim u) (Dim v) = Dim (f u v)



type R n = Dim n (Vector ℝ)

type L m n = Dim m (Dim n (Matrix ℝ))


infixl 4 &
(&) :: forall n . KnownNat n
    => R n -> ℝ -> R (n+1)
Dim v & x = Dim (vjoin [v', scalar x])
  where
    d = fromIntegral . natVal $ (undefined :: Proxy n)
    v' | d > 1 && size v == 1 = LA.konst (v!0) d
       | otherwise = v


-- vect0 :: R 0
-- vect0 = Dim (fromList[])

𝕧 :: ℝ -> R 1
𝕧 = Dim . scalar


vec2 :: ℝ -> ℝ -> R 2
vec2 a b = Dim $ runSTVector $ do
    v <- newUndefinedVector 2
    writeVector v 0 a
    writeVector v 1 b
    return v

vec3 :: ℝ -> ℝ -> ℝ -> R 3
vec3 a b c = Dim $ runSTVector $ do
    v <- newUndefinedVector 3
    writeVector v 0 a
    writeVector v 1 b
    writeVector v 2 c
    return v


vec4 :: ℝ -> ℝ -> ℝ -> ℝ -> R 4
vec4 a b c d = Dim $ runSTVector $ do
    v <- newUndefinedVector 4
    writeVector v 0 a
    writeVector v 1 b
    writeVector v 2 c
    writeVector v 3 d
    return v




instance forall n t . (Num (Vector t), Numeric t )=> Num (Dim n (Vector t))
  where
    (+) = lift2F (+)
    (*) = lift2F (*)
    (-) = lift2F (-)
    abs = lift1F abs
    signum = lift1F signum
    negate = lift1F negate
    fromInteger x = Dim (fromInteger x)

instance (Num (Matrix t), Numeric t) => Num (Dim m (Dim n (Matrix t)))
  where
    (+) = (lift2F . lift2F) (+)
    (*) = (lift2F . lift2F) (*)
    (-) = (lift2F . lift2F) (-)
    abs = (lift1F . lift1F) abs
    signum = (lift1F . lift1F) signum
    negate = (lift1F . lift1F) negate
    fromInteger x = Dim (Dim (fromInteger x))

instance Fractional (Dim n (Vector Double))
  where
    fromRational x = Dim (fromRational x)
    (/) = lift2F (/)

instance Fractional (Dim m (Dim n (Matrix Double)))
  where
    fromRational x = Dim (Dim (fromRational x))
    (/) = (lift2F.lift2F) (/)

--------------------------------------------------------------------------------

class Konst t
  where
    konst :: ℝ -> t

instance forall n. KnownNat n => Konst (R n)
  where
    konst x = Dim (LA.konst x d)
      where
        d = fromIntegral . natVal $ (undefined :: Proxy n)

instance forall m n . (KnownNat m, KnownNat n) => Konst (L m n)
  where
    konst x = Dim (Dim (LA.konst x (m',n')))
      where
        m' = fromIntegral . natVal $ (undefined :: Proxy m)
        n' = fromIntegral . natVal $ (undefined :: Proxy n)

--------------------------------------------------------------------------------

diagR :: forall m n k . (KnownNat m, KnownNat n) => ℝ -> R k -> L m n
diagR x v = Dim (Dim (diagRect x (unDim v) m' n'))
  where
    m' = fromIntegral . natVal $ (undefined :: Proxy m)
    n' = fromIntegral . natVal $ (undefined :: Proxy n)

diag :: KnownNat n => R n -> Sq n
diag = diagR 0

--------------------------------------------------------------------------------

blockAt :: forall m n . (KnownNat m, KnownNat n) =>  ℝ -> Int -> Int -> Matrix Double -> L m n
blockAt x r c a = Dim (Dim res)
  where
    z = scalar x
    z1 = LA.konst x (r,c)
    z2 = LA.konst x (max 0 (m'-(ra+r)), max 0 (n'-(ca+c)))
    ra = min (rows a) . max 0 $ m'-r
    ca = min (cols a) . max 0 $ n'-c
    sa = subMatrix (0,0) (ra, ca) a
    m' = fromIntegral . natVal $ (undefined :: Proxy m)
    n' = fromIntegral . natVal $ (undefined :: Proxy n)
    res = fromBlocks [[z1,z,z],[z,sa,z],[z,z,z2]]

{-
matrix :: (KnownNat m, KnownNat n) => Matrix Double -> L n m
matrix = blockAt 0 0 0
-}

--------------------------------------------------------------------------------

class Disp t
  where
    disp :: Int -> t -> IO ()

instance Disp (L n m)
  where
    disp n (d2 -> a) = do
        if rows a == 1 && cols a == 1
            then putStrLn $ "Const " ++ (last . words . LA.dispf n $ a)
            else putStr "Dim " >> LA.disp n a

instance Disp (R n)
  where
    disp n (unDim -> v) = do
        let su = LA.dispf n (asRow v)
        if LA.size v == 1
            then putStrLn $ "Const " ++ (last . words $ su )
            else putStr "Dim " >> putStr (tail . dropWhile (/='x') $ su)

--------------------------------------------------------------------------------
{-
infixl 3 #
(#) :: L r c -> R c -> L (r+1) c
Dim (Dim m) # Dim v = Dim (Dim (m LA.—— asRow v))


𝕞  :: forall n . KnownNat n => L 0 n
𝕞  = Dim (Dim (LA.konst 0 (0,d)))
  where
    d = fromIntegral . natVal $ (undefined :: Proxy n)
-}

row :: R n -> L 1 n
row (Dim v) = Dim (Dim (asRow v))

col :: R n -> L n 1
col = tr . row

infixl 3 ¦
(¦) :: (KnownNat r, KnownNat c1, KnownNat c2) => L r c1 -> L r c2 -> L r (c1+c2)
a ¦ b = rjoin (expk a) (expk b)
  where
    Dim (Dim a') `rjoin` Dim (Dim b') = Dim (Dim (a' LA.¦ b'))

infixl 2 ——
(——) :: (KnownNat r1, KnownNat r2, KnownNat c) => L r1 c -> L r2 c -> L (r1+r2) c
a —— b = cjoin (expk a) (expk b)
  where
    Dim (Dim a') `cjoin` Dim (Dim b') = Dim (Dim (a' LA.—— b'))

expk :: (KnownNat n, KnownNat m) => L m n -> L m n
expk x | singleton x = konst (d2 x `atIndex` (0,0))
       | otherwise = x
  where
    singleton (d2 -> m) = rows m == 1 && cols m == 1


{-

-}

type Sq n = L n n

type GL = (KnownNat n, KnownNat m) => L m n
type GSq = KnownNat n => Sq n

infixr 8 <>
(<>) :: L m k -> L k n -> L m n
(d2 -> a) <> (d2 -> b) = Dim (Dim (a LA.<> b))

infixr 8 #>
(#>) :: L m n -> R n -> R m
(d2 -> m) #> (unDim -> v) = Dim (m LA.#> v)

infixr 8 <·>
(<·>) :: R n -> R n -> ℝ
(unDim -> u) <·> (unDim -> v) = udot u v


d2 :: forall c (n :: Nat) (n1 :: Nat). Dim n1 (Dim n c) -> c
d2 = unDim . unDim


instance Transposable (L m n) (L n m)
  where
    tr (Dim (Dim a)) = Dim (Dim (tr a))


eye :: forall n . KnownNat n => Sq n
eye = Dim (Dim (ident d))
  where
    d = fromIntegral . natVal $ (undefined :: Proxy n)


--------------------------------------------------------------------------------

test :: (Bool, IO ())
test = (ok,info)
  where
    ok =   d2 (eye :: Sq 5) == ident 5
           && d2 (mTm sm :: Sq 3) == tr ((3><3)[1..]) LA.<> (3><3)[1..]
           && d2 (tm :: L 3 5) == mat 5 [1..15]
           && thingS == thingD
           && precS == precD

    info = do
        print $ u
        print $ v
        print (eye :: Sq 3)
        print $ ((u & 5) + 1) <·> v
        print (tm :: L 2 5)
        print (tm <> sm :: L 2 3)
        print thingS
        print thingD
        print precS
        print precD

    u = vec2 3 5

    v = 𝕧 2 & 4 & 7

    mTm :: L n m -> Sq m
    mTm a = tr a <> a

    tm :: GL
    tm = lmat 0 [1..]

    lmat :: forall m n . (KnownNat m, KnownNat n) => ℝ -> [ℝ] -> L m n
    lmat z xs = Dim . Dim . reshape n' . fromList . take (m'*n') $ xs ++ repeat z
      where
        m' = fromIntegral . natVal $ (undefined :: Proxy m)
        n' = fromIntegral . natVal $ (undefined :: Proxy n)

    sm :: GSq
    sm = lmat 0 [1..]

    thingS = (u & 1) <·> tr q #> q #> v
      where
        q = tm :: L 10 3

    thingD = vjoin [unDim u, 1] LA.<·> tr m LA.#> m LA.#> unDim v
      where
        m = mat 3 [1..30]

    precS = (1::Double) + (2::Double) * ((1 :: R 3) * (u & 6)) <·> konst 2 #> v
    precD = 1 + 2 * vjoin[unDim u, 6] LA.<·> LA.konst 2 (size (unDim u) +1, size (unDim v)) LA.#> unDim v


instance (KnownNat n', KnownNat m') => Testable (L n' m')
  where
    checkT _ = test


