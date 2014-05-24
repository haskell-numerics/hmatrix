{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeOperators #-}

module Numeric.LinearAlgebra.Util.Static(
    Static (ddata),
    R,
    vect0, sScalar, vect2, vect3, (&)
) where


import GHC.TypeLits
import Data.Packed.Numeric
import Numeric.Vector()
import Numeric.LinearAlgebra.Util(Numeric,ℝ)

lift1F :: (Vector t -> Vector t) -> Static n (Vector t) -> Static n (Vector t)
lift1F f (Static v) = Static (f v)

lift2F :: (Vector t -> Vector t -> Vector t) -> Static n (Vector t) -> Static n (Vector t) -> Static n (Vector t)
lift2F f (Static u) (Static v) = Static (f u v)

newtype Static (n :: Nat) t = Static { ddata :: t } deriving Show

type R n = Static n (Vector ℝ)


infixl 4 &
(&) :: R n -> ℝ -> R (n+1)
Static v & x = Static (vjoin [v, scalar x])

vect0 :: R 0
vect0 = Static (fromList[])

sScalar :: ℝ -> R 1
sScalar = Static . scalar


vect2 :: ℝ -> ℝ -> R 2
vect2 x1 x2 = Static (fromList [x1,x2])

vect3 :: ℝ -> ℝ -> ℝ -> R 3
vect3 x1 x2 x3 = Static (fromList [x1,x2,x3])






instance forall n t . (KnownNat n, Num (Vector t), Numeric t )=> Num (Static n (Vector t))
  where
    (+) = lift2F add
    (*) = lift2F mul
    (-) = lift2F sub
    abs = lift1F abs
    signum = lift1F signum
    negate = lift1F (scale (-1))
    fromInteger x = Static (konst (fromInteger x) d)
      where
        d = fromIntegral . natVal $ (undefined :: Proxy n)

data Proxy :: Nat -> *

