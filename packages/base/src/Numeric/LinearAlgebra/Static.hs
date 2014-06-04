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
Module      :  Numeric.LinearAlgebra.Static
Copyright   :  (c) Alberto Ruiz 2006-14
License     :  BSD3
Stability   :  provisional

-}

module Numeric.LinearAlgebra.Static(
    Dim(..),
    R(..), C(..),
    lift1F, lift2F,
    vconcat, gvec2, gvec3, gvec4, gvect, gmat,
    Sized(..),
    singleV, singleM,GM
) where


import GHC.TypeLits
import Numeric.HMatrix as LA
import Data.Packed as D
import Data.Packed.ST
import Data.Proxy(Proxy)
import Foreign.Storable(Storable)



newtype R n = R (Dim n (Vector ℝ))
  deriving (Num,Fractional)


newtype C n = C (Dim n (Vector ℂ))
  deriving (Num,Fractional)



newtype Dim (n :: Nat) t = Dim t
  deriving Show

lift1F
  :: (c t -> c t)
  -> Dim n (c t) -> Dim n (c t)
lift1F f (Dim v) = Dim (f v)

lift2F
  :: (c t -> c t -> c t)
  -> Dim n (c t) -> Dim n (c t) -> Dim n (c t)
lift2F f (Dim u) (Dim v) = Dim (f u v)

--------------------------------------------------------------------------------

instance forall n t . (Num (Vector t), Numeric t )=> Num (Dim n (Vector t))
  where
    (+) = lift2F (+)
    (*) = lift2F (*)
    (-) = lift2F (-)
    abs = lift1F abs
    signum = lift1F signum
    negate = lift1F negate
    fromInteger x = Dim (fromInteger x)

instance (Num (Vector t), Num (Matrix t), Numeric t) => Fractional (Dim n (Vector t))
  where
    fromRational x = Dim (fromRational x)
    (/) = lift2F (/)


instance (Num (Matrix t), Numeric t) => Num (Dim m (Dim n (Matrix t)))
  where
    (+) = (lift2F . lift2F) (+)
    (*) = (lift2F . lift2F) (*)
    (-) = (lift2F . lift2F) (-)
    abs = (lift1F . lift1F) abs
    signum = (lift1F . lift1F) signum
    negate = (lift1F . lift1F) negate
    fromInteger x = Dim (Dim (fromInteger x))

instance (Num (Vector t), Num (Matrix t), Numeric t) => Fractional (Dim m (Dim n (Matrix t)))
  where
    fromRational x = Dim (Dim (fromRational x))
    (/) = (lift2F.lift2F) (/)

--------------------------------------------------------------------------------

type V n t = Dim n (Vector t)

ud :: Dim n (Vector t) -> Vector t
ud (Dim v) = v

mkV :: forall (n :: Nat) t . t -> Dim n t
mkV = Dim 

type GM m n t = Dim m (Dim n (Matrix t))

--ud2 :: Dim m (Dim n (Matrix t)) -> Matrix t
--ud2 (Dim (Dim m)) = m

mkM :: forall (m :: Nat) (n :: Nat) t . t -> Dim m (Dim n t)
mkM = Dim . Dim


vconcat :: forall n m t . (KnownNat n, KnownNat m, Numeric t)
    => V n t -> V m t -> V (n+m) t
(ud -> u) `vconcat` (ud -> v) = mkV (vjoin [u', v'])
  where
    du = fromIntegral . natVal $ (undefined :: Proxy n)
    dv = fromIntegral . natVal $ (undefined :: Proxy m)
    u' | du > 1 && size u == 1 = LA.konst (u D.@> 0) du
       | otherwise = u
    v' | dv > 1 && size v == 1 = LA.konst (v D.@> 0) dv
       | otherwise = v


gvec2 :: Storable t => t -> t -> V 2 t
gvec2 a b = mkV $ runSTVector $ do
    v <- newUndefinedVector 2
    writeVector v 0 a
    writeVector v 1 b
    return v

gvec3 :: Storable t => t -> t -> t -> V 3 t
gvec3 a b c = mkV $ runSTVector $ do
    v <- newUndefinedVector 3
    writeVector v 0 a
    writeVector v 1 b
    writeVector v 2 c
    return v


gvec4 :: Storable t => t -> t -> t -> t -> V 4 t
gvec4 a b c d = mkV $ runSTVector $ do
    v <- newUndefinedVector 4
    writeVector v 0 a
    writeVector v 1 b
    writeVector v 2 c
    writeVector v 3 d
    return v


gvect :: forall n t . (Show t, KnownNat n, Numeric t) => String -> [t] -> V n t
gvect st xs'
    | ok = mkV v
    | not (null rest) && null (tail rest) = abort (show xs')
    | not (null rest) = abort (init (show (xs++take 1 rest))++", ... ]")
    | otherwise = abort (show xs)
  where
    (xs,rest) = splitAt d xs'
    ok = size v == d && null rest
    v = LA.fromList xs
    d = fromIntegral . natVal $ (undefined :: Proxy n)
    abort info = error $ st++" "++show d++" can't be created from elements "++info


gmat :: forall m n t . (Show t, KnownNat m, KnownNat n, Numeric t) => String -> [t] -> GM m n t
gmat st xs'
    | ok = mkM x
    | not (null rest) && null (tail rest) = abort (show xs')
    | not (null rest) = abort (init (show (xs++take 1 rest))++", ... ]")
    | otherwise = abort (show xs)
  where
    (xs,rest) = splitAt (m'*n') xs'
    v = LA.fromList xs
    x = reshape n' v
    ok = rem (size v) n' == 0 && size x == (m',n') && null rest
    m' = fromIntegral . natVal $ (undefined :: Proxy m) :: Int
    n' = fromIntegral . natVal $ (undefined :: Proxy n) :: Int
    abort info = error $ st ++" "++show m' ++ " " ++ show n'++" can't be created from elements " ++ info


class Num t => Sized t s d | s -> t, s -> d
  where
    konst     ::  t  -> s
    unwrap    ::  s  -> d
    fromList  :: [t] -> s
    extract   ::  s  -> d

singleV v = size v == 1
singleM m = rows m == 1 && cols m == 1

