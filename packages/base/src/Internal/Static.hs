{-# LANGUAGE CPP #-}
#if __GLASGOW_HASKELL__ >= 708

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveGeneric #-}

{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

{- |
Module      :  Internal.Static
Copyright   :  (c) Alberto Ruiz 2006-14
License     :  BSD3
Stability   :  provisional

-}

module Internal.Static where


import GHC.TypeLits
import qualified Numeric.LinearAlgebra as LA
import Numeric.LinearAlgebra hiding (konst,size,R,C)
import Internal.Vector as D hiding (R,C)
import Internal.ST
import Control.DeepSeq
import Data.Proxy(Proxy)
import Foreign.Storable(Storable)
import Text.Printf

import Data.Binary
import GHC.Generics (Generic)
import Data.Proxy (Proxy(..))

--------------------------------------------------------------------------------

type ℝ = Double
type ℂ = Complex Double

newtype Dim (n :: Nat) t = Dim t
  deriving (Show, Generic)

instance (KnownNat n, Binary a) => Binary (Dim n a) where
  get = do
    k <- get
    let n = natVal (Proxy :: Proxy n)
    if n == k
      then Dim <$> get
      else fail ("Expected dimension " ++ (show n) ++ ", but found dimension " ++ (show k))

  put (Dim x) = do
    put (natVal (Proxy :: Proxy n))
    put x

lift1F
  :: (c t -> c t)
  -> Dim n (c t) -> Dim n (c t)
lift1F f (Dim v) = Dim (f v)

lift2F
  :: (c t -> c t -> c t)
  -> Dim n (c t) -> Dim n (c t) -> Dim n (c t)
lift2F f (Dim u) (Dim v) = Dim (f u v)

instance NFData t => NFData (Dim n t) where
    rnf (Dim (force -> !_)) = ()

--------------------------------------------------------------------------------

newtype R n = R (Dim n (Vector ℝ))
  deriving (Num,Fractional,Floating,Generic,Binary)

newtype C n = C (Dim n (Vector ℂ))
  deriving (Num,Fractional,Floating,Generic)

newtype L m n = L (Dim m (Dim n (Matrix ℝ)))
  deriving (Generic, Binary)

newtype M m n = M (Dim m (Dim n (Matrix ℂ)))
  deriving (Generic)

mkR :: Vector ℝ -> R n
mkR = R . Dim

mkC :: Vector ℂ -> C n
mkC = C . Dim

mkL :: Matrix ℝ -> L m n
mkL x = L (Dim (Dim x))

mkM :: Matrix ℂ -> M m n
mkM x = M (Dim (Dim x))

instance NFData (R n) where
    rnf (R (force -> !_)) = ()

instance NFData (C n) where
    rnf (C (force -> !_)) = ()

instance NFData (L n m) where
    rnf (L (force -> !_)) = ()

instance NFData (M n m) where
    rnf (M (force -> !_)) = ()

--------------------------------------------------------------------------------

type V n t = Dim n (Vector t)

ud :: Dim n (Vector t) -> Vector t
ud (Dim v) = v

mkV :: forall (n :: Nat) t . t -> Dim n t
mkV = Dim


vconcat :: forall n m t . (KnownNat n, KnownNat m, Numeric t)
    => V n t -> V m t -> V (n+m) t
(ud -> u) `vconcat` (ud -> v) = mkV (vjoin [u', v'])
  where
    du = fromIntegral . natVal $ (undefined :: Proxy n)
    dv = fromIntegral . natVal $ (undefined :: Proxy m)
    u' | du /= 1 && LA.size u == 1 = LA.konst (u D.@> 0) du
       | otherwise = u
    v' | dv /= 1 && LA.size v == 1 = LA.konst (v D.@> 0) dv
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
    ok = LA.size v == d && null rest
    v = LA.fromList xs
    d = fromIntegral . natVal $ (undefined :: Proxy n)
    abort info = error $ st++" "++show d++" can't be created from elements "++info


--------------------------------------------------------------------------------

type GM m n t = Dim m (Dim n (Matrix t))


gmat :: forall m n t . (Show t, KnownNat m, KnownNat n, Numeric t) => String -> [t] -> GM m n t
gmat st xs'
    | ok = Dim (Dim x)
    | not (null rest) && null (tail rest) = abort (show xs')
    | not (null rest) = abort (init (show (xs++take 1 rest))++", ... ]")
    | otherwise = abort (show xs)
  where
    (xs,rest) = splitAt (m'*n') xs'
    v = LA.fromList xs
    x = reshape n' v
    ok = null rest && ((n' == 0 && dim v == 0) || n'> 0 && (rem (LA.size v) n' == 0) && LA.size x == (m',n'))
    m' = fromIntegral . natVal $ (undefined :: Proxy m) :: Int
    n' = fromIntegral . natVal $ (undefined :: Proxy n) :: Int
    abort info = error $ st ++" "++show m' ++ " " ++ show n'++" can't be created from elements " ++ info

--------------------------------------------------------------------------------

class Num t => Sized t s d | s -> t, s -> d
  where
    konst     ::  t   -> s
    unwrap    ::  s   -> d t
    fromList  :: [t]  -> s
    extract   ::  s   -> d t
    create    ::  d t -> Maybe s
    size      ::  s   -> IndexOf d

singleV v = LA.size v == 1
singleM m = rows m == 1 && cols m == 1


instance KnownNat n => Sized ℂ (C n) Vector
  where
    size _ = fromIntegral . natVal $ (undefined :: Proxy n)
    konst x = mkC (LA.scalar x)
    unwrap (C (Dim v)) = v
    fromList xs = C (gvect "C" xs)
    extract s@(unwrap -> v)
      | singleV v = LA.konst (v!0) (size s)
      | otherwise = v
    create v
        | LA.size v == size r = Just r
        | otherwise = Nothing
      where
        r = mkC v :: C n


instance KnownNat n => Sized ℝ (R n) Vector
  where
    size _ = fromIntegral . natVal $ (undefined :: Proxy n)
    konst x = mkR (LA.scalar x)
    unwrap (R (Dim v)) = v
    fromList xs = R (gvect "R" xs)
    extract s@(unwrap -> v)
      | singleV v = LA.konst (v!0) (size s)
      | otherwise = v
    create v
        | LA.size v == size r = Just r
        | otherwise = Nothing
      where
        r = mkR v :: R n



instance (KnownNat m, KnownNat n) => Sized ℝ (L m n) Matrix
  where
    size _ = ((fromIntegral . natVal) (undefined :: Proxy m)
             ,(fromIntegral . natVal) (undefined :: Proxy n))
    konst x = mkL (LA.scalar x)
    fromList xs = L (gmat "L" xs)
    unwrap (L (Dim (Dim m))) = m
    extract (isDiag -> Just (z,y,(m',n'))) = diagRect z y m' n'
    extract s@(unwrap -> a)
        | singleM a = LA.konst (a `atIndex` (0,0)) (size s)
        | otherwise = a
    create x
        | LA.size x == size r = Just r
        | otherwise = Nothing
      where
        r = mkL x :: L m n


instance (KnownNat m, KnownNat n) => Sized ℂ (M m n) Matrix
  where
    size _ = ((fromIntegral . natVal) (undefined :: Proxy m)
             ,(fromIntegral . natVal) (undefined :: Proxy n))
    konst x = mkM (LA.scalar x)
    fromList xs = M (gmat "M" xs)
    unwrap (M (Dim (Dim m))) = m
    extract (isDiagC -> Just (z,y,(m',n'))) = diagRect z y m' n'
    extract s@(unwrap -> a)
        | singleM a = LA.konst (a `atIndex` (0,0)) (size s)
        | otherwise = a
    create x
        | LA.size x == size r = Just r
        | otherwise = Nothing
      where
        r = mkM x :: M m n

--------------------------------------------------------------------------------

instance (KnownNat n, KnownNat m) => Transposable (L m n) (L n m)
  where
    tr a@(isDiag -> Just _) = mkL (extract a)
    tr (extract -> a) = mkL (tr a)
    tr' = tr

instance (KnownNat n, KnownNat m) => Transposable (M m n) (M n m)
  where
    tr a@(isDiagC -> Just _) = mkM (extract a)
    tr (extract -> a) = mkM (tr a)
    tr' a@(isDiagC -> Just _) = mkM (extract a)
    tr' (extract -> a) = mkM (tr' a)

--------------------------------------------------------------------------------

isDiag :: forall m n . (KnownNat m, KnownNat n) => L m n -> Maybe (ℝ, Vector ℝ, (Int,Int))
isDiag (L x) = isDiagg x

isDiagC :: forall m n . (KnownNat m, KnownNat n) => M m n -> Maybe (ℂ, Vector ℂ, (Int,Int))
isDiagC (M x) = isDiagg x


isDiagg :: forall m n t . (Numeric t, KnownNat m, KnownNat n) => GM m n t -> Maybe (t, Vector t, (Int,Int))
isDiagg (Dim (Dim x))
    | singleM x = Nothing
    | rows x == 1 && m' > 1 || cols x == 1 && n' > 1 = Just (z,yz,(m',n'))
    | otherwise = Nothing
  where
    m' = fromIntegral . natVal $ (undefined :: Proxy m) :: Int
    n' = fromIntegral . natVal $ (undefined :: Proxy n) :: Int
    v = flatten x
    z = v `atIndex` 0
    y = subVector 1 (LA.size v-1) v
    ny = LA.size y
    zeros = LA.konst 0 (max 0 (min m' n' - ny))
    yz = vjoin [y,zeros]

--------------------------------------------------------------------------------

instance KnownNat n => Show (R n)
  where
    show s@(R (Dim v))
      | singleV v = "(" ++ show (v!0) ++ " :: R " ++ show d ++ ")"
      | otherwise = "(vector " ++ show v ++ " :: R " ++ show d ++")"
      where
        d = size s

instance KnownNat n => Show (C n)
  where
    show s@(C (Dim v))
      | singleV v = "(" ++ show (v!0) ++ " :: C " ++ show d ++ ")"
      | otherwise = "(vector " ++ show v ++ " :: C " ++ show d ++")"
      where
        d = size s

instance (KnownNat m, KnownNat n) => Show (L m n)
  where
    show (isDiag -> Just (z,y,(m',n'))) = printf "(diag %s %s :: L %d %d)" (show z) (show y) m' n'
    show s@(L (Dim (Dim x)))
       | singleM x = printf "(%s :: L %d %d)" (show (x `atIndex` (0,0))) m' n'
       | otherwise = "(matrix" ++ dropWhile (/='\n') (show x) ++ " :: L " ++ show m' ++ " " ++ show n' ++ ")"
      where
        (m',n') = size s

instance (KnownNat m, KnownNat n) => Show (M m n)
  where
    show (isDiagC -> Just (z,y,(m',n'))) = printf "(diag %s %s :: M %d %d)" (show z) (show y) m' n'
    show s@(M (Dim (Dim x)))
       | singleM x = printf "(%s :: M %d %d)" (show (x `atIndex` (0,0))) m' n'
       | otherwise = "(matrix" ++ dropWhile (/='\n') (show x) ++ " :: M " ++ show m' ++ " " ++ show n' ++ ")"
      where
        (m',n') = size s

--------------------------------------------------------------------------------

instance (Num (Vector t), Numeric t )=> Num (Dim n (Vector t))
  where
    (+) = lift2F (+)
    (*) = lift2F (*)
    (-) = lift2F (-)
    abs = lift1F abs
    signum = lift1F signum
    negate = lift1F negate
    fromInteger x = Dim (fromInteger x)

instance (Num (Vector t), Fractional t, Numeric t) => Fractional (Dim n (Vector t))
  where
    fromRational x = Dim (fromRational x)
    (/) = lift2F (/)

instance (Fractional t, Floating (Vector t), Numeric t) => Floating (Dim n (Vector t)) where
    sin   = lift1F sin
    cos   = lift1F cos
    tan   = lift1F tan
    asin  = lift1F asin
    acos  = lift1F acos
    atan  = lift1F atan
    sinh  = lift1F sinh
    cosh  = lift1F cosh
    tanh  = lift1F tanh
    asinh = lift1F asinh
    acosh = lift1F acosh
    atanh = lift1F atanh
    exp   = lift1F exp
    log   = lift1F log
    sqrt  = lift1F sqrt
    (**)  = lift2F (**)
    pi    = Dim pi


instance (Num (Vector t), Numeric t) => Num (Dim m (Dim n (Matrix t)))
  where
    (+) = (lift2F . lift2F) (+)
    (*) = (lift2F . lift2F) (*)
    (-) = (lift2F . lift2F) (-)
    abs = (lift1F . lift1F) abs
    signum = (lift1F . lift1F) signum
    negate = (lift1F . lift1F) negate
    fromInteger x = Dim (Dim (fromInteger x))

instance (Num (Vector t), Fractional t, Numeric t) => Fractional (Dim m (Dim n (Matrix t)))
  where
    fromRational x = Dim (Dim (fromRational x))
    (/) = (lift2F.lift2F) (/)

instance (Floating (Vector t), Floating t, Numeric t) => Floating (Dim m (Dim n (Matrix t))) where
    sin   = (lift1F . lift1F) sin
    cos   = (lift1F . lift1F) cos
    tan   = (lift1F . lift1F) tan
    asin  = (lift1F . lift1F) asin
    acos  = (lift1F . lift1F) acos
    atan  = (lift1F . lift1F) atan
    sinh  = (lift1F . lift1F) sinh
    cosh  = (lift1F . lift1F) cosh
    tanh  = (lift1F . lift1F) tanh
    asinh = (lift1F . lift1F) asinh
    acosh = (lift1F . lift1F) acosh
    atanh = (lift1F . lift1F) atanh
    exp   = (lift1F . lift1F) exp
    log   = (lift1F . lift1F) log
    sqrt  = (lift1F . lift1F) sqrt
    (**)  = (lift2F . lift2F) (**)
    pi    = Dim (Dim pi)

--------------------------------------------------------------------------------


adaptDiag f a@(isDiag -> Just _) b | isFull b = f (mkL (extract a)) b
adaptDiag f a b@(isDiag -> Just _) | isFull a = f a (mkL (extract b))
adaptDiag f a b = f a b

isFull m = isDiag m == Nothing && not (singleM (unwrap m))


lift1L f (L v) = L (f v)
lift2L f (L a) (L b) = L (f a b)
lift2LD f = adaptDiag (lift2L f)


instance (KnownNat n, KnownNat m) =>  Num (L n m)
  where
    (+) = lift2LD (+)
    (*) = lift2LD (*)
    (-) = lift2LD (-)
    abs = lift1L abs
    signum = lift1L signum
    negate = lift1L negate
    fromInteger = L . Dim . Dim . fromInteger

instance (KnownNat n, KnownNat m) => Fractional (L n m)
  where
    fromRational = L . Dim . Dim . fromRational
    (/) = lift2LD (/)

instance (KnownNat n, KnownNat m) => Floating (L n m) where
    sin   = lift1L sin
    cos   = lift1L cos
    tan   = lift1L tan
    asin  = lift1L asin
    acos  = lift1L acos
    atan  = lift1L atan
    sinh  = lift1L sinh
    cosh  = lift1L cosh
    tanh  = lift1L tanh
    asinh = lift1L asinh
    acosh = lift1L acosh
    atanh = lift1L atanh
    exp   = lift1L exp
    log   = lift1L log
    sqrt  = lift1L sqrt
    (**)  = lift2LD (**)
    pi    = konst pi

--------------------------------------------------------------------------------

adaptDiagC f a@(isDiagC -> Just _) b | isFullC b = f (mkM (extract a)) b
adaptDiagC f a b@(isDiagC -> Just _) | isFullC a = f a (mkM (extract b))
adaptDiagC f a b = f a b

isFullC m = isDiagC m == Nothing && not (singleM (unwrap m))

lift1M f (M v) = M (f v)
lift2M f (M a) (M b) = M (f a b)
lift2MD f = adaptDiagC (lift2M f)

instance (KnownNat n, KnownNat m) =>  Num (M n m)
  where
    (+) = lift2MD (+)
    (*) = lift2MD (*)
    (-) = lift2MD (-)
    abs = lift1M abs
    signum = lift1M signum
    negate = lift1M negate
    fromInteger = M . Dim . Dim . fromInteger

instance (KnownNat n, KnownNat m) => Fractional (M n m)
  where
    fromRational = M . Dim . Dim . fromRational
    (/) = lift2MD (/)

instance (KnownNat n, KnownNat m) => Floating (M n m) where
    sin   = lift1M sin
    cos   = lift1M cos
    tan   = lift1M tan
    asin  = lift1M asin
    acos  = lift1M acos
    atan  = lift1M atan
    sinh  = lift1M sinh
    cosh  = lift1M cosh
    tanh  = lift1M tanh
    asinh = lift1M asinh
    acosh = lift1M acosh
    atanh = lift1M atanh
    exp   = lift1M exp
    log   = lift1M log
    sqrt  = lift1M sqrt
    (**)  = lift2MD (**)
    pi    = M pi

instance Additive (R n) where
    add = (+)

instance Additive (C n) where
    add = (+)

instance (KnownNat m, KnownNat n) => Additive (L m n) where
    add = (+)

instance (KnownNat m, KnownNat n) => Additive (M m n) where
    add = (+)

--------------------------------------------------------------------------------


class Disp t
  where
    disp :: Int -> t -> IO ()


instance (KnownNat m, KnownNat n) => Disp (L m n)
  where
    disp n x = do
        let a = extract x
        let su = LA.dispf n a
        printf "L %d %d" (rows a) (cols a) >> putStr (dropWhile (/='\n') $ su)

instance (KnownNat m, KnownNat n) => Disp (M m n)
  where
    disp n x = do
        let a = extract x
        let su = LA.dispcf n a
        printf "M %d %d" (rows a) (cols a) >> putStr (dropWhile (/='\n') $ su)


instance KnownNat n => Disp (R n)
  where
    disp n v = do
        let su = LA.dispf n (asRow $ extract v)
        putStr "R " >> putStr (tail . dropWhile (/='x') $ su)

instance KnownNat n => Disp (C n)
  where
    disp n v = do
        let su = LA.dispcf n (asRow $ extract v)
        putStr "C " >> putStr (tail . dropWhile (/='x') $ su)

--------------------------------------------------------------------------------

overMatL' :: (KnownNat m, KnownNat n)
          => (LA.Matrix ℝ -> LA.Matrix ℝ) -> L m n -> L m n
overMatL' f = mkL . f . unwrap
{-# INLINE overMatL' #-}

overMatM' :: (KnownNat m, KnownNat n)
          => (LA.Matrix ℂ -> LA.Matrix ℂ) -> M m n -> M m n
overMatM' f = mkM . f . unwrap
{-# INLINE overMatM' #-}


#else

module Numeric.LinearAlgebra.Static.Internal where

#endif

