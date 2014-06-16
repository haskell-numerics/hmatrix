#if __GLASGOW_HASKELL__ >= 708

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
Module      :  Numeric.LinearAlgebra.Static.Internal
Copyright   :  (c) Alberto Ruiz 2006-14
License     :  BSD3
Stability   :  provisional

-}

module Numeric.LinearAlgebra.Static.Internal where


import GHC.TypeLits
import Numeric.LinearAlgebra.HMatrix as LA
import Data.Packed as D
import Data.Packed.ST
import Data.Proxy(Proxy)
import Foreign.Storable(Storable)
import Text.Printf

--------------------------------------------------------------------------------

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

newtype R n = R (Dim n (Vector ℝ))
  deriving (Num,Fractional)

newtype C n = C (Dim n (Vector ℂ))
  deriving (Num,Fractional)

newtype L m n = L (Dim m (Dim n (Matrix ℝ)))

newtype M m n = M (Dim m (Dim n (Matrix  ℂ)))


mkR :: Vector ℝ -> R n
mkR = R . Dim

mkC :: Vector ℂ -> C n
mkC = C . Dim

mkL :: Matrix ℝ -> L m n
mkL x = L (Dim (Dim x))

mkM :: Matrix ℂ -> M m n
mkM x = M (Dim (Dim x))

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
    ok = rem (size v) n' == 0 && size x == (m',n') && null rest
    m' = fromIntegral . natVal $ (undefined :: Proxy m) :: Int
    n' = fromIntegral . natVal $ (undefined :: Proxy n) :: Int
    abort info = error $ st ++" "++show m' ++ " " ++ show n'++" can't be created from elements " ++ info

--------------------------------------------------------------------------------

class Num t => Sized t s d | s -> t, s -> d
  where
    konst     ::  t  -> s
    unwrap    ::  s  -> d
    fromList  :: [t] -> s
    extract   ::  s  -> d

singleV v = size v == 1
singleM m = rows m == 1 && cols m == 1


instance forall n. KnownNat n => Sized ℂ (C n) (Vector ℂ)
  where
    konst x = mkC (LA.scalar x)
    unwrap (C (Dim v)) = v
    fromList xs = C (gvect "C" xs)
    extract (unwrap -> v)
      | singleV v = LA.konst (v!0) d
      | otherwise = v
     where
       d = fromIntegral . natVal $ (undefined :: Proxy n)


instance forall n. KnownNat n => Sized ℝ (R n) (Vector ℝ)
  where
    konst x = mkR (LA.scalar x)
    unwrap (R (Dim v)) = v
    fromList xs = R (gvect "R" xs)
    extract (unwrap -> v)
      | singleV v = LA.konst (v!0) d
      | otherwise = v
     where
       d = fromIntegral . natVal $ (undefined :: Proxy n)



instance forall m n . (KnownNat m, KnownNat n) => Sized ℝ (L m n) (Matrix ℝ)
  where
    konst x = mkL (LA.scalar x)
    fromList xs = L (gmat "L" xs)
    unwrap (L (Dim (Dim m))) = m
    extract (isDiag -> Just (z,y,(m',n'))) = diagRect z y m' n'
    extract (unwrap -> a)
        | singleM a = LA.konst (a `atIndex` (0,0)) (m',n')
        | otherwise = a
      where
        m' = fromIntegral . natVal $ (undefined :: Proxy m)
        n' = fromIntegral . natVal $ (undefined :: Proxy n)


instance forall m n . (KnownNat m, KnownNat n) => Sized ℂ (M m n) (Matrix ℂ)
  where
    konst x = mkM (LA.scalar x)
    fromList xs = M (gmat "M" xs)
    unwrap (M (Dim (Dim m))) = m
    extract (isDiagC -> Just (z,y,(m',n'))) = diagRect z y m' n'
    extract (unwrap -> a)
        | singleM a = LA.konst (a `atIndex` (0,0)) (m',n')
        | otherwise = a
      where
        m' = fromIntegral . natVal $ (undefined :: Proxy m)
        n' = fromIntegral . natVal $ (undefined :: Proxy n)

--------------------------------------------------------------------------------

instance (KnownNat n, KnownNat m) => Transposable (L m n) (L n m)
  where
    tr a@(isDiag -> Just _) = mkL (extract a)
    tr (extract -> a) = mkL (tr a)

instance (KnownNat n, KnownNat m) => Transposable (M m n) (M n m)
  where
    tr a@(isDiagC -> Just _) = mkM (extract a)
    tr (extract -> a) = mkM (tr a)

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
    y = subVector 1 (size v-1) v
    ny = size y
    zeros = LA.konst 0 (max 0 (min m' n' - ny))
    yz = vjoin [y,zeros]

--------------------------------------------------------------------------------

instance forall n . KnownNat n => Show (R n)
  where
    show (R (Dim v))
      | singleV v = "("++show (v!0)++" :: R "++show d++")"
      | otherwise   = "(vector"++ drop 8 (show v)++" :: R "++show d++")"
      where
        d = fromIntegral . natVal $ (undefined :: Proxy n) :: Int

instance forall n . KnownNat n => Show (C n)
  where
    show (C (Dim v))
      | singleV v = "("++show (v!0)++" :: C "++show d++")"
      | otherwise   = "(vector"++ drop 8 (show v)++" :: C "++show d++")"
      where
        d = fromIntegral . natVal $ (undefined :: Proxy n) :: Int

instance forall m n . (KnownNat m, KnownNat n) => Show (L m n)
  where
    show (isDiag -> Just (z,y,(m',n'))) = printf "(diag %s %s :: L %d %d)" (show z) (drop 9 $ show y) m' n'
    show (L (Dim (Dim x)))
       | singleM x = printf "(%s :: L %d %d)" (show (x `atIndex` (0,0))) m' n'
       | otherwise = "(matrix"++ dropWhile (/='\n') (show x)++" :: L "++show m'++" "++show n'++")"
      where
        m' = fromIntegral . natVal $ (undefined :: Proxy m) :: Int
        n' = fromIntegral . natVal $ (undefined :: Proxy n) :: Int

instance forall m n . (KnownNat m, KnownNat n) => Show (M m n)
  where
    show (isDiagC -> Just (z,y,(m',n'))) = printf "(diag %s %s :: M %d %d)" (show z) (drop 9 $ show y) m' n'
    show (M (Dim (Dim x)))
       | singleM x = printf "(%s :: M %d %d)" (show (x `atIndex` (0,0))) m' n'
       | otherwise = "(matrix"++ dropWhile (/='\n') (show x)++" :: M "++show m'++" "++show n'++")"
      where
        m' = fromIntegral . natVal $ (undefined :: Proxy m) :: Int
        n' = fromIntegral . natVal $ (undefined :: Proxy n) :: Int

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

#else

module Numeric.LinearAlgebra.Static.Internal where

#endif

