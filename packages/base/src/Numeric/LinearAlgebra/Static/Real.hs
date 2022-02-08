{-# LANGUAGE CPP #-}
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
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoStarIsType #-}


{- |
Module      :  Numeric.LinearAlgebra.Static
Copyright   :  (c) Alberto Ruiz 2014
License     :  BSD3
Stability   :  experimental

Experimental interface with statically checked dimensions.

See code examples at http://dis.um.es/~alberto/hmatrix/static.html.

-}

module Numeric.LinearAlgebra.Static.Real(
    -- * Vector
       ℝ, R,
    vec2, vec3, vec4, (&), (#), split, headTail,
    vector,
    linspace, range, dim,
    -- * Matrix
    L, Sq, build,
    row, col, (|||),(===), splitRows, splitCols,
    unrow, uncol,
    tr,
    eye,
    diag, diagR,
    blockAt,
    matrix,
    -- * Products
    mul, app, dot,
    (<>),(#>),(<.>),(<·>),
    -- * Linear Systems
    linSolve, (<\>),
    -- * Norms
    Normed(..),
    -- * Random arrays
    Seed, RandDist(..),
    -- * Element accessing
    vAt, mAt,
    -- * Misc
    mean,
    Disp(..),
    cross, outer, mapR, mapL, det, zipWith,
    invlndet, expm, sqrtm, inv,
    isKonst, isKonstV,
    withVector, withMatrix, exactLength, exactDims,
    toRows, toColumns, withRows, withColumns,
    flatten, reshape,
    Sized(..), Diag(..),

) where


import GHC.TypeLits
import Numeric.LinearAlgebra hiding (
    (<>),(#>),(<.>),Konst(..),diag, disp,(===),(|||),
    row,col,vector,matrix,linspace,toRows,toColumns,
    (<\>),fromList,takeDiag,svd,eig,eigSH,
    eigenvalues,eigenvaluesSH,build,
    qr,size,dot,chol,range,R,C,sym,mTm,unSym,
    randomVector,rand,randn,gaussianSample,uniformSample,meanCov,
    toComplex, fromComplex, complex, real, magnitude,
    cross, outer, det, invlndet, expm, sqrtm, inv,
    flatten, reshape
    )
import qualified Numeric.LinearAlgebra as LA
import qualified Numeric.LinearAlgebra.Devel as LA
import Data.Proxy(Proxy(..))
import Internal.Static
import Control.Arrow((***))
import Data.Type.Equality ((:~:)(Refl))
import qualified Data.Bifunctor as BF (first)

import Prelude hiding (zipWith, (<>))
import Data.Finite (Finite, getFinite)


ud1 :: R n -> Vector ℝ
ud1 (R (Dim v)) = v


infixl 4 &
(&) :: forall n . KnownNat n
    => R n -> ℝ -> R (n+1)
u & x = u # (konst x :: R 1)

infixl 4 #
(#) :: forall n m . (KnownNat n, KnownNat m)
    => R n -> R m -> R (n+m)
(R u) # (R v) = R (vconcat u v)



vec2 :: ℝ -> ℝ -> R 2
vec2 a b = R (gvec2 a b)

vec3 :: ℝ -> ℝ -> ℝ -> R 3
vec3 a b c = R (gvec3 a b c)


vec4 :: ℝ -> ℝ -> ℝ -> ℝ -> R 4
vec4 a b c d = R (gvec4 a b c d)

vector :: KnownNat n => [ℝ] -> R n
vector = fromList

matrix :: (KnownNat m, KnownNat n) => [ℝ] -> L m n
matrix = fromList

linspace :: forall n . KnownNat n => (ℝ,ℝ) -> R n
linspace (a,b) = v
  where
    v = mkR (LA.linspace (size v) (a,b))

range :: forall n . KnownNat n => R n
range = v
  where
    v = mkR (LA.linspace d (1,fromIntegral d))
    d = size v

dim :: forall n . KnownNat n => R n
dim = v
  where
    v = mkR (scalar (fromIntegral $ size v))

--------------------------------------------------------------------------------


ud2 :: L m n -> Matrix ℝ
ud2 (L (Dim (Dim x))) = x


--------------------------------------------------------------------------------

diag :: KnownNat n => R n -> Sq n
diag = diagR 0

eye :: KnownNat n => Sq n
eye = diag 1

--------------------------------------------------------------------------------

blockAt :: forall m n . (KnownNat m, KnownNat n) =>  ℝ -> Int -> Int -> Matrix Double -> L m n
blockAt x r c a = res
  where
    z = scalar x
    z1 = LA.konst x (r,c)
    z2 = LA.konst x (max 0 (m'-(ra+r)), max 0 (n'-(ca+c)))
    ra = min (rows a) . max 0 $ m'-r
    ca = min (cols a) . max 0 $ n'-c
    sa = subMatrix (0,0) (ra, ca) a
    (m',n') = size res
    res = mkL $ fromBlocks [[z1,z,z],[z,sa,z],[z,z,z2]]

--------------------------------------------------------------------------------


row :: R n -> L 1 n
row = mkL . asRow . ud1

col :: KnownNat n => R n -> L n 1
col = tr . row

unrow :: L 1 n -> R n
unrow = mkR . head . LA.toRows . ud2

uncol :: KnownNat n => L n 1 -> R n
uncol = unrow . tr


infixl 2 ===
(===) :: (KnownNat r1, KnownNat r2, KnownNat c) => L r1 c -> L r2 c -> L (r1+r2) c
a === b = mkL (extract a LA.=== extract b)


infixl 3 |||
(|||) :: (KnownNat r, KnownNat c1, KnownNat c2, KnownNat (c1+c2)) => L r c1 -> L r c2 -> L r (c1+c2)
a ||| b = tr (tr a === tr b)


type Sq n  = L n n
--type CSq n = CL n n


isKonst :: forall m n . (KnownNat m, KnownNat n) => L m n -> Maybe (ℝ,(Int,Int))
isKonst s@(unwrap -> x)
    | singleM x = Just (x `atIndex` (0,0), size s)
    | otherwise = Nothing

isKonstV :: forall n . KnownNat n => R n -> Maybe (ℝ,Int)
isKonstV s@(unwrap -> x)
    | singleV x = Just (x `atIndex` 0, size s)
    | otherwise = Nothing

-- isKonstC :: forall m n . (KnownNat m, KnownNat n) => M m n -> Maybe (ℂ,(Int,Int))
-- isKonstC s@(unwrap -> x)
--     | singleM x = Just (x `atIndex` (0,0), (size s))
--     | otherwise = Nothing


infixr 8 <>
(<>) :: forall m k n. (KnownNat m, KnownNat k, KnownNat n) => L m k -> L k n -> L m n
(<>) = mul


infixr 8 #>
(#>) :: (KnownNat m, KnownNat n) => L m n -> R n -> R m
(#>) = app


infixr 8 <·>
(<·>) :: KnownNat n => R n -> R n -> ℝ
(<·>) = dot

infixr 8 <.>
(<.>) :: KnownNat n => R n -> R n -> ℝ
(<.>) = dot

--------------------------------------------------------------------------------

class Diag m d | m -> d
  where
    takeDiag :: m -> d


instance KnownNat n => Diag (L n n) (R n)
  where
    takeDiag x = mkR (LA.takeDiag (extract x))


instance KnownNat n => Diag (M n n) (C n)
  where
    takeDiag x = mkC (LA.takeDiag (extract x))

--------------------------------------------------------------------------------


-- toComplex :: KnownNat n => (R n, R n) -> C n
-- toComplex (r,i) = mkC $ LA.toComplex (ud1 r, ud1 i)

-- fromComplex :: KnownNat n => C n -> (R n, R n)
-- fromComplex (C (Dim v)) = let (r,i) = LA.fromComplex v in (mkR r, mkR i)

-- complex :: KnownNat n => R n -> C n
-- complex r = mkC $ LA.toComplex (ud1 r, LA.konst 0 (size r))

-- real :: KnownNat n => C n -> R n
-- real = fst . fromComplex

-- imag :: KnownNat n => C n -> R n
-- imag = snd . fromComplex

-- sqMagnitude :: KnownNat n => C n -> R n
-- sqMagnitude c = let (r,i) = fromComplex c in r**2 + i**2

-- magnitude :: KnownNat n => C n -> R n
-- magnitude = sqrt . sqMagnitude

--------------------------------------------------------------------------------

linSolve :: (KnownNat m, KnownNat n) => L m m -> L m n -> Maybe (L m n)
linSolve (extract -> a) (extract -> b) = fmap mkL (LA.linearSolve a b)

(<\>) :: (KnownNat m, KnownNat n, KnownNat r) => L m n -> L m r -> L n r
(extract -> a) <\> (extract -> b) = mkL (a LA.<\> b)

--------------------------------------------------------------------------------

split :: forall p n . (KnownNat p, KnownNat n, p<=n) => R n -> (R p, R (n-p))
split (extract -> v) = ( mkR (subVector 0 p' v) ,
                         mkR (subVector p' (LA.size v - p') v) )
  where
    p' = fromIntegral . natVal $ (undefined :: Proxy p) :: Int


headTail :: (KnownNat n, 1<=n) => R n -> (ℝ, R (n-1))
headTail = ((! 0) . extract *** id) . split


splitRows :: forall p m n . (KnownNat p, KnownNat m, KnownNat n, p<=m) => L m n -> (L p n, L (m-p) n)
splitRows (extract -> x) = ( mkL (takeRows p' x) ,
                             mkL (dropRows p' x) )
  where
    p' = fromIntegral . natVal $ (undefined :: Proxy p) :: Int

splitCols :: forall p m n. (KnownNat p, KnownNat m, KnownNat n, KnownNat (n-p), p<=n) => L m n -> (L m p, L m (n-p))
splitCols = (tr *** tr) . splitRows . tr


toRows :: forall m n . (KnownNat m, KnownNat n) => L m n -> [R n]
toRows (LA.toRows . extract -> vs) = map mkR vs

withRows
    :: forall n z . KnownNat n
    => [R n]
    -> (forall m . KnownNat m => L m n -> z)
    -> z
withRows (LA.fromRows . map extract -> m) f =
    case someNatVal $ fromIntegral $ LA.rows m of
       Nothing -> error "static/dynamic mismatch"
       Just (SomeNat (_ :: Proxy m)) -> f (mkL m :: L m n)

toColumns :: forall m n . (KnownNat m, KnownNat n) => L m n -> [R m]
toColumns (LA.toColumns . extract -> vs) = map mkR vs

withColumns
    :: forall m z . KnownNat m
    => [R m]
    -> (forall n . KnownNat n => L m n -> z)
    -> z
withColumns (LA.fromColumns . map extract -> m) f =
    case someNatVal $ fromIntegral $ LA.cols m of
       Nothing -> error "static/dynamic mismatch"
       Just (SomeNat (_ :: Proxy n)) -> f (mkL m :: L m n)



--------------------------------------------------------------------------------

build
  :: forall m n . (KnownNat n, KnownNat m)
    => (ℝ -> ℝ -> ℝ)
    -> L m n
build f = r
  where
    r = mkL $ LA.build (size r) f

--------------------------------------------------------------------------------

withVector
    :: forall z
     . Vector ℝ
    -> (forall n . (KnownNat n) => R n -> z)
    -> z
withVector v f =
    case someNatVal $ fromIntegral $ LA.size v of
       Nothing -> error "static/dynamic mismatch"
       Just (SomeNat (_ :: Proxy m)) -> f (mkR v :: R m)

-- | Useful for constraining two dependently typed vectors to match each
-- other in length when they are unknown at compile-time.
exactLength
    :: forall n m . (KnownNat n, KnownNat m)
    => R m
    -> Maybe (R n)
exactLength v = do
    Refl <- sameNat (Proxy :: Proxy n) (Proxy :: Proxy m)
    return $ mkR (unwrap v)

withMatrix
    :: forall z
     . Matrix ℝ
    -> (forall m n . (KnownNat m, KnownNat n) => L m n -> z)
    -> z
withMatrix a f =
    case someNatVal $ fromIntegral $ rows a of
       Nothing -> error "static/dynamic mismatch"
       Just (SomeNat (_ :: Proxy m)) ->
           case someNatVal $ fromIntegral $ cols a of
               Nothing -> error "static/dynamic mismatch"
               Just (SomeNat (_ :: Proxy n)) ->
                  f (mkL a :: L m n)

-- | Useful for constraining two dependently typed matrices to match each
-- other in dimensions when they are unknown at compile-time.
exactDims
    :: forall n m j k . (KnownNat n, KnownNat m, KnownNat j, KnownNat k)
    => L m n
    -> Maybe (L j k)
exactDims m = do
    Refl <- sameNat (Proxy :: Proxy m) (Proxy :: Proxy j)
    Refl <- sameNat (Proxy :: Proxy n) (Proxy :: Proxy k)
    return $ mkL (unwrap m)


--------------------------------------------------------------------------------

mul :: forall m k n. (KnownNat m, KnownNat k, KnownNat n) => L m k -> L k n -> L m n
mul (isKonst -> Just (a,(_,k))) (isKonst -> Just (b,_)) = konst (a * b * fromIntegral k)
mul (isDiag -> Just (0,a,_)) (isDiag -> Just (0,b,_)) = diagR 0 (mkR v :: R k)
  where
    v = a' * b'
    n = min (LA.size a) (LA.size b)
    a' = subVector 0 n a
    b' = subVector 0 n b
-- mul (isDiag -> Just (0,a,_)) (extract -> b) = mkL (asColumn a * takeRows (LA.size a) b)
-- mul (extract -> a) (isDiag -> Just (0,b,_)) = mkL (takeColumns (LA.size b) a * asRow b)
mul a b = mkL (extract a LA.<> extract b)


app :: (KnownNat m, KnownNat n) => L m n -> R n -> R m
app (isDiag -> Just (0, w, _)) v = mkR (w * subVector 0 (LA.size w) (extract v))
app m v = mkR (extract m LA.#> extract v)


dot :: KnownNat n => R n -> R n -> ℝ
dot (extract -> u) (extract -> v) = LA.dot u v


cross :: R 3 -> R 3 -> R 3
cross (extract -> x) (extract -> y) = vec3 z1 z2 z3
  where
    z1 = x!1*y!2-x!2*y!1
    z2 = x!2*y!0-x!0*y!2
    z3 = x!0*y!1-x!1*y!0

outer :: (KnownNat m, KnownNat n) => R n -> R m -> L n m
outer (extract -> x) (extract -> y) = mkL (LA.outer x y)

mapR :: KnownNat n => (ℝ -> ℝ) -> R n -> R n
mapR f (unwrap -> v) = mkR (LA.cmap f v)

zipWith :: KnownNat n => (ℝ -> ℝ -> ℝ) -> R n -> R n -> R n
zipWith f (extract -> x) (extract -> y) = mkR (LA.zipVectorWith f x y)

mapL :: (KnownNat n, KnownNat m) => (ℝ -> ℝ) -> L n m -> L n m
mapL f = overMatL' (LA.cmap f)

det :: KnownNat n => Sq n -> ℝ
det = LA.det . unwrap

invlndet :: KnownNat n => Sq n -> (L n n, (ℝ, ℝ))
invlndet = BF.first mkL . LA.invlndet . unwrap

expm :: KnownNat n => Sq n -> Sq n
expm = overMatL' LA.expm

sqrtm :: KnownNat n => Sq n -> Sq n
sqrtm = overMatL' LA.sqrtm

inv :: KnownNat n => Sq n -> Sq n
inv = overMatL' LA.inv

flatten :: (KnownNat m, KnownNat n, KnownNat (m * n)) => L m n -> R (m * n)
flatten (extract -> mat') = vector (LA.toList (LA.flatten mat'))

reshape :: forall m n k. (KnownNat m, KnownNat n, KnownNat k, n ~ (k * m)) => R n -> L k m
reshape (extract -> v) = mkL $ LA.reshape nrow v
  where
    nrow = (fromIntegral . natVal) (undefined :: Proxy k)


diagR :: forall m n k . (KnownNat m, KnownNat n, KnownNat k) => ℝ -> R k -> L m n
diagR x v
    | m' == 1 = mkL (LA.diagRect x ev m' n')
    | m'*n' > 0 = r
    | otherwise = matrix []
  where
    r = mkL (asRow (vjoin [scalar x, ev, zeros]))
    ev = extract v
    zeros = LA.konst x (max 0 ((min m' n') - LA.size ev))
    (m',n') = size r



mean :: (KnownNat n, 1<=n) => R n -> ℝ
mean v = v <·> (1/dim)


--------------------------------------------------------------------------------
-- Element access using the Finite type

vAt :: KnownNat n => R n -> Finite n -> ℝ
vAt v i = extract v LA.! fromIntegral (getFinite i)

mAt :: (KnownNat m, KnownNat n) => L m n -> Finite m -> Finite n -> ℝ
mAt m i' j' = extract m `LA.atIndex` (i, j)
  where i = fromIntegral (getFinite i')
        j = fromIntegral (getFinite j')
