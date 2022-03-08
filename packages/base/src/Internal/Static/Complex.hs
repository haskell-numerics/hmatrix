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

module Internal.Static.Complex where


import GHC.TypeLits
import Numeric.LinearAlgebra hiding (
    (<>),(#>),(<.>),Konst(..),diag, disp,(===),(|||),
    row,col,vector,matrix,linspace,toRows,toColumns,
    (<\>),fromList,takeDiag,svd,eig,eigSH,
    eigenvalues,eigenvaluesSH,build,
    qr,size,dot,chol,range,R,C,sym,mTm,unSym,
    randomVector,rand,randn,gaussianSample,uniformSample,meanCov,
    toComplex, fromComplex, complex, real, magnitude, diag,
    cross, outer, det, invlndet, expm, sqrtm, inv,
    flatten, reshape
    )
import qualified Numeric.LinearAlgebra as LA
import qualified Numeric.LinearAlgebra.Devel as LA
import Data.Proxy(Proxy(..))
import Internal.Static
import Control.Arrow((***), Arrow (first))
import Data.Type.Equality ((:~:)(Refl))
import qualified Data.Bifunctor as BF (first)

import Prelude hiding (zipWith, mapM, (<>))


infixl 4 &
(&) :: forall n . KnownNat n
    => C n -> ℂ -> C (n+1)
u & x = u # (konst x :: C 1)

infixl 4 #
(#) :: forall n m . (KnownNat n, KnownNat m)
    => C n -> C m -> C (n+m)
(C u) # (C v) = C (vconcat u v)



vec2 :: ℂ -> ℂ -> C 2
vec2 a b = C (gvec2 a b)

vec3 :: ℂ -> ℂ -> ℂ -> C 3
vec3 a b c = C (gvec3 a b c)


vec4 :: ℂ -> ℂ -> ℂ -> ℂ -> C 4
vec4 a b c d = C (gvec4 a b c d)

vector :: KnownNat n => [ℂ] -> C n
vector = fromList

matrix :: (KnownNat m, KnownNat n) => [ℂ] -> M m n
matrix = fromList

linspace :: forall n . KnownNat n => (ℂ,ℂ) -> C n
linspace (a,b) = v
  where
    v = mkC (LA.linspace (size v) (a,b))

range :: forall n . KnownNat n => C n
range = v
  where
    v = mkC (LA.linspace d (1,fromIntegral d))
    d = size v

dim :: forall n . KnownNat n => C n
dim = v
  where
    v = mkC (scalar (fromIntegral $ size v))


--------------------------------------------------------------------------------

diag :: KnownNat n => C n -> Sq n
diag = diagR 0

eye :: KnownNat n => Sq n
eye = diag 1


blockAt :: forall m n . (KnownNat m, KnownNat n) =>  ℂ -> Int -> Int -> Matrix ℂ -> M m n
blockAt x r c a = res
  where
    z = scalar x
    z1 = LA.konst x (r,c)
    z2 = LA.konst x (max 0 (m'-(ra+r)), max 0 (n'-(ca+c)))
    ra = min (rows a) . max 0 $ m'-r
    ca = min (cols a) . max 0 $ n'-c
    sa = subMatrix (0,0) (ra, ca) a
    (m',n') = size res
    res = mkM $ fromBlocks [[z1,z,z],[z,sa,z],[z,z,z2]]

--------------------------------------------------------------------------------


row :: KnownNat n => C n -> M 1 n
row = mkM . asRow . extract

col :: KnownNat n => C n -> M n 1
col = tr . row

unrow :: KnownNat n => M 1 n -> C n
unrow = mkC . head . LA.toRows . extract

uncol :: KnownNat n => M n 1 -> C n
uncol = unrow . tr


infixl 2 ===
(===) :: (KnownNat r1, KnownNat r2, KnownNat c) => M r1 c -> M r2 c -> M (r1+r2) c
a === b = mkM (extract a LA.=== extract b)


infixl 3 |||
(|||) :: (KnownNat r, KnownNat c1, KnownNat c2, KnownNat (c1+c2)) => M r c1 -> M r c2 -> M r (c1+c2)
a ||| b = tr (tr a === tr b)


type Sq n  = M n n
--type CSq n = CL n n


isKonst :: forall m n . (KnownNat m, KnownNat n) => M m n -> Maybe (ℂ,(Int,Int))
isKonst s@(unwrap -> x)
    | singleM x = Just (x `atIndex` (0,0), size s)
    | otherwise = Nothing

isKonstV :: forall n . KnownNat n => C n -> Maybe (ℂ,Int)
isKonstV s@(unwrap -> x)
    | singleV x = Just (x `atIndex` 0, size s)
    | otherwise = Nothing

infixr 8 <>
(<>) :: forall m k n. (KnownNat m, KnownNat k, KnownNat n) => M m k -> M k n -> M m n
(<>) = mul


infixr 8 #>
(#>) :: (KnownNat m, KnownNat n) => M m n -> C n -> C m
(#>) = app


infixr 8 <·>
(<·>) :: KnownNat n => C n -> C n -> ℂ
(<·>) = dot

infixr 8 <.>
(<.>) :: KnownNat n => C n -> C n -> ℂ
(<.>) = dot


linSolve :: (KnownNat m, KnownNat n) => M m m -> M m n -> Maybe (M m n)
linSolve (extract -> a) (extract -> b) = fmap mkM (LA.linearSolve a b)

(<\>) :: (KnownNat m, KnownNat n, KnownNat r) => M m n -> M m r -> M n r
(extract -> a) <\> (extract -> b) = mkM (a LA.<\> b)


--------------------------------------------------------------------------------

withNullspace
    :: forall m n z . (KnownNat m, KnownNat n)
    => L m n
    -> (forall k . (KnownNat k) => L n k -> z)
    -> z
withNullspace (LA.nullspace . extract -> a) f =
    case someNatVal $ fromIntegral $ cols a of
       Nothing -> error "static/dynamic mismatch"
       Just (SomeNat (_ :: Proxy k)) -> f (mkL a :: L n k)

withOrth
    :: forall m n z . (KnownNat m, KnownNat n)
    => L m n
    -> (forall k. (KnownNat k) => L n k -> z)
    -> z
withOrth (LA.orth . extract -> a) f =
    case someNatVal $ fromIntegral $ cols a of
       Nothing -> error "static/dynamic mismatch"
       Just (SomeNat (_ :: Proxy k)) -> f (mkL a :: L n k)

withCompactSVD
    :: forall m n z . (KnownNat m, KnownNat n)
    => L m n
    -> (forall k . (KnownNat k) => (L m k, R k, L n k) -> z)
    -> z
withCompactSVD (LA.compactSVD . extract -> (u,s,v)) f =
    case someNatVal $ fromIntegral $ LA.size s of
       Nothing -> error "static/dynamic mismatch"
       Just (SomeNat (_ :: Proxy k)) -> f (mkL u :: L m k, mkR s :: R k, mkL v :: L n k)

--------------------------------------------------------------------------------

qr :: (KnownNat m, KnownNat n) => L m n -> (L m m, L m n)
qr (extract -> x) = (mkL q, mkL r)
  where
    (q,r) = LA.qr x


split :: forall p n . (KnownNat p, KnownNat n, p<=n) => C n -> (C p, C (n-p))
split (extract -> v) = ( mkC (subVector 0 p' v) ,
                          mkC (subVector p' (LA.size v - p') v) )
  where
    p' = fromIntegral . natVal $ (undefined :: Proxy p) :: Int


headTail :: (KnownNat n, 1<=n) => C n -> (ℂ, C (n-1))
headTail = first ((! 0) . extract) . split


splitRows :: forall p m n . (KnownNat p, KnownNat m, KnownNat n, p<=m) => M m n -> (M p n, M (m-p) n)
splitRows (extract -> x) = ( mkM (takeRows p' x) ,
                              mkM (dropRows p' x) )
  where
    p' = fromIntegral . natVal $ (undefined :: Proxy p) :: Int

splitCols :: forall p m n. (KnownNat p, KnownNat m, KnownNat n, KnownNat (n-p), p<=n) => M m n -> (M m p, M m (n-p))
splitCols = (tr *** tr) . splitRows . tr


toRows :: forall m n . (KnownNat m, KnownNat n) => M m n -> [C n]
toRows (LA.toRows . extract -> vs) = map mkC vs

withRows
    :: forall n z . KnownNat n
    => [C n]
    -> (forall m . KnownNat m => M m n -> z)
    -> z
withRows (LA.fromRows . map extract -> m) f =
    case someNatVal $ fromIntegral $ LA.rows m of
       Nothing -> error "static/dynamic mismatch"
       Just (SomeNat (_ :: Proxy m)) -> f (mkM m :: M m n)

toColumns :: forall m n . (KnownNat m, KnownNat n) => M m n -> [C m]
toColumns (LA.toColumns . extract -> vs) = map mkC vs

withColumns
    :: forall m z . KnownNat m
    => [C m]
    -> (forall n . KnownNat n => M m n -> z)
    -> z
withColumns (LA.fromColumns . map extract -> m) f =
    case someNatVal $ fromIntegral $ LA.cols m of
       Nothing -> error "static/dynamic mismatch"
       Just (SomeNat (_ :: Proxy n)) -> f (mkM m :: M m n)


--------------------------------------------------------------------------------

build
  :: forall m n . (KnownNat n, KnownNat m)
    => (ℂ -> ℂ -> ℂ)
    -> M m n
build f = r
  where
    r = mkM $ LA.build (size r) f

--------------------------------------------------------------------------------

withVector
    :: forall z
     . Vector ℂ
    -> (forall n . (KnownNat n) => C n -> z)
    -> z
withVector v f =
    case someNatVal $ fromIntegral $ LA.size v of
       Nothing -> error "static/dynamic mismatch"
       Just (SomeNat (_ :: Proxy m)) -> f (mkC v :: C m)

-- | Useful for constraining two dependently typed vectors to match each
-- other in length when they are unknown at compile-time.
exactLength
    :: forall n m . (KnownNat n, KnownNat m)
    => C m
    -> Maybe (C n)
exactLength v = do
    Refl <- sameNat (Proxy :: Proxy n) (Proxy :: Proxy m)
    return $ mkC (unwrap v)

withMatrix
    :: forall z
     . Matrix ℂ
    -> (forall m n . (KnownNat m, KnownNat n) => M m n -> z)
    -> z
withMatrix a f =
    case someNatVal $ fromIntegral $ rows a of
       Nothing -> error "static/dynamic mismatch"
       Just (SomeNat (_ :: Proxy m)) ->
           case someNatVal $ fromIntegral $ cols a of
               Nothing -> error "static/dynamic mismatch"
               Just (SomeNat (_ :: Proxy n)) ->
                  f (mkM a :: M m n)

-- | Useful for constraining two dependently typed matrices to match each
-- other in dimensions when they are unknown at compile-time.
exactDims
    :: forall n m j k . (KnownNat n, KnownNat m, KnownNat j, KnownNat k)
    => M m n
    -> Maybe (M j k)
exactDims m = do
    Refl <- sameNat (Proxy :: Proxy m) (Proxy :: Proxy j)
    Refl <- sameNat (Proxy :: Proxy n) (Proxy :: Proxy k)
    return $ mkM (unwrap m)


-- --------------------------------------------------------------------------------

mul :: forall m k n. (KnownNat m, KnownNat k, KnownNat n) => M m k -> M k n -> M m n

mul (isKonst -> Just (a,(_,k))) (isKonst -> Just (b,_)) = konst (a * b * fromIntegral k)

mul (isDiagC -> Just (0,a,_)) (isDiagC -> Just (0,b,_)) = diagR 0 (mkC v :: C k)
  where
    v = a' * b'
    n = min (LA.size a) (LA.size b)
    a' = subVector 0 n a
    b' = subVector 0 n b

-- mul (isDiagC -> Just (0,a,_)) (extract -> b) = mkM (asColumn a * takeRows (LA.size a) b)

-- mul (extract -> a) (isDiagC -> Just (0,b,_)) = mkM (takeColumns (LA.size b) a * asRow b)

mul a b = mkM (extract a LA.<> extract b)


app :: (KnownNat m, KnownNat n) => M m n -> C n -> C m
app (isDiagC -> Just (0, w, _)) v = mkC (w * subVector 0 (LA.size w) (extract v))
app m v = mkC (extract m LA.#> extract v)


dot :: KnownNat n => C n -> C n -> ℂ
dot (extract -> u) (extract -> v) = LA.dot u v


cross :: C 3 -> C 3 -> C 3
cross (extract -> x) (extract -> y) = mkC (LA.fromList [z1, z2, z3])
  where
    z1 = x!1*y!2-x!2*y!1
    z2 = x!2*y!0-x!0*y!2
    z3 = x!0*y!1-x!1*y!0

outer :: (KnownNat m, KnownNat n) => C n -> C m -> M n m
outer (extract -> x) (extract -> y) = mkM (LA.outer x y)

mapC :: KnownNat n => (ℂ -> ℂ) -> C n -> C n
mapC f (unwrap -> v) = mkC (LA.cmap f v)

zipWith :: KnownNat n => (ℂ -> ℂ -> ℂ) -> C n -> C n -> C n
zipWith f (extract -> x) (extract -> y) = mkC (LA.zipVectorWith f x y)

zipWith3 :: KnownNat n => (ℂ -> ℂ -> ℂ -> ℂ) -> C n -> C n -> C n -> C n
zipWith3 f (extract -> x) (extract -> y) (extract -> z) = mkC (LA.zipVectorWith3 f x y z)

mapM :: (KnownNat n, KnownNat m) => (ℂ -> ℂ) -> M n m -> M n m
mapM f = overMatM' (LA.cmap f)

det :: KnownNat n => M n n -> ℂ
det = LA.det . unwrap

invlndet :: KnownNat n => M n n -> (M n n, (ℂ, ℂ))
invlndet = BF.first mkM . LA.invlndet . unwrap

expm :: KnownNat n => M n n -> M n n
expm = overMatM' LA.expm

sqrtm :: KnownNat n => M n n -> M n n
sqrtm = overMatM' LA.sqrtm

inv :: KnownNat n => M n n -> M n n
inv = overMatM' LA.inv


flatten
  :: forall m n. (KnownNat m, KnownNat n, KnownNat (m * n)) => M m n -> C (m * n)
flatten (extract -> mat') = vector (LA.toList (LA.flatten mat'))

reshape :: forall m n k. (KnownNat m, KnownNat n, KnownNat k, n ~ (k * m)) => C n -> M k m
reshape (extract -> v) = mkM $ LA.reshape nrow v
  where
    nrow = (fromIntegral . natVal) (undefined :: Proxy k)


diagR :: forall m n k . (KnownNat m, KnownNat n, KnownNat k) => ℂ -> C k -> M m n
diagR x v
    | m' == 1 = mkM (LA.diagRect x ev m' n')
    | m'*n' > 0 = r
    | otherwise = fromList []
  where
    r = mkM (asRow (vjoin [scalar x, ev, zeros]))
    ev = extract v
    zeros = LA.konst x (max 0 (min m' n' - LA.size ev))
    (m',n') = size r


mean :: (KnownNat n, 1<=n) => C n -> ℂ
mean v = v <·> (1/dim)
