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

module Numeric.LinearAlgebra.Static(
    -- * Vector
       ℝ, R,
    -- * Matrix
    L, Sq,
    tr,
    -- * Complex
    ℂ, C, M, Her, her, 𝑖,
    toComplex,
    fromComplex,
    complex,
    real,
    imag,
    sqMagnitude,
    magnitude,
    -- * Factorizations
    svd, withCompactSVD, svdTall, svdFlat, Eigen(..),
    withNullspace, withOrth, qr, chol,
    -- * Norms
    Normed(..),
    -- * Random arrays
    Seed, RandDist(..),
    randomVector, rand, randn, gaussianSample, uniformSample,
    -- * Misc
    Disp(..), Domain(..),
    Sized(..), Diag(..), Sym, sym, mTm, unSym,
    -- * Element access
    Coord(..), ElementAccess(..),
    -- * Existential Wrappers
    toSomeR, toSomeL, toSomeC, toSomeM,
    toContR, toContL, toContC, toContM
) where


import GHC.TypeLits
import Numeric.LinearAlgebra hiding (
    (<>),(#>),(<.>),Konst(..),diag, disp,(===),(|||),
    row,col,vector,matrix,linspace,toRows,toColumns,
    (<\>),fromList,takeDiag,svd,eig,eigSH,
    eigenvalues,eigenvaluesSH,build,
    qr,size,dot,chol,range,R,C,sym,mTm,unSym,
    randomVector,rand,randn,gaussianSample,uniformSample,meanCov,
    toComplex, fromComplex, complex, real, magnitude, diag
    )
import qualified Numeric.LinearAlgebra as LA
import qualified Internal.Static.Real as R
import qualified Internal.Static.Complex as C
import Data.Proxy(Proxy(..))
import Internal.Static
import Text.Printf
import Control.Monad.Cont (Cont)
#if MIN_VERSION_base(4,11,0)
import Prelude hiding ((<>))
#endif


class Domain field vec mat | mat -> vec field, vec -> mat field, field -> mat vec
  where

    mul :: forall m k n. (KnownNat m, KnownNat k, KnownNat n) => mat m k -> mat k n -> mat m n
    app :: forall m n . (KnownNat m, KnownNat n) => mat m n -> vec n -> vec m
    dot :: forall n . (KnownNat n) => vec n -> vec n -> field
    infixr 8 <>
    (<>) :: forall m k n. (KnownNat m, KnownNat k, KnownNat n) => mat m k -> mat k n -> mat m n
    infixr 8 #>
    (#>) :: (KnownNat m, KnownNat n) => mat m n -> vec n -> vec m
    infixr 8 <·>
    (<·>) :: KnownNat n => vec n -> vec n -> field
    infixr 8 <.>
    (<.>) :: KnownNat n => vec n -> vec n -> field

    cross :: vec 3 -> vec 3 -> vec 3
    diag ::  forall n . KnownNat n => vec n -> mat n n
    diagR ::  forall m n k . (KnownNat m, KnownNat n, KnownNat k) => field -> vec k -> mat m n
    eye :: forall n. KnownNat n => mat n n
    dvmap :: forall n. KnownNat n => (field -> field) -> vec n -> vec n
    dmmap :: forall n m. (KnownNat m, KnownNat n) => (field -> field) -> mat n m -> mat n m
    outer :: forall n m. (KnownNat m, KnownNat n) => vec n -> vec m -> mat n m
    zipWithVector :: forall n. KnownNat n => (field -> field -> field) -> vec n -> vec n -> vec n
    zipWith3Vector :: forall n. KnownNat n => (field -> field -> field -> field) -> vec n -> vec n -> vec n -> vec n

    isKonst :: forall m n . (KnownNat m, KnownNat n) => mat m n -> Maybe (field,(Int,Int))
    isKonstV :: forall n . KnownNat n => vec n -> Maybe (field,Int)

    vector :: KnownNat n => [field] -> vec n
    matrix :: (KnownNat m, KnownNat n) => [field] -> mat m n
    split :: forall p n . (KnownNat p, KnownNat n, p<=n) => vec n -> (vec p, vec (n-p))
    splitRows :: forall p m n . (KnownNat p, KnownNat m, KnownNat n, p<=m) => mat m n -> (mat p n, mat (m-p) n)
    splitCols :: forall p m n. (KnownNat p, KnownNat m, KnownNat n, KnownNat (n-p), p<=n) => mat m n -> (mat m p, mat m (n-p))
    headTail :: (KnownNat n, 1<=n) => vec n -> (field, vec (n-1))

    toRows :: forall m n . (KnownNat m, KnownNat n) => mat m n -> [vec n]
    withRows :: forall n z . KnownNat n => [vec n] -> (forall m . KnownNat m => mat m n -> z) -> z
    toColumns :: forall m n . (KnownNat m, KnownNat n) => mat m n -> [vec m]
    withColumns :: forall m z . KnownNat m => [vec m] -> (forall n . KnownNat n => mat m n -> z) -> z

    det :: forall n. KnownNat n => mat n n -> field
    invlndet :: forall n. KnownNat n => mat n n -> (mat n n, (field, field))
    expm :: forall n. KnownNat n => mat n n -> mat n n
    sqrtm :: forall n. KnownNat n => mat n n -> mat n n
    inv :: forall n. KnownNat n => mat n n -> mat n n
    infixl 7 <\>
    (<\>) :: (KnownNat m, KnownNat n, KnownNat r) => mat m n -> mat m r -> mat n r
    linSolve :: (KnownNat m, KnownNat n) => mat m m -> mat m n -> Maybe (mat m n)

    build :: forall m n . (KnownNat n, KnownNat m) => (field -> field -> field) -> mat m n
    vec2 :: field -> field -> vec 2
    vec3 :: field -> field -> field -> vec 3
    vec4 :: field -> field -> field -> field -> vec 4
    row :: KnownNat n => vec n -> mat 1 n
    unrow :: KnownNat n => mat 1 n -> vec n
    col :: KnownNat n => vec n -> mat n 1
    uncol :: KnownNat n => mat n 1 -> vec n
    infixl 2 ===
    (===) :: (KnownNat r1, KnownNat r2, KnownNat c) => mat r1 c -> mat r2 c -> mat (r1+r2) c
    (|||) :: (KnownNat r, KnownNat c1, KnownNat c2, KnownNat (c1+c2)) => mat r c1 -> mat r c2 -> mat r (c1+c2)
    infixl 4 #
    (#) :: forall n m . (KnownNat n, KnownNat m) => vec n -> vec m -> vec (n+m)
    infixl 4 &
    (&) :: forall n . KnownNat n => vec n -> field -> vec (n+1)
    flatten :: (KnownNat m, KnownNat n, KnownNat (m * n)) => mat m n -> vec (m * n)
    reshape :: (KnownNat m, KnownNat n, KnownNat k, n ~ (k * m)) => vec n -> mat k m

    linspace :: forall n . KnownNat n => (field,field) -> vec n
    range :: forall n . KnownNat n => vec n
    dim :: forall n . KnownNat n => vec n
    mean :: (KnownNat n, 1<=n) => vec n -> field

    withVector :: forall z . Vector field -> (forall n . (KnownNat n) => vec n -> z) -> z
    exactLength :: forall n m . (KnownNat n, KnownNat m) => vec m -> Maybe (vec n)
    withMatrix :: forall z . Matrix field -> (forall m n . (KnownNat m, KnownNat n) => mat m n -> z) -> z
    exactDims :: forall n m j k . (KnownNat n, KnownNat m, KnownNat j, KnownNat k) => mat m n -> Maybe (mat j k)

    blockAt :: forall m n . (KnownNat m, KnownNat n) => field -> Int -> Int -> Matrix field -> mat m n

--------------------------------------------------------------------------------

instance Domain ℝ R L
  where
    mul = R.mul
    app = R.app
    dot = R.dot
    (<>) = mul
    (#>) = app
    (<·>) = dot
    (<.>) = dot

    cross = R.cross
    diag  = R.diag
    diagR = R.diagR
    eye   = R.eye
    dvmap = R.mapR
    dmmap = R.mapL
    outer = R.outer
    zipWithVector = R.zipWith
    zipWith3Vector = R.zipWith3

    isKonst = R.isKonst
    isKonstV = R.isKonstV

    vector = fromList
    matrix = fromList
    split = R.split
    splitRows = R.splitRows
    splitCols = R.splitCols
    headTail = R.headTail

    toRows = R.toRows
    withRows = R.withRows
    toColumns = R.toColumns
    withColumns  = R.withColumns

    det = R.det
    invlndet = R.invlndet
    expm = R.expm
    sqrtm = R.sqrtm
    inv = R.inv
    (<\>) = (R.<\>)
    linSolve = R.linSolve

    build = R.build
    vec2 = R.vec2
    vec3 = R.vec3
    vec4 = R.vec4
    row = R.row
    unrow = R.unrow
    col = R.col
    uncol = R.uncol
    (===) = (R.===)
    (|||) = (R.|||)
    (#) = (R.#)
    (&) = (R.&)
    flatten = R.flatten
    reshape = R.reshape

    linspace = R.linspace
    range = R.range
    dim = R.dim
    mean = R.mean

    withVector  = R.withVector
    exactLength = R.exactLength
    withMatrix = R.withMatrix
    exactDims = R.exactDims

    blockAt = R.blockAt

instance Domain ℂ C M
  where
    mul = C.mul
    app = C.app
    dot = C.dot
    (<>) = mul
    (#>) = app
    (<·>) = dot
    (<.>) = dot

    cross = C.cross
    diag  = C.diag
    diagR = C.diagR
    eye   = C.eye
    dvmap = C.mapC
    dmmap = C.mapM
    outer = C.outer
    zipWithVector = C.zipWith
    zipWith3Vector = C.zipWith3

    isKonst = C.isKonst
    isKonstV = C.isKonstV

    vector = fromList
    matrix = fromList
    split = C.split
    splitRows = C.splitRows
    splitCols = C.splitCols
    headTail = C.headTail

    toRows = C.toRows
    withRows = C.withRows
    toColumns = C.toColumns
    withColumns  = C.withColumns

    det = C.det
    invlndet = C.invlndet
    expm = C.expm
    sqrtm = C.sqrtm
    inv = C.inv
    (<\>) = (C.<\>)
    linSolve = C.linSolve

    build = C.build
    vec2 = C.vec2
    vec3 = C.vec3
    vec4 = C.vec4
    row = C.row
    unrow = C.unrow
    col = C.col
    uncol = C.uncol
    (===) = (C.===)
    (|||) = (C.|||)
    (#) = (C.#)
    (&) = (C.&)
    flatten = C.flatten
    reshape = C.reshape

    linspace = C.linspace
    range = C.range
    dim = C.dim
    mean = C.mean

    withVector  = C.withVector
    exactLength = C.exactLength
    withMatrix = C.withMatrix
    exactDims = C.exactDims

    blockAt = C.blockAt

--------------------------------------------------------------------------------

type Sq n  = L n n


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


class FromComplex r c | r -> c, c -> r where
  fromComplex :: c -> (r, r)
  real :: c -> r
  imag :: c -> r
  sqMagnitude :: c -> r
  magnitude :: c -> r

class ToComplex r c | r -> c, c -> r where
  toComplex :: (r, r) -> c
  complex :: r -> c

instance KnownNat n => FromComplex (R n) (C n) where
  fromComplex (C (Dim v)) = let (r,i) = LA.fromComplex v in (mkR r, mkR i)
  real = fst . fromComplex
  imag = snd . fromComplex
  sqMagnitude c = let (r,i) = fromComplex c in r**2 + i**2
  magnitude = sqrt . sqMagnitude

instance KnownNat n => ToComplex (R n) (C n) where
  toComplex (r,i) = mkC $ LA.toComplex (extract r, extract i)
  complex r = mkC $ LA.toComplex (extract r, LA.konst 0 (size r))

instance (KnownNat m, KnownNat n) => FromComplex (L m n) (M m n) where
  fromComplex m = let (r,i) = LA.fromComplex (extract m) in (mkL r, mkL i)
  real = fst . fromComplex
  imag = snd . fromComplex
  sqMagnitude c = let (r,i) = fromComplex c in r**2 + i**2
  magnitude = sqrt . sqMagnitude

instance (KnownNat m, KnownNat n) => ToComplex (L m n) (M m n) where
  toComplex (r,i) = mkM $ LA.toComplex (extract r, extract i)
  complex r = mkM $ LA.toComplex (extract r, LA.konst 0 (size r))


--------------------------------------------------------------------------------

svd :: (KnownNat m, KnownNat n) => L m n -> (L m m, R n, L n n)
svd (extract -> m) = (mkL u, mkR s', mkL v)
  where
    (u,s,v) = LA.svd m
    s' = vjoin [s, z]
    z = LA.konst 0 (max 0 (cols m - LA.size s))


svdTall :: (KnownNat m, KnownNat n, n <= m) => L m n -> (L m n, R n, L n n)
svdTall (extract -> m) = (mkL u, mkR s, mkL v)
  where
    (u,s,v) = LA.thinSVD m


svdFlat :: (KnownNat m, KnownNat n, m <= n) => L m n -> (L m m, R m, L n m)
svdFlat (extract -> m) = (mkL u, mkR s, mkL v)
  where
    (u,s,v) = LA.thinSVD m

--------------------------------------------------------------------------------

randomVector
    :: forall n . KnownNat n
    => Seed
    -> RandDist
    -> R n
randomVector s d = mkR (LA.randomVector s d
                          (fromInteger (natVal (Proxy :: Proxy n)))
                       )

rand
    :: forall m n . (KnownNat m, KnownNat n)
    => IO (L m n)
rand = mkL <$> LA.rand (fromInteger (natVal (Proxy :: Proxy m)))
                       (fromInteger (natVal (Proxy :: Proxy n)))

randn
    :: forall m n . (KnownNat m, KnownNat n)
    => IO (L m n)
randn = mkL <$> LA.randn (fromInteger (natVal (Proxy :: Proxy m)))
                         (fromInteger (natVal (Proxy :: Proxy n)))

gaussianSample
    :: forall m n . (KnownNat m, KnownNat n)
    => Seed
    -> R n
    -> Sym n
    -> L m n
gaussianSample s (extract -> mu) (Sym (extract -> sigma)) =
    mkL $ LA.gaussianSample s (fromInteger (natVal (Proxy :: Proxy m)))
                            mu (LA.trustSym sigma)

uniformSample
    :: forall m n . (KnownNat m, KnownNat n)
    => Seed
    -> R n    -- ^ minimums of each row
    -> R n    -- ^ maximums of each row
    -> L m n
uniformSample s (extract -> mins) (extract -> maxs) =
    mkL $ LA.uniformSample s (fromInteger (natVal (Proxy :: Proxy m)))
                           (zip (LA.toList mins) (LA.toList maxs))

--------------------------------------------------------------------------------

class Eigen m l v | m -> l, m -> v
  where
    eigensystem :: m -> (l,v)
    eigenvalues :: m -> l

instance KnownNat n => Eigen (Sym n) (R n) (L n n)
  where
    eigenvalues (Sym (extract -> m)) =  mkR . LA.eigenvaluesSH . LA.trustSym $ m
    eigensystem (Sym (extract -> m)) = (mkR l, mkL v)
      where
        (l,v) = LA.eigSH . LA.trustSym $ m

instance KnownNat n => Eigen (Sq n) (C n) (M n n)
  where
    eigenvalues (extract -> m) = mkC . LA.eigenvalues $ m
    eigensystem (extract -> m) = (mkC l, mkM v)
      where
        (l,v) = LA.eig m

instance KnownNat n => Eigen (M n n) (C n) (M n n)
  where
    eigenvalues (extract -> m) = mkC . LA.eigenvalues $ m
    eigensystem (extract -> m) = (mkC l, mkM v)
      where
        (l,v) = LA.eig m

--------------------------------------------------------------------------------

instance KnownNat n => Normed (R n)
  where
    norm_0 v = norm_0 (extract v)
    norm_1 v = norm_1 (extract v)
    norm_2 v = norm_2 (extract v)
    norm_Inf v = norm_Inf (extract v)

instance KnownNat n => Normed (C n)
  where
    norm_0 v = norm_0 (extract v)
    norm_1 v = norm_1 (extract v)
    norm_2 v = norm_2 (extract v)
    norm_Inf v = norm_Inf (extract v)

instance (KnownNat m, KnownNat n) => Normed (L m n)
  where
    norm_0 m = norm_0 (extract m)
    norm_1 m = norm_1 (extract m)
    norm_2 m = norm_2 (extract m)
    norm_Inf m = norm_Inf (extract m)

instance (KnownNat m, KnownNat n) => Normed (M m n)
  where
    norm_0 m = norm_0 (extract m)
    norm_1 m = norm_1 (extract m)
    norm_2 m = norm_2 (extract m)
    norm_Inf m = norm_Inf (extract m)

--------------------------------------------------------------------------------


sym :: KnownNat n => Sq n -> Sym n
sym m = Sym $ (m + tr m)/2

mTm :: (KnownNat m, KnownNat n) => L m n -> Sym n
mTm x = Sym (tr x <> x)

unSym :: Sym n -> Sq n
unSym (Sym x) = x



newtype Sym n = Sym (Sq n) deriving Show

instance (KnownNat n) => Disp (Sym n)
  where
    disp n (Sym x) = do
        let a = extract x
        let su = LA.dispf n a
        printf "Sym %d" (cols a) >> putStr (dropWhile (/='\n') $ su)



mkSym f = Sym . f . unSym
mkSym2 f x y = Sym (f (unSym x) (unSym y))

instance KnownNat n =>  Num (Sym n)
  where
    (+) = mkSym2 (+)
    (*) = mkSym2 (*)
    (-) = mkSym2 (-)
    abs = mkSym abs
    signum = mkSym signum
    negate = mkSym negate
    fromInteger = Sym . fromInteger

instance KnownNat n => Fractional (Sym n)
  where
    fromRational = Sym . fromRational
    (/) = mkSym2 (/)

instance KnownNat n => Floating (Sym n)
  where
    sin   = mkSym sin
    cos   = mkSym cos
    tan   = mkSym tan
    asin  = mkSym asin
    acos  = mkSym acos
    atan  = mkSym atan
    sinh  = mkSym sinh
    cosh  = mkSym cosh
    tanh  = mkSym tanh
    asinh = mkSym asinh
    acosh = mkSym acosh
    atanh = mkSym atanh
    exp   = mkSym exp
    log   = mkSym log
    sqrt  = mkSym sqrt
    (**)  = mkSym2 (**)
    pi    = Sym pi

instance KnownNat n => Additive (Sym n) where
    add = (+)

instance KnownNat n => Transposable (Sym n) (Sym n) where
    tr  = id
    tr' = id


chol :: KnownNat n => Sym n -> Sq n
chol (extract . unSym -> m) = mkL $ LA.chol $ LA.trustSym m

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


--------------------------------------------------------------------------------

𝑖 :: Sized ℂ s c => s
𝑖 = konst iC

newtype Her n = Her (M n n)


her :: KnownNat n => M n n -> Her n
her m = Her $ (m + LA.tr m)/2

instance KnownNat n => Transposable (Her n) (Her n) where
    tr          = id
    tr' (Her m) = Her (tr' m)

instance (KnownNat n) => Disp (Her n)
  where
    disp n (Her x) = do
        let a = extract x
        let su = LA.dispcf n a
        printf "Her %d" (cols a) >> putStr (dropWhile (/='\n') $ su)


--------------------------------------------------------------------------------

class ElementAccess c i e where
  (!) :: c -> i -> e

data Coord (n :: Nat) = Coord

instance (KnownNat n, KnownNat i, i+1<=n) => ElementAccess (R n) (Coord i) Double where
  v ! _ =  extract v LA.! pos
    where pos = fromIntegral . natVal $ (undefined :: Proxy i)

instance (KnownNat m, KnownNat n, KnownNat i, KnownNat j, i+1<=m, j+1<=n)
  => ElementAccess (L m n) (Coord i, Coord j) Double where
  m ! _ =  extract m `LA.atIndex` pos
    where pos = ( fromIntegral . natVal $ (undefined :: Proxy i)
                , fromIntegral . natVal $ (undefined :: Proxy j))

instance (KnownNat n, KnownNat i, i+1<=n) => ElementAccess (C n) (Coord i) (Complex Double) where
  v ! _ =  extract v LA.! pos
    where pos = fromIntegral . natVal $ (undefined :: Proxy i)

instance (KnownNat m, KnownNat n, KnownNat i, KnownNat j, i+1<=m, j+1<=n)
  => ElementAccess (M m n) (Coord i, Coord j) (Complex Double) where
  m ! _ =  extract m `LA.atIndex` pos
    where pos = ( fromIntegral . natVal $ (undefined :: Proxy i)
                , fromIntegral . natVal $ (undefined :: Proxy j))


--------------------------------------------------------------------------------
-- Existential Wrappers


data SomeR = forall n. KnownNat n => SomeR (R n)

toSomeR :: Vector ℝ -> SomeR
toSomeR v = case someNatVal $ fromIntegral $ LA.size v of
       Nothing -> error "static/dynamic mismatch"
       Just (SomeNat (_ :: Proxy m)) -> SomeR (mkR v :: R m)

toContR :: forall z. Vector ℝ -> Cont z SomeR
toContR v = return $ toSomeR v


data SomeC = forall n. KnownNat n => SomeC (C n)

toSomeC :: Vector ℂ -> SomeC
toSomeC v = case someNatVal $ fromIntegral $ LA.size v of
       Nothing -> error "static/dynamic mismatch"
       Just (SomeNat (_ :: Proxy m)) -> SomeC (mkC v :: C m)

toContC :: forall z. Vector ℂ -> Cont z SomeC
toContC v = return $ toSomeC v


data SomeL = forall m n. (KnownNat m, KnownNat n)  => SomeL (L m n)

toSomeL :: Matrix ℝ -> SomeL
toSomeL a = case (dr, dc) of
  (Just (SomeNat (_ :: Proxy m)), Just (SomeNat (_ :: Proxy n))) -> SomeL (mkL a :: L m n)
  _ -> error "static/dynamic mismatch"
  where dr = someNatVal $ fromIntegral $ rows a
        dc = someNatVal $ fromIntegral $ cols a

toContL :: forall z . Matrix ℝ -> Cont z SomeL
toContL a = return $ toSomeL a


data SomeM = forall m n. (KnownNat m, KnownNat n)  => SomeM (M m n)

toSomeM :: Matrix ℂ -> SomeM
toSomeM a = case (dr, dc) of
  (Just (SomeNat (_ :: Proxy m)), Just (SomeNat (_ :: Proxy n))) -> SomeM (mkM a :: M m n)
  _ -> error "static/dynamic mismatch"
  where dr = someNatVal $ fromIntegral $ rows a
        dc = someNatVal $ fromIntegral $ cols a

toContM :: forall z . Matrix ℂ -> Cont z SomeM
toContM a = return $ toSomeM a

--------------------------------------------------------------------------------




-- type GL = forall n m . (KnownNat n, KnownNat m) => L m n
-- type GSq = forall n . KnownNat n => Sq n

-- test :: (Bool, IO ())
-- test = (ok,info)
--   where
--     ok =   extract (eye :: Sq 5) == ident 5
--            && (unwrap .unSym) (mTm sm :: Sym 3) == tr ((3><3)[1..]) LA.<> (3><3)[1..]
--            && unwrap (tm :: L 3 5) == LA.matrix 5 [1..15]
--            && thingS == thingD
--            && precS == precD
--            && withVector (LA.vector [1..15]) sumV == sumElements (LA.fromList [1..15])

--     info = do
--         print $ u
--         print $ v
--         print (eye :: Sq 3)
--         print $ ((u & 5) + 1) <·> v
--         print (tm :: L 2 5)
--         print (tm <> sm :: L 2 3)
--         print thingS
--         print thingD
--         print precS
--         print precD
--         print $ withVector (LA.vector [1..15]) sumV
--         splittest

--     sumV w = w <·> konst 1

--     u = vec2 3 5

--     𝕧 x = vector [x] :: R 1

--     v = 𝕧 2 & 4 & 7

--     tm :: GL
--     tm = lmat 0 [1..]

--     lmat :: forall m n . (KnownNat m, KnownNat n) => ℝ -> [ℝ] -> L m n
--     lmat z xs = r
--       where
--         r = mkL . reshape n' . LA.fromList . take (m'*n') $ xs ++ repeat z
--         (m',n') = size r

--     sm :: GSq
--     sm = lmat 0 [1..]

--     thingS = (u & 1) <·> tr q #> q #> v
--       where
--         q = tm :: L 10 3

--     thingD = vjoin [ud1 u, 1] LA.<.> tr m LA.#> m LA.#> ud1 v
--       where
--         m = LA.matrix 3 [1..30]

--     precS = (1::Double) + (2::Double) * ((1 :: R 3) * (u & 6)) <·> konst 2 #> v
--     precD = 1 + 2 * vjoin[ud1 u, 6] LA.<.> LA.konst 2 (LA.size (ud1 u) +1, LA.size (ud1 v)) LA.#> ud1 v


-- splittest
--     = do
--     let v = range :: R 7
--         a = snd (split v) :: R 4
--     print $ a
--     print $ snd . headTail . snd . headTail $ v
--     print $ first (vec3 1 2 3)
--     print $ second (vec3 1 2 3)
--     print $ third (vec3 1 2 3)
--     print $ (snd $ splitRows eye :: L 4 6)
--  where
--     first v = fst . headTail $ v
--     second v = first . snd . headTail $ v
--     third v = first . snd . headTail . snd . headTail $ v
