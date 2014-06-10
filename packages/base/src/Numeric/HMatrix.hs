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
{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE TypeFamilies #-}


{- |
Module      :  Numeric.HMatrix
Copyright   :  (c) Alberto Ruiz 2006-14
License     :  BSD3
Stability   :  experimental

Experimental interface for real arrays with statically checked dimensions.

-}

module Numeric.HMatrix(
    -- * Vector
    R,
    vec2, vec3, vec4, (&), (#), split, headTail,
    vector,
    linspace, range, dim,
    -- * Matrix
    L, Sq, build,
    row, col, (¦),(——), splitRows, splitCols,
    unrow, uncol,

    eye,
    diagR, diag,
    blockAt,
    matrix,
    -- * Products
    (<>),(#>),(<·>),
    -- * Linear Systems
    linSolve, (<\>),
    -- * Factorizations
    svd, svdTall, svdFlat, Eigen(..),
    withNullspace,
    -- * Misc
    Disp(..),
    withVector, withMatrix,
    toRows, toColumns,
    Sized(..), Diag(..), Sym, sym,
    module Numeric.LinearAlgebra.HMatrix
) where


import GHC.TypeLits
import Numeric.LinearAlgebra.HMatrix hiding (
    (<>),(#>),(<·>),Konst(..),diag, disp,(¦),(——),row,col,vector,matrix,linspace,toRows,toColumns,
    (<\>),fromList,takeDiag,svd,eig,eigSH,eigSH',eigenvalues,eigenvaluesSH,eigenvaluesSH',build)
import qualified Numeric.LinearAlgebra.HMatrix as LA
import Data.Proxy(Proxy)
import Numeric.LinearAlgebra.Static
import Control.Arrow((***))





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
linspace (a,b) = mkR (LA.linspace d (a,b))
  where
    d = fromIntegral . natVal $ (undefined :: Proxy n)

range :: forall n . KnownNat n => R n
range = mkR (LA.linspace d (1,fromIntegral d))
  where
    d = fromIntegral . natVal $ (undefined :: Proxy n)

dim :: forall n . KnownNat n => R n
dim = mkR (scalar d)
  where
    d = fromIntegral . natVal $ (undefined :: Proxy n)


--------------------------------------------------------------------------------


ud2 :: L m n -> Matrix ℝ
ud2 (L (Dim (Dim x))) = x


--------------------------------------------------------------------------------
--------------------------------------------------------------------------------

diagR :: forall m n k . (KnownNat m, KnownNat n, KnownNat k) => ℝ -> R k -> L m n
diagR x v = mkL (asRow (vjoin [scalar x, ev, zeros]))
  where
    ev = extract v
    zeros = LA.konst x (max 0 ((min m' n') - size ev))
    m' = fromIntegral . natVal $ (undefined :: Proxy m)
    n' = fromIntegral . natVal $ (undefined :: Proxy n)

diag :: KnownNat n => R n -> Sq n
diag = diagR 0

eye :: KnownNat n => Sq n
eye = diag 1

--------------------------------------------------------------------------------

blockAt :: forall m n . (KnownNat m, KnownNat n) =>  ℝ -> Int -> Int -> Matrix Double -> L m n
blockAt x r c a = mkL res
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





--------------------------------------------------------------------------------


row :: R n -> L 1 n
row = mkL . asRow . ud1

--col :: R n -> L n 1
col v = tr . row $ v

unrow :: L 1 n -> R n
unrow = mkR . head . LA.toRows . ud2

--uncol :: L n 1 -> R n
uncol v = unrow . tr $ v


infixl 2 ——
(——) :: (KnownNat r1, KnownNat r2, KnownNat c) => L r1 c -> L r2 c -> L (r1+r2) c
a —— b = mkL (extract a LA.—— extract b)


infixl 3 ¦
-- (¦) :: (KnownNat r, KnownNat c1, KnownNat c2) => L r c1 -> L r c2 -> L r (c1+c2)
a ¦ b = tr (tr a —— tr b)


type Sq n  = L n n
--type CSq n = CL n n

type GL = (KnownNat n, KnownNat m) => L m n
type GSq = KnownNat n => Sq n

isKonst :: forall m n . (KnownNat m, KnownNat n) => L m n -> Maybe (ℝ,(Int,Int))
isKonst (unwrap -> x)
    | singleM x = Just (x `atIndex` (0,0), (m',n'))
    | otherwise = Nothing
  where
    m' = fromIntegral . natVal $ (undefined :: Proxy m) :: Int
    n' = fromIntegral . natVal $ (undefined :: Proxy n) :: Int




infixr 8 <>
(<>) :: forall m k n. (KnownNat m, KnownNat k, KnownNat n) => L m k -> L k n -> L m n

(isKonst -> Just (a,(_,k))) <> (isKonst -> Just (b,_)) = konst (a * b * fromIntegral k)

(isDiag -> Just (0,a,_)) <> (isDiag -> Just (0,b,_)) = diagR 0 (mkR v :: R k)
  where
    v = a' * b'
    n = min (size a) (size b)
    a' = subVector 0 n a
    b' = subVector 0 n b

(isDiag -> Just (0,a,_)) <> (extract -> b) = mkL (asColumn a * takeRows (size a) b)

(extract -> a) <> (isDiag -> Just (0,b,_)) = mkL (takeColumns (size b) a * asRow b)

a <> b = mkL (extract a LA.<> extract b)

infixr 8 #>
(#>) :: (KnownNat m, KnownNat n) => L m n -> R n -> R m
(isDiag -> Just (0, w, _)) #> v = mkR (w * subVector 0 (size w) (extract v))
m #> v = mkR (extract m LA.#> extract v)


infixr 8 <·>
(<·>) :: R n -> R n -> ℝ
(ud1 -> u) <·> (ud1 -> v)
    | singleV u || singleV v = sumElements (u * v)
    | otherwise = udot u v

--------------------------------------------------------------------------------

{-
class Minim (n :: Nat) (m :: Nat)
  where
    type Mini n m :: Nat

instance forall (n :: Nat) . Minim n n
  where
    type Mini n n = n


instance forall (n :: Nat) (m :: Nat) . (n <= m+1) => Minim n m
  where
    type Mini n m = n

instance forall (n :: Nat) (m :: Nat) . (m <= n+1) => Minim n m
  where
    type Mini n m = m
-}

class Diag m d | m -> d
  where
    takeDiag :: m -> d



instance forall n . (KnownNat n) => Diag (L n n) (R n)
  where
    takeDiag m = mkR (LA.takeDiag (extract m))


instance forall m n . (KnownNat m, KnownNat n, m <= n+1) => Diag (L m n) (R m)
  where
    takeDiag m = mkR (LA.takeDiag (extract m))


instance forall m n . (KnownNat m, KnownNat n, n <= m+1) => Diag (L m n) (R n)
  where
    takeDiag m = mkR (LA.takeDiag (extract m))


--------------------------------------------------------------------------------

linSolve :: (KnownNat m, KnownNat n) => L m m -> L m n -> Maybe (L m n)
linSolve (extract -> a) (extract -> b) = fmap mkL (LA.linearSolve a b)

(<\>) :: (KnownNat m, KnownNat n, KnownNat r) => L m n -> L m r -> L n r
(extract -> a) <\> (extract -> b) = mkL (a LA.<\> b)

svd :: (KnownNat m, KnownNat n) => L m n -> (L m m, R n, L n n)
svd (extract -> m) = (mkL u, mkR s', mkL v)
  where
    (u,s,v) = LA.svd m
    s' = vjoin [s, z]
    z = LA.konst 0 (max 0 (cols m - size s))


svdTall :: (KnownNat m, KnownNat n, n <= m) => L m n -> (L m n, R n, L n n)
svdTall (extract -> m) = (mkL u, mkR s, mkL v)
  where
    (u,s,v) = LA.thinSVD m


svdFlat :: (KnownNat m, KnownNat n, m <= n) => L m n -> (L m m, R m, L n m)
svdFlat (extract -> m) = (mkL u, mkR s, mkL v)
  where
    (u,s,v) = LA.thinSVD m

--------------------------------------------------------------------------------

class Eigen m l v | m -> l, m -> v
  where
    eigensystem :: m -> (l,v)
    eigenvalues :: m -> l

newtype Sym n = Sym (Sq n) deriving Show


sym :: KnownNat n => Sq n -> Sym n
sym m = Sym $ (m + tr m)/2



instance KnownNat n => Eigen (Sym n) (R n) (L n n)
  where
    eigenvalues (Sym (extract -> m)) =  mkR . LA.eigenvaluesSH' $ m
    eigensystem (Sym (extract -> m)) = (mkR l, mkL v)
      where
        (l,v) = LA.eigSH' m

instance KnownNat n => Eigen (Sq n) (C n) (M n n)
  where
    eigenvalues (extract -> m) = mkC . LA.eigenvalues $ m
    eigensystem (extract -> m) = (mkC l, mkM v)
      where
        (l,v) = LA.eig m

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

--------------------------------------------------------------------------------

split :: forall p n . (KnownNat p, KnownNat n, p<=n) => R n -> (R p, R (n-p))
split (extract -> v) = ( mkR (subVector 0 p' v) ,
                         mkR (subVector p' (size v - p') v) )
  where
    p' = fromIntegral . natVal $ (undefined :: Proxy p) :: Int


headTail :: (KnownNat n, 1<=n) => R n -> (ℝ, R (n-1))
headTail = ((!0) . extract *** id) . split


splitRows :: forall p m n . (KnownNat p, KnownNat m, KnownNat n, p<=m) => L m n -> (L p n, L (m-p) n)
splitRows (extract -> x) = ( mkL (takeRows p' x) ,
                             mkL (dropRows p' x) )
  where
    p' = fromIntegral . natVal $ (undefined :: Proxy p) :: Int

splitCols :: forall p m n. (KnownNat p, KnownNat m, KnownNat n, KnownNat (n-p), p<=n) => L m n -> (L m p, L m (n-p))
splitCols = (tr *** tr) . splitRows . tr


toRows :: forall m n . (KnownNat m, KnownNat n) => L m n -> [R n]
toRows (LA.toRows . extract -> vs) = map mkR vs


toColumns :: forall m n . (KnownNat m, KnownNat n) => L m n -> [R m]
toColumns (LA.toColumns . extract -> vs) = map mkR vs


splittest
    = do
    let v = range :: R 7
        a = snd (split v) :: R 4
    print $ a
    print $ snd . headTail . snd . headTail $ v
    print $ first (vec3 1 2 3)
    print $ second (vec3 1 2 3)
    print $ third (vec3 1 2 3)
    print $ (snd $ splitRows eye :: L 4 6)
 where
    first v = fst . headTail $ v
    second v = first . snd . headTail $ v
    third v = first . snd . headTail . snd . headTail $ v

--------------------------------------------------------------------------------

build
  :: forall m n . (KnownNat n, KnownNat m)
    => (ℝ -> ℝ -> ℝ)
    -> L m n
build f = mkL $ LA.build (m',n') f
  where
    m' = fromIntegral . natVal $ (undefined :: Proxy m) :: Int
    n' = fromIntegral . natVal $ (undefined :: Proxy n) :: Int

--------------------------------------------------------------------------------

withVector
    :: forall z
     . Vector ℝ
    -> (forall n . (KnownNat n) => R n -> z)
    -> z
withVector v f =
    case someNatVal $ fromIntegral $ size v of
       Nothing -> error "static/dynamic mismatch"
       Just (SomeNat (_ :: Proxy m)) -> f (mkR v :: R m)


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


--------------------------------------------------------------------------------

test :: (Bool, IO ())
test = (ok,info)
  where
    ok =   extract (eye :: Sq 5) == ident 5
           && unwrap (mTm sm :: Sq 3) == tr ((3><3)[1..]) LA.<> (3><3)[1..]
           && unwrap (tm :: L 3 5) == LA.matrix 5 [1..15]
           && thingS == thingD
           && precS == precD
           && withVector (LA.vector [1..15]) sumV == sumElements (LA.fromList [1..15])

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
        print $ withVector (LA.vector [1..15]) sumV
        splittest

    sumV w = w <·> konst 1

    u = vec2 3 5

    𝕧 x = vector [x] :: R 1

    v = 𝕧 2 & 4 & 7

--    mTm :: L n m -> Sq m
    mTm a = tr a <> a

    tm :: GL
    tm = lmat 0 [1..]

    lmat :: forall m n . (KnownNat m, KnownNat n) => ℝ -> [ℝ] -> L m n
    lmat z xs = mkL . reshape n' . LA.fromList . take (m'*n') $ xs ++ repeat z
      where
        m' = fromIntegral . natVal $ (undefined :: Proxy m)
        n' = fromIntegral . natVal $ (undefined :: Proxy n)

    sm :: GSq
    sm = lmat 0 [1..]

    thingS = (u & 1) <·> tr q #> q #> v
      where
        q = tm :: L 10 3

    thingD = vjoin [ud1 u, 1] LA.<·> tr m LA.#> m LA.#> ud1 v
      where
        m = LA.matrix 3 [1..30]

    precS = (1::Double) + (2::Double) * ((1 :: R 3) * (u & 6)) <·> konst 2 #> v
    precD = 1 + 2 * vjoin[ud1 u, 6] LA.<·> LA.konst 2 (size (ud1 u) +1, size (ud1 v)) LA.#> ud1 v


instance (KnownNat n', KnownNat m') => Testable (L n' m')
  where
    checkT _ = test


