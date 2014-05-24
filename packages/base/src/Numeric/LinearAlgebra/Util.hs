{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}

-----------------------------------------------------------------------------
{- |
Module      :  Numeric.LinearAlgebra.Util
Copyright   :  (c) Alberto Ruiz 2013
License     :  BSD3
Maintainer  :  Alberto Ruiz
Stability   :  provisional

-}
-----------------------------------------------------------------------------
{-# OPTIONS_HADDOCK hide #-}

module Numeric.LinearAlgebra.Util(

    -- * Convenience functions
    vect, mat,
    disp,
    zeros, ones,
    diagl,
    row,
    col,
    (&), (¦), (——), (#),
    (?), (¿),
    Indexable(..), size,
    Numeric,
    rand, randn,
    cross,
    norm,
    ℕ,ℤ,ℝ,ℂ,ℝn,ℂn,𝑖,i_C, --ℍ
    norm_1, norm_2, norm_0, norm_Inf, norm_Frob, norm_nuclear,
    mnorm_1, mnorm_2, mnorm_0, mnorm_Inf,
    unitary,
    mt,
    pairwiseD2,
    rowOuters,
    null1,
    null1sym,
    -- * Convolution
    -- ** 1D
    corr, conv, corrMin,
    -- ** 2D
    corr2, conv2, separable,
    -- * Tools for the Kronecker product
    --
    -- | (see A. Fusiello, A matter of notation: Several uses of the Kronecker product in
    --  3d computer vision, Pattern Recognition Letters 28 (15) (2007) 2127-2132)

    --
    -- | @`vec` (a \<> x \<> b) == ('trans' b ` 'kronecker' ` a) \<> 'vec' x@
    vec,
    vech,
    dup,
    vtrans
) where

import Data.Packed.Numeric
import Numeric.LinearAlgebra.Algorithms hiding (i)
import Numeric.Matrix()
import Numeric.Vector()
import Numeric.LinearAlgebra.Random
import Numeric.LinearAlgebra.Util.Convolution

type ℝ = Double
type ℕ = Int
type ℤ = Int
type ℂ = Complex Double
type ℝn = Vector ℝ
type ℂn = Vector ℂ
--newtype ℍ m = H m

i_C, 𝑖 :: ℂ
𝑖 = 0:+1
i_C = 𝑖

{- | create a real vector

>>> vect [1..5]
fromList [1.0,2.0,3.0,4.0,5.0]

-}
vect :: [ℝ] -> ℝn
vect = fromList

{- | create a real matrix

>>> mat 5 [1..15]
(3><5)
 [  1.0,  2.0,  3.0,  4.0,  5.0
 ,  6.0,  7.0,  8.0,  9.0, 10.0
 , 11.0, 12.0, 13.0, 14.0, 15.0 ]

-}
mat
  :: Int -- ^ columns
  -> [ℝ] -- ^ elements
  -> Matrix ℝ
mat c = reshape c . fromList



class ( Container Vector t
      , Container Matrix t
      , Konst t Int Vector
      , Konst t (Int,Int) Matrix
      ) => Numeric t

instance Numeric Double
instance Numeric (Complex Double)



{- | print a real matrix with given number of digits after the decimal point

>>> disp 5 $ ident 2 / 3
2x2
0.33333  0.00000
0.00000  0.33333

-}
disp :: Int -> Matrix Double -> IO ()

disp n = putStrLn . dispf n


{- | create a real diagonal matrix from a list

>>> diagl [1,2,3]
(3><3)
 [ 1.0, 0.0, 0.0
 , 0.0, 2.0, 0.0
 , 0.0, 0.0, 3.0 ]

-}
diagl :: [Double] -> Matrix Double
diagl = diag . fromList

-- | a real matrix of zeros
zeros :: Int -- ^ rows
      -> Int -- ^ columns
      -> Matrix Double
zeros r c = konst 0 (r,c)

-- | a real matrix of ones
ones :: Int -- ^ rows
     -> Int -- ^ columns
     -> Matrix Double
ones r c = konst 1 (r,c)

-- | concatenation of real vectors
infixl 3 &
(&) :: Vector Double -> Vector Double -> Vector Double
a & b = vjoin [a,b]

{- | horizontal concatenation of real matrices

 (unicode 0x00a6, broken bar)

>>> ident 3 ¦ konst 7 (3,4)
(3><7)
 [ 1.0, 0.0, 0.0, 7.0, 7.0, 7.0, 7.0
 , 0.0, 1.0, 0.0, 7.0, 7.0, 7.0, 7.0
 , 0.0, 0.0, 1.0, 7.0, 7.0, 7.0, 7.0 ]

-}
infixl 3 ¦
(¦) :: Matrix Double -> Matrix Double -> Matrix Double
a ¦ b = fromBlocks [[a,b]]

-- | vertical concatenation of real matrices
--
-- (unicode 0x2014, em dash)
(——) :: Matrix Double -> Matrix Double -> Matrix Double
infixl 2 ——
a —— b = fromBlocks [[a],[b]]

(#) :: Matrix Double -> Matrix Double -> Matrix Double
infixl 2 #
a # b = fromBlocks [[a],[b]]

-- | create a single row real matrix from a list
row :: [Double] -> Matrix Double
row = asRow . fromList

-- | create a single column real matrix from a list
col :: [Double] -> Matrix Double
col = asColumn . fromList

{- | extract rows

>>> (20><4) [1..] ? [2,1,1]
(3><4)
 [ 9.0, 10.0, 11.0, 12.0
 , 5.0,  6.0,  7.0,  8.0
 , 5.0,  6.0,  7.0,  8.0 ]

-}
infixl 9 ?
(?) :: Element t => Matrix t -> [Int] -> Matrix t
(?) = flip extractRows

{- | extract columns

(unicode 0x00bf, inverted question mark, Alt-Gr ?)

>>> (3><4) [1..] ¿ [3,0]
(3><2)
 [  4.0, 1.0
 ,  8.0, 5.0
 , 12.0, 9.0 ]

-}
infixl 9 ¿
(¿) :: Element t => Matrix t -> [Int] -> Matrix t
(¿)= flip extractColumns


cross :: Vector Double -> Vector Double -> Vector Double
-- ^ cross product (for three-element real vectors)
cross x y | dim x == 3 && dim y == 3 = fromList [z1,z2,z3]
          | otherwise = error $ "cross ("++show x++") ("++show y++")"
  where
    [x1,x2,x3] = toList x
    [y1,y2,y3] = toList y
    z1 = x2*y3-x3*y2
    z2 = x3*y1-x1*y3
    z3 = x1*y2-x2*y1

norm :: Vector Double -> Double
-- ^ 2-norm of real vector
norm = pnorm PNorm2

norm_2 :: Normed Vector t => Vector t -> RealOf t
norm_2 = pnorm PNorm2

norm_1 :: Normed Vector t => Vector t -> RealOf t
norm_1 = pnorm PNorm1

norm_Inf :: Normed Vector t => Vector t -> RealOf t
norm_Inf = pnorm Infinity

norm_0 :: Vector ℝ -> ℝ
norm_0 v = sumElements (step (abs v - scalar (eps*mx)))
  where
    mx = norm_Inf v

norm_Frob :: Normed Matrix t => Matrix t -> RealOf t
norm_Frob = pnorm Frobenius

norm_nuclear :: Field t => Matrix t -> ℝ
norm_nuclear = sumElements . singularValues

mnorm_2 :: Normed Matrix t => Matrix t -> RealOf t
mnorm_2 = pnorm PNorm2

mnorm_1 :: Normed Matrix t => Matrix t -> RealOf t
mnorm_1 = pnorm PNorm1

mnorm_Inf :: Normed Matrix t => Matrix t -> RealOf t
mnorm_Inf = pnorm Infinity

mnorm_0 :: Matrix ℝ -> ℝ
mnorm_0 = norm_0 . flatten

-- | Obtains a vector in the same direction with 2-norm=1
unitary :: Vector Double -> Vector Double
unitary v = v / scalar (norm v)


-- | trans . inv
mt :: Matrix Double -> Matrix Double
mt = trans . inv

--------------------------------------------------------------------------------
{- |

>>> size $ fromList[1..10::Double]
10
>>> size $ (2><5)[1..10::Double]
(2,5)

-}
size :: Container c t => c t -> IndexOf c
size = size'

{- |

>>> vect [1..10] ! 3
4.0

>>> mat 5 [1..15] ! 1
fromList [6.0,7.0,8.0,9.0,10.0]

>>> mat 5 [1..15] ! 1 ! 3
9.0

-}
class Indexable c t | c -> t , t -> c
  where
    infixl 9 !
    (!) :: c -> Int -> t

instance Indexable (Vector Double) Double
  where
    (!) = (@>)

instance Indexable (Vector Float) Float
  where
    (!) = (@>)

instance Indexable (Vector (Complex Double)) (Complex Double)
  where
    (!) = (@>)

instance Indexable (Vector (Complex Float)) (Complex Float)
  where
    (!) = (@>)

instance Element t => Indexable (Matrix t) (Vector t)
  where
    m!j = subVector (j*c) c (flatten m)
      where
        c = cols m

--------------------------------------------------------------------------------

-- | Matrix of pairwise squared distances of row vectors
-- (using the matrix product trick in blog.smola.org)
pairwiseD2 :: Matrix Double -> Matrix Double -> Matrix Double
pairwiseD2 x y | ok = x2 `outer` oy + ox `outer` y2 - 2* x <> trans y
               | otherwise = error $ "pairwiseD2 with different number of columns: "
                                   ++ show (size x) ++ ", " ++ show (size y)
  where
    ox = one (rows x)
    oy = one (rows y)
    oc = one (cols x)
    one k = konst 1 k
    x2 = x * x <> oc
    y2 = y * y <> oc
    ok = cols x == cols y

--------------------------------------------------------------------------------

-- | outer products of rows
rowOuters :: Matrix Double -> Matrix Double -> Matrix Double
rowOuters a b = a' * b'
  where
    a' = kronecker a (ones 1 (cols b))
    b' = kronecker (ones 1 (cols a)) b

--------------------------------------------------------------------------------

-- | solution of overconstrained homogeneous linear system
null1 :: Matrix Double -> Vector Double
null1 = last . toColumns . snd . rightSV

-- | solution of overconstrained homogeneous symmetric linear system
null1sym :: Matrix Double -> Vector Double
null1sym = last . toColumns . snd . eigSH'

--------------------------------------------------------------------------------

vec :: Element t => Matrix t -> Vector t
-- ^ stacking of columns
vec = flatten . trans


vech :: Element t => Matrix t -> Vector t
-- ^ half-vectorization (of the lower triangular part)
vech m = vjoin . zipWith f [0..] . toColumns $ m
  where
    f k v = subVector k (dim v - k) v


dup :: (Num t, Num (Vector t), Element t) => Int -> Matrix t
-- ^ duplication matrix (@'dup' k \<> 'vech' m == 'vec' m@, for symmetric m of 'dim' k)
dup k = trans $ fromRows $ map f es
  where
    rs = zip [0..] (toRows (ident (k^(2::Int))))
    es = [(i,j) | j <- [0..k-1], i <- [0..k-1], i>=j ]
    f (i,j) | i == j = g (k*j + i)
            | otherwise = g (k*j + i) + g (k*i + j)
    g j = v
      where
        Just v = lookup j rs


vtrans :: Element t => Int -> Matrix t -> Matrix t
-- ^ generalized \"vector\" transposition: @'vtrans' 1 == 'trans'@, and @'vtrans' ('rows' m) m == 'asColumn' ('vec' m)@
vtrans p m | r == 0 = fromBlocks . map (map asColumn . takesV (replicate q p)) . toColumns $ m
           | otherwise = error $ "vtrans " ++ show p ++ " of matrix with " ++ show (rows m) ++ " rows"
  where
    (q,r) = divMod (rows m) p

