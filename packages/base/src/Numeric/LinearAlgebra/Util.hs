{-# LANGUAGE FlexibleContexts #-}
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
    size, disp,
    zeros, ones,
    diagl,
    row,
    col,
    (&), (¦), (——), (#),
    (?), (¿),
    cross,
    norm,
    unitary,
    mt,
    pairwiseD2,
    meanCov,
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
    vtrans,
{-    -- * Plot
    mplot,
    plot, parametricPlot,
    splot, mesh, meshdom,
    matrixToPGM, imshow,
    gnuplotX, gnuplotpdf, gnuplotWin
-}
) where

import Numeric.Container
import Data.Packed.IO
import Numeric.LinearAlgebra.Algorithms hiding (i)
import Numeric.Matrix()
import Numeric.Vector()

import Numeric.LinearAlgebra.Util.Convolution
--import Graphics.Plot


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


-- | Obtains a vector in the same direction with 2-norm=1
unitary :: Vector Double -> Vector Double
unitary v = v / scalar (norm v)

-- | ('rows' &&& 'cols')
size :: Matrix t -> (Int, Int)
size m = (rows m, cols m)

-- | trans . inv
mt :: Matrix Double -> Matrix Double
mt = trans . inv

--------------------------------------------------------------------------------

{- | Compute mean vector and covariance matrix of the rows of a matrix.

>>> meanCov $ gaussianSample 666 1000 (fromList[4,5]) (diagl[2,3])
(fromList [4.010341078059521,5.0197204699640405],
(2><2)
 [     1.9862461923890056, -1.0127225830525157e-2
 , -1.0127225830525157e-2,     3.0373954915729318 ])

-}
meanCov :: Matrix Double -> (Vector Double, Matrix Double)
meanCov x = (med,cov) where
    r    = rows x
    k    = 1 / fromIntegral r
    med  = konst k r `vXm` x
    meds = konst 1 r `outer` med
    xc   = x `sub` meds
    cov  = scale (recip (fromIntegral (r-1))) (trans xc `mXm` xc)

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
    one k = constant 1 k
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

