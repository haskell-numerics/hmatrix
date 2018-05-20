{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
-----------------------------------------------------------------------------
{- |
Module      :  Internal.Convolution
Copyright   :  (c) Alberto Ruiz 2012
License     :  BSD3
Maintainer  :  Alberto Ruiz
Stability   :  provisional

-}
-----------------------------------------------------------------------------
{-# OPTIONS_HADDOCK hide #-}

module Internal.Convolution(
   corr, conv, corrMin,
   corr2, conv2, separable
) where

import qualified Data.Vector.Storable as SV
import Internal.Vector
import Internal.Matrix
import Internal.Numeric
import Internal.Element
import Internal.Conversion
import Internal.Container
#if MIN_VERSION_base(4,11,0)
import Prelude hiding ((<>))
#endif


vectSS :: Element t => Int -> Vector t -> Matrix t
vectSS n v = fromRows [ subVector k n v | k <- [0 .. dim v - n] ]


corr
  :: (Container Vector t, Product t)
    => Vector t -- ^ kernel
    -> Vector t -- ^ source
    -> Vector t
{- ^ correlation

>>> corr (fromList[1,2,3]) (fromList [1..10])
[14.0,20.0,26.0,32.0,38.0,44.0,50.0,56.0]
it :: (Enum t, Product t, Container Vector t) => Vector t

-}
corr ker v
    | dim ker == 0 = konst 0 (dim v)
    | dim ker <= dim v = vectSS (dim ker) v <> ker
    | otherwise = error $ "corr: dim kernel ("++show (dim ker)++") > dim vector ("++show (dim v)++")"


conv :: (Container Vector t, Product t, Num t) => Vector t -> Vector t -> Vector t
{- ^ convolution ('corr' with reversed kernel and padded input, equivalent to polynomial product)

>>> conv (fromList[1,1]) (fromList [-1,1])
[-1.0,0.0,1.0]
it :: (Product t, Container Vector t) => Vector t

-}
conv ker v
    | dim ker == 0 = konst 0 (dim v)
    | otherwise = corr ker' v'
  where
    ker' = SV.reverse ker
    v' = vjoin [z,v,z]
    z = konst 0 (dim ker -1)

corrMin :: (Container Vector t, RealElement t, Product t)
        => Vector t
        -> Vector t
        -> Vector t
-- ^ similar to 'corr', using 'min' instead of (*)
corrMin ker v
    | dim ker == 0 = error "corrMin: empty kernel"
    | otherwise    = minEvery ss (asRow ker) <> ones
  where
    minEvery a b = cond a b a a b
    ss = vectSS (dim ker) v
    ones = konst 1 (dim ker)



matSS :: Element t => Int -> Matrix t -> [Matrix t]
matSS dr m = map (reshape c) [ subVector (k*c) n v | k <- [0 .. r - dr] ]
  where
    v = flatten m
    c = cols m
    r = rows m
    n = dr*c


{- | 2D correlation (without padding)

>>> disp 5 $ corr2 (konst 1 (3,3)) (ident 10 :: Matrix Double)
8x8
3  2  1  0  0  0  0  0
2  3  2  1  0  0  0  0
1  2  3  2  1  0  0  0
0  1  2  3  2  1  0  0
0  0  1  2  3  2  1  0
0  0  0  1  2  3  2  1
0  0  0  0  1  2  3  2
0  0  0  0  0  1  2  3

-}
corr2 :: Product a => Matrix a -> Matrix a -> Matrix a
corr2 ker mat = dims
              . concatMap (map (udot ker' . flatten) . matSS c . trans)
              . matSS r $ mat
  where
    r = rows ker
    c = cols ker
    ker' = flatten (trans ker)
    rr = rows mat - r + 1
    rc = cols mat - c + 1
    dims | rr > 0 && rc > 0 = (rr >< rc)
         | otherwise = error $ "corr2: dim kernel ("++sz ker++") > dim matrix ("++sz mat++")"
    sz m = show (rows m)++"x"++show (cols m)
-- TODO check empty kernel

{- | 2D convolution

>>> disp 5 $ conv2 (konst 1 (3,3)) (ident 10 :: Matrix Double)
12x12
1  1  1  0  0  0  0  0  0  0  0  0
1  2  2  1  0  0  0  0  0  0  0  0
1  2  3  2  1  0  0  0  0  0  0  0
0  1  2  3  2  1  0  0  0  0  0  0
0  0  1  2  3  2  1  0  0  0  0  0
0  0  0  1  2  3  2  1  0  0  0  0
0  0  0  0  1  2  3  2  1  0  0  0
0  0  0  0  0  1  2  3  2  1  0  0
0  0  0  0  0  0  1  2  3  2  1  0
0  0  0  0  0  0  0  1  2  3  2  1
0  0  0  0  0  0  0  0  1  2  2  1
0  0  0  0  0  0  0  0  0  1  1  1

-}
conv2
    :: (Num (Matrix a), Product a, Container Vector a)
    => Matrix a -- ^ kernel
    -> Matrix a -> Matrix a
conv2 k m
    | empty     = konst 0 (rows m + r -1, cols m + c -1)
    | otherwise = corr2 (fliprl . flipud $ k) padded
  where
    padded = fromBlocks [[z,0,0]
                        ,[0,m,0]
                        ,[0,0,z]]
    r = rows k
    c = cols k
    z = konst 0 (r-1,c-1)
    empty = r == 0 || c == 0


separable :: Element t => (Vector t -> Vector t) -> Matrix t -> Matrix t
-- ^ matrix computation implemented as separated vector operations by rows and columns.
separable f = fromColumns . map f . toColumns . fromRows . map f . toRows

