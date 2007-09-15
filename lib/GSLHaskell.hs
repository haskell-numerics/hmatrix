{-# OPTIONS_GHC -fglasgow-exts -fallow-undecidable-instances #-}
-----------------------------------------------------------------------------
{- |
Module      :  GSLHaskell
Copyright   :  (c) Alberto Ruiz 2006
License     :  GPL-style

Maintainer  :  Alberto Ruiz (aruiz at um dot es)
Stability   :  provisional
Portability :  uses -fffi and -fglasgow-exts

Old GSLHaskell interface.

-}
-----------------------------------------------------------------------------

module GSLHaskell(
    module Data.Packed.Vector,
    module Data.Packed.Matrix,
    module LinearAlgebra.Algorithms,
    module LinearAlgebra.Linear,
    module LAPACK,
    module GSL.Integration,
    module GSL.Differentiation,
    module GSL.Fourier,
    module GSL.Polynomials,
    module GSL.Minimization,
    module GSL.Matrix,
    module GSL.Special,
    module Graphics.Plot,
    module Complex,
    Mul,(<>), readMatrix, size, dispR, dispC, format, gmap, Joinable, (<|>),(<->),
    fromArray2D, GSLHaskell.pnorm,
) where

import GSL.Integration
import GSL.Differentiation
import GSL.Fourier
import GSL.Polynomials
import GSL.Minimization
import Graphics.Plot
import Complex
import GSL.Special(setErrorHandlerOff,
    erf,
    erf_Z,
    bessel_J0_e,
    exp_e10_e,
    gamma)
import Data.Packed.Vector
import Data.Packed.Matrix
import Data.Packed.Matrix hiding ((><))
import GSL.Vector
import qualified LinearAlgebra.Algorithms
import LAPACK
import GSL.Matrix
import LinearAlgebra.Algorithms hiding (pnorm)
import LinearAlgebra.Linear hiding (Mul,(<>))
import Data.Packed.Internal.Matrix(multiply)
import Complex
import Numeric(showGFloat)
import Data.List(transpose,intersperse)
import Foreign(Storable)
import Data.Array
import LinearAlgebra.Instances


class Mul a b c | a b -> c where
 infixl 7 <>
{- | An overloaded operator for matrix products, matrix-vector and vector-matrix products, dot products and scaling of vectors and matrices. Type consistency is statically checked. Alternatively, you can use the specific functions described below, but using this operator you can automatically combine real and complex objects.

@v  = 'fromList' [1,2,3]    :: Vector Double
cv = 'fromList' [1+'i',2]
m  = 'fromLists' [[1,2,3],
                [4,5,7]] :: Matrix Double
cm = 'fromLists' [[  1,  2],
                [3+'i',7*'i'],
                [  'i',  1]]
\ 
\> m \<\> v
14. 35.
\ 
\> cv \<\> m
9.+1.i  12.+2.i  17.+3.i
\ 
\> m \<\> cm
  7.+5.i   5.+14.i
19.+12.i  15.+35.i
\ 
\> v \<\> 'i'
1.i  2.i  3.i
\ 
\> v \<\> v
14.0
\ 
\> cv \<\> cv
4.0 :+ 2.0@

-}
 (<>) :: a -> b -> c


instance Mul Double Double Double where
 (<>) = (*)

instance Mul Double (Complex Double) (Complex Double) where
 a <> b = (a:+0) * b

instance Mul (Complex Double) Double (Complex Double) where
 a <> b = a * (b:+0)

instance Mul (Complex Double) (Complex Double) (Complex Double) where
 (<>) = (*)

--------------------------------- matrix matrix

instance Mul (Matrix Double) (Matrix Double) (Matrix Double) where
 (<>) = multiply

instance Mul (Matrix (Complex Double)) (Matrix (Complex Double)) (Matrix (Complex Double)) where
 (<>) = multiply

instance Mul (Matrix (Complex Double)) (Matrix Double) (Matrix (Complex Double)) where
 c <> r = c <> liftMatrix comp r

instance Mul (Matrix Double) (Matrix (Complex Double)) (Matrix (Complex Double)) where
 r <> c = liftMatrix comp r <> c

--------------------------------- (Matrix Double) (Vector Double)

instance Mul (Matrix Double) (Vector Double) (Vector Double) where
 (<>) = mXv

instance Mul (Matrix (Complex Double)) (Vector (Complex Double)) (Vector (Complex Double)) where
 (<>) = mXv

instance Mul (Matrix (Complex Double)) (Vector Double) (Vector (Complex Double)) where
 m <> v = m <> comp v

instance Mul (Matrix Double) (Vector (Complex Double)) (Vector (Complex Double)) where
 m <> v = liftMatrix comp m <> v

--------------------------------- (Vector Double) (Matrix Double)

instance Mul (Vector Double) (Matrix Double) (Vector Double) where
 (<>) = vXm

instance Mul (Vector (Complex Double)) (Matrix (Complex Double)) (Vector (Complex Double)) where
 (<>) = vXm

instance Mul (Vector (Complex Double)) (Matrix Double) (Vector (Complex Double)) where
 v <> m = v <> liftMatrix comp m

instance Mul (Vector Double) (Matrix (Complex Double)) (Vector (Complex Double)) where
 v <> m = comp v <> m

--------------------------------- dot product

instance Mul (Vector Double) (Vector Double) Double where
 (<>) = dot

instance Mul (Vector (Complex Double)) (Vector (Complex Double)) (Complex Double) where
 (<>) = dot

instance Mul (Vector Double) (Vector (Complex Double)) (Complex Double) where
 a <> b = comp a <> b

instance Mul (Vector (Complex Double)) (Vector Double) (Complex Double) where
 (<>) = flip (<>)

--------------------------------- scaling vectors  

instance Mul Double (Vector Double) (Vector Double) where
 (<>) = scale

instance Mul (Vector Double) Double (Vector Double) where
 (<>) = flip (<>)

instance Mul (Complex Double) (Vector (Complex Double)) (Vector (Complex Double)) where
 (<>) = scale

instance Mul (Vector (Complex Double)) (Complex Double) (Vector (Complex Double)) where
 (<>) = flip (<>)

instance Mul Double (Vector (Complex Double)) (Vector (Complex Double)) where
 a <> v = (a:+0) <> v

instance Mul (Vector (Complex Double)) Double (Vector (Complex Double)) where
 (<>) = flip (<>)

instance Mul (Complex Double) (Vector Double) (Vector (Complex Double)) where
 a <> v = a <> comp v

instance Mul (Vector Double) (Complex Double) (Vector (Complex Double)) where
 (<>) = flip (<>)

--------------------------------- scaling matrices

instance Mul Double (Matrix Double) (Matrix Double) where
 (<>) a = liftMatrix (a <>)

instance Mul (Matrix Double) Double (Matrix Double) where
 (<>) = flip (<>)

instance Mul (Complex Double) (Matrix (Complex Double)) (Matrix (Complex Double)) where
 (<>) a = liftMatrix (a <>)

instance Mul (Matrix (Complex Double)) (Complex Double) (Matrix (Complex Double)) where
 (<>) = flip (<>)

instance Mul Double (Matrix (Complex Double)) (Matrix (Complex Double)) where
 a <> m = (a:+0) <> m

instance Mul (Matrix (Complex Double)) Double (Matrix (Complex Double)) where
 (<>) = flip (<>)

instance Mul (Complex Double) (Matrix Double) (Matrix (Complex Double)) where
 a <> m = a <> liftMatrix comp m

instance Mul (Matrix Double) (Complex Double) (Matrix (Complex Double)) where
 (<>) = flip (<>)

-----------------------------------------------------------------------------------

size :: Vector a -> Int
size = dim

gmap :: (Storable a, Storable b) => (a->b) -> Vector a -> Vector b
gmap f v = liftVector f v

-- shows a Double with n digits after the decimal point    
shf :: (RealFloat a) => Int -> a -> String     
shf dec n | abs n < 1e-10 = "0."
          | abs (n - (fromIntegral.round $ n)) < 1e-10 = show (round n) ++"."
          | otherwise = showGFloat (Just dec) n ""    
-- shows a Complex Double as a pair, with n digits after the decimal point    
shfc n z@ (a:+b) 
    | magnitude z <1e-10 = "0."
    | abs b < 1e-10 = shf n a
    | abs a < 1e-10 = shf n b ++"i"
    | b > 0         = shf n a ++"+"++shf n b ++"i"
    | otherwise     = shf n a ++shf n b ++"i"         

dsp :: String -> [[String]] -> String
dsp sep as = unlines . map unwords' $ transpose mtp where 
    mt = transpose as
    longs = map (maximum . map length) mt
    mtp = zipWith (\a b -> map (pad a) b) longs mt
    pad n str = replicate (n - length str) ' ' ++ str
    unwords' = concat . intersperse sep

format :: (Field t) => String -> (t -> String) -> Matrix t -> String
format sep f m = dsp sep . map (map f) . toLists $ m

disp m f = putStrLn $ "matrix ("++show (rows m) ++"x"++ show (cols m) ++")\n"++format " | " f m

dispR :: Int -> Matrix Double -> IO ()
dispR d m = disp m (shf d)

dispC :: Int -> Matrix (Complex Double) -> IO ()
dispC d m = disp m (shfc d)

-- | creates a matrix from a table of numbers.
readMatrix :: String -> Matrix Double
readMatrix = fromLists . map (map read). map words . filter (not.null) . lines

-------------------------------------------------------------

class Joinable a b c | a b -> c where
    joinH :: a -> b -> c
    joinV :: a -> b -> c

instance Joinable (Matrix Double) (Vector Double) (Matrix Double) where
    joinH m v = fromBlocks [[m,reshape 1 v]]
    joinV m v = fromBlocks [[m],[reshape (size v) v]]

instance Joinable (Vector Double) (Matrix Double) (Matrix Double) where
    joinH v m = fromBlocks [[reshape 1 v,m]]
    joinV v m = fromBlocks [[reshape (size v) v],[m]]

instance Joinable (Matrix Double) (Matrix Double) (Matrix Double) where
    joinH m1 m2 = fromBlocks [[m1,m2]]
    joinV m1 m2 = fromBlocks [[m1],[m2]]

instance Joinable (Matrix (Complex Double)) (Vector (Complex Double)) (Matrix (Complex Double)) where
    joinH m v = fromBlocks [[m,reshape 1 v]]
    joinV m v = fromBlocks [[m],[reshape (size v) v]]

instance Joinable (Vector (Complex Double)) (Matrix (Complex Double)) (Matrix (Complex Double)) where
    joinH v m = fromBlocks [[reshape 1 v,m]]
    joinV v m = fromBlocks [[reshape (size v) v],[m]]

instance Joinable (Matrix (Complex Double)) (Matrix (Complex Double)) (Matrix (Complex Double)) where
    joinH m1 m2 = fromBlocks [[m1,m2]]
    joinV m1 m2 = fromBlocks [[m1],[m2]]

infixl 3 <|>, <->

{- | Horizontal concatenation of matrices and vectors:

@\> 'ident' 3 \<-\> i\<\>'ident' 3 \<|\> 'fromList' [1..6]
 1.   0.   0.  1.
 0.   1.   0.  2.
 0.   0.   1.  3.
1.i   0.   0.  4.
 0.  1.i   0.  5.
 0.   0.  1.i  6.@
-}
(<|>) :: (Joinable a b c) => a -> b -> c
a <|> b = joinH a b

-- | Vertical concatenation of matrices and vectors.
(<->) :: (Joinable a b c) => a -> b -> c
a <-> b = joinV a b

----------------------------------------------------------

fromArray2D :: (Field e) => Array (Int, Int) e -> Matrix e
fromArray2D m = (r><c) (elems m)
    where ((r0,c0),(r1,c1)) = bounds m
          r = r1-r0+1
          c = c1-c0+1


pnorm :: (Normed t1, Num t) => t -> t1 -> Double
pnorm 0 = LinearAlgebra.Algorithms.pnorm Infinity
pnorm 1 = LinearAlgebra.Algorithms.pnorm PNorm1
pnorm 2 = LinearAlgebra.Algorithms.pnorm PNorm2