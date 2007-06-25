{-# OPTIONS_GHC -fglasgow-exts #-}
-----------------------------------------------------------------------------
{- |
Module      :  GSL.Compat
Copyright   :  (c) Alberto Ruiz 2006
License     :  GPL-style

Maintainer  :  Alberto Ruiz (aruiz at um dot es)
Stability   :  provisional
Portability :  uses -fffi and -fglasgow-exts

Creates reasonable numeric instances for Vectors and Matrices. In the context of the standard numeric operators, one-component vectors and matrices automatically expand to match the dimensions of the other operand.

-}
-----------------------------------------------------------------------------

module GSL.Compat(
  Mul,(<>), fromFile, readMatrix, size, dispR, dispC, format, gmap
) where

import Data.Packed.Internal hiding (dsp)
import Data.Packed.Vector
import Data.Packed.Matrix
import GSL.Vector
import GSL.Matrix
import LinearAlgebra.Algorithms
import Complex
import Numeric(showGFloat)
import Data.List(transpose,intersperse)


adaptScalar f1 f2 f3 x y
    | dim x == 1 = f1   (x@>0) y
    | dim y == 1 = f3 x (y@>0)
    | otherwise = f2 x y

instance (Eq a, Field a) => Eq (Vector a) where
    a == b = dim a == dim b && toList a == toList b

instance (Num a, Field a) => Num (Vector a) where
    (+) = adaptScalar addConstant add (flip addConstant)
    negate = scale (-1)
    (*) = adaptScalar scale mul (flip scale)
    signum = liftVector signum
    abs = liftVector abs
    fromInteger = fromList . return . fromInteger

instance (Eq a, Field a) => Eq (Matrix a) where
    a == b = rows a == rows b && cols a == cols b && cdat a == cdat b && fdat a == fdat b

instance (Num a, Field a) => Num (Matrix a) where
    (+) = liftMatrix2 (+)
    negate = liftMatrix negate
    (*) = liftMatrix2 (*)
    signum = liftMatrix signum
    abs = liftMatrix abs
    fromInteger = (1><1) . return . fromInteger

---------------------------------------------------

instance Fractional (Vector Double) where
    fromRational n = fromList [fromRational n]
    (/) = adaptScalar f (vectorZipR Div) g where
        r `f` v = vectorMapValR Recip r v
        v `g` r = scale (recip r) v

-------------------------------------------------------

instance Fractional (Vector (Complex Double)) where
    fromRational n = fromList [fromRational n]
    (/) = adaptScalar f (vectorZipC Div) g where
        r `f` v = vectorMapValC Recip r v
        v `g` r = scale (recip r) v

------------------------------------------------------

instance Fractional (Matrix Double) where
    fromRational n = (1><1) [fromRational n]
    (/) = liftMatrix2 (/)

-------------------------------------------------------

instance Fractional (Matrix (Complex Double)) where
    fromRational n = (1><1) [fromRational n]
    (/) = liftMatrix2 (/)

---------------------------------------------------------

instance Floating (Vector Double) where
    sin   = vectorMapR Sin
    cos   = vectorMapR Cos
    tan   = vectorMapR Tan
    asin  = vectorMapR ASin
    acos  = vectorMapR ACos
    atan  = vectorMapR ATan
    sinh  = vectorMapR Sinh
    cosh  = vectorMapR Cosh
    tanh  = vectorMapR Tanh
    asinh = vectorMapR ASinh
    acosh = vectorMapR ACosh
    atanh = vectorMapR ATanh
    exp   = vectorMapR Exp
    log   = vectorMapR Log
    sqrt  = vectorMapR Sqrt
    (**)  = adaptScalar (vectorMapValR PowSV) (vectorZipR Pow) (flip (vectorMapValR PowVS))
    pi    = fromList [pi]

-----------------------------------------------------------

instance Floating (Matrix Double) where
    sin   = liftMatrix sin
    cos   = liftMatrix cos
    tan   = liftMatrix tan
    asin  = liftMatrix asin
    acos  = liftMatrix acos
    atan  = liftMatrix atan
    sinh  = liftMatrix sinh
    cosh  = liftMatrix cosh
    tanh  = liftMatrix tanh
    asinh = liftMatrix asinh
    acosh = liftMatrix acosh
    atanh = liftMatrix atanh
    exp   = liftMatrix exp
    log   = liftMatrix log
    (**)  = liftMatrix2 (**)
    sqrt  = liftMatrix sqrt
    pi    = (1><1) [pi]
-------------------------------------------------------------

instance Floating (Vector (Complex Double)) where
    sin   = vectorMapC Sin
    cos   = vectorMapC Cos
    tan   = vectorMapC Tan
    asin  = vectorMapC ASin
    acos  = vectorMapC ACos
    atan  = vectorMapC ATan
    sinh  = vectorMapC Sinh
    cosh  = vectorMapC Cosh
    tanh  = vectorMapC Tanh
    asinh = vectorMapC ASinh
    acosh = vectorMapC ACosh
    atanh = vectorMapC ATanh
    exp   = vectorMapC Exp
    log   = vectorMapC Log
    sqrt  = vectorMapC Sqrt
    (**)  = adaptScalar (vectorMapValC PowSV) (vectorZipC Pow) (flip (vectorMapValC PowVS))
    pi    = fromList [pi]

---------------------------------------------------------------

instance Floating (Matrix (Complex Double)) where
    sin   = liftMatrix sin
    cos   = liftMatrix cos
    tan   = liftMatrix tan
    asin  = liftMatrix asin
    acos  = liftMatrix acos
    atan  = liftMatrix atan
    sinh  = liftMatrix sinh
    cosh  = liftMatrix cosh
    tanh  = liftMatrix tanh
    asinh = liftMatrix asinh
    acosh = liftMatrix acosh
    atanh = liftMatrix atanh
    exp   = liftMatrix exp
    log   = liftMatrix log
    (**)  = liftMatrix2 (**)
    sqrt  = liftMatrix sqrt
    pi    = (1><1) [pi]

---------------------------------------------------------------


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
 (<>) = mXm

instance Mul (Matrix (Complex Double)) (Matrix (Complex Double)) (Matrix (Complex Double)) where
 (<>) = mXm

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