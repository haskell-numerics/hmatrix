{-# OPTIONS_GHC -fglasgow-exts #-}
-----------------------------------------------------------------------------
{- |
Module      :  Data.Packed.Instances
Copyright   :  (c) Alberto Ruiz 2006
License     :  GPL-style

Maintainer  :  Alberto Ruiz (aruiz at um dot es)
Stability   :  provisional
Portability :  uses -fffi and -fglasgow-exts

Creates reasonable numeric instances for Vectors and Matrices. In the context of the standard numeric operators, one-component vectors and matrices automatically expand to match the dimensions of the other operand.

-}
-----------------------------------------------------------------------------

module Data.Packed.Instances(
    Contractible(..)
) where

import Data.Packed.Internal
import Data.Packed.Vector
import Data.Packed.Matrix
import GSL.Vector
import GSL.Matrix
import LinearAlgebra.Algorithms
import Complex

instance (Eq a, Field a) => Eq (Vector a) where
    a == b = dim a == dim b && toList a == toList b

instance (Num a, Field a) => Num (Vector a) where
    (+) = add
    (-) = sub
    (*) = mul
    signum = liftVector signum
    abs = liftVector abs
    fromInteger = fromList . return . fromInteger

instance (Eq a, Field a) => Eq (Matrix a) where
    a == b = rows a == rows b && cols a == cols b && cdat a == cdat b && fdat a == fdat b

instance (Num a, Field a) => Num (Matrix a) where
    (+) = liftMatrix2 add
    (-) = liftMatrix2 sub
    (*) = liftMatrix2 mul
    signum = liftMatrix signum
    abs = liftMatrix abs
    fromInteger = (1><1) . return . fromInteger

---------------------------------------------------

adaptScalar f1 f2 f3 x y
    | dim x == 1 = f1 (x@>0) y
    | dim y == 1 = f3 x (y@>0)
    | otherwise = f2 x y

{-
subvv = vectorZip 4
subvc v c = addConstant (-c) v
subcv c v = addConstant c (scale (-1) v)

mul = vectorZip 1

instance Num (Vector Double) where
    (+) = adaptScalar addConstant add (flip addConstant)
    (-) = adaptScalar subcv subvv subvc
    (*) = adaptScalar scale mul (flip scale)
    abs = vectorMap 3
    signum = vectorMap 15
    fromInteger n = fromList [fromInteger n]

----------------------------------------------------

--addConstantC a = gmap (+a)
--subCvv u v = u `add` scale (-1) v
subCvv = vectorZipComplex 4 -- faster?
subCvc v c = addConstantC (-c) v
subCcv c v = addConstantC c (scale (-1) v)


instance Num (Vector (Complex Double)) where
    (+) = adaptScalar addConstantC add (flip addConstantC)
    (-) = adaptScalar subCcv subCvv subCvc
    (*) = adaptScalar scale (vectorZipComplex 1) (flip scale)
    abs = gmap abs
    signum = gmap signum
    fromInteger n = fromList [fromInteger n]


-- | adapts a function on two vectors to work on all the elements of two matrices
liftMatrix2' :: (Vector a -> Vector b -> Vector c) -> Matrix a -> Matrix b -> Matrix c
liftMatrix2' f m1@(M r1 c1 _) m2@(M r2 c2 _)
    | sameShape m1 m2 || r1*c1==1 || r2*c2==1
        = reshape (max c1 c2) $ f (flatten m1) (flatten m2)
    | otherwise = error "inconsistent matrix dimensions" 

---------------------------------------------------

instance (Eq a, Field a) => Eq (Matrix a) where
    a == b = rows a == rows b && cdat a == cdat b

instance Num (Matrix Double) where
    (+) = liftMatrix2' (+)
    (-) = liftMatrix2' (-)
    (*) = liftMatrix2' (*)
    abs = liftMatrix abs
    signum = liftMatrix signum
    fromInteger n = fromLists [[fromInteger n]]

----------------------------------------------------

instance Num (Matrix (Complex Double)) where
    (+) = liftMatrix2' (+)
    (-) = liftMatrix2' (-)
    (*) = liftMatrix2' (*)
    abs = liftMatrix abs
    signum = liftMatrix signum
    fromInteger n = fromLists [[fromInteger n]]

------------------------------------------------------

instance Fractional (Vector Double) where
    fromRational n = fromList [fromRational n]
    (/) = adaptScalar f (vectorZip 2) g where
        r `f` v = vectorZip 2 (constant r (dim v)) v
        v `g` r = scale (recip r) v

-------------------------------------------------------

instance Fractional (Vector (Complex Double)) where
    fromRational n = fromList [fromRational n]
    (/) = adaptScalar f (vectorZipComplex 2) g where
        r `f` v = gmap ((*r).recip) v
        v `g` r = gmap (/r) v

------------------------------------------------------

instance Fractional (Matrix Double) where
    fromRational n = fromLists [[fromRational n]]
    (/) = liftMatrix2' (/)

-------------------------------------------------------

instance Fractional (Matrix (Complex Double)) where
    fromRational n = fromLists [[fromRational n]]
    (/) = liftMatrix2' (/)

---------------------------------------------------------

instance Floating (Vector Double) where
    sin   = vectorMap 0
    cos   = vectorMap 1
    tan   = vectorMap 2
    asin  = vectorMap 4
    acos  = vectorMap 5
    atan  = vectorMap 6
    sinh  = vectorMap 7
    cosh  = vectorMap 8
    tanh  = vectorMap 9
    asinh = vectorMap 10
    acosh = vectorMap 11
    atanh = vectorMap 12
    exp   = vectorMap 13
    log   = vectorMap 14
    sqrt  = vectorMap 16
    (**)  = adaptScalar f (vectorZip 5) g where f s v = constant s (dim v) ** v
                                                g v s = v ** constant s (dim v)
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
    sqrt  = liftMatrix sqrt
    (**)  = liftMatrix2 (**)
    pi    = fromLists [[pi]]

-------------------------------------------------------------

instance Floating (Vector (Complex Double)) where
    sin   = vectorMapComplex 0
    cos   = vectorMapComplex 1
    tan   = vectorMapComplex 2
    asin  = vectorMapComplex 4
    acos  = vectorMapComplex 5
    atan  = vectorMapComplex 6
    sinh  = vectorMapComplex 7
    cosh  = vectorMapComplex 8
    tanh  = vectorMapComplex 9
    asinh = vectorMapComplex 10
    acosh = vectorMapComplex 11
    atanh = vectorMapComplex 12
    exp   = vectorMapComplex 13
    log   = vectorMapComplex 14
    sqrt  = vectorMapComplex 16
    (**)  = adaptScalar f (vectorZipComplex 5) g where f s v = constantC s (dim v) ** v
                                                       g v s = v ** constantC s (dim v)
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
    pi    = fromLists [[pi]]

---------------------------------------------------------------
-}

class Contractible a b c | a b -> c where
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


instance Contractible Double Double Double where
 (<>) = (*)

instance Contractible Double (Complex Double) (Complex Double) where
 a <> b = (a:+0) * b

instance Contractible (Complex Double) Double (Complex Double) where
 a <> b = a * (b:+0)

instance Contractible (Complex Double) (Complex Double) (Complex Double) where
 (<>) = (*)

--------------------------------- matrix matrix

instance Contractible (Matrix Double) (Matrix Double) (Matrix Double) where
 (<>) = mXm

instance Contractible (Matrix (Complex Double)) (Matrix (Complex Double)) (Matrix (Complex Double)) where
 (<>) = mXm

instance Contractible (Matrix (Complex Double)) (Matrix Double) (Matrix (Complex Double)) where
 c <> r = c <> liftMatrix comp r

instance Contractible (Matrix Double) (Matrix (Complex Double)) (Matrix (Complex Double)) where
 r <> c = liftMatrix comp r <> c

--------------------------------- (Matrix Double) (Vector Double)

instance Contractible (Matrix Double) (Vector Double) (Vector Double) where
 (<>) = mXv

instance Contractible (Matrix (Complex Double)) (Vector (Complex Double)) (Vector (Complex Double)) where
 (<>) = mXv

instance Contractible (Matrix (Complex Double)) (Vector Double) (Vector (Complex Double)) where
 m <> v = m <> comp v

instance Contractible (Matrix Double) (Vector (Complex Double)) (Vector (Complex Double)) where
 m <> v = liftMatrix comp m <> v

--------------------------------- (Vector Double) (Matrix Double)

instance Contractible (Vector Double) (Matrix Double) (Vector Double) where
 (<>) = vXm

instance Contractible (Vector (Complex Double)) (Matrix (Complex Double)) (Vector (Complex Double)) where
 (<>) = vXm

instance Contractible (Vector (Complex Double)) (Matrix Double) (Vector (Complex Double)) where
 v <> m = v <> liftMatrix comp m

instance Contractible (Vector Double) (Matrix (Complex Double)) (Vector (Complex Double)) where
 v <> m = comp v <> m

--------------------------------- dot product

instance Contractible (Vector Double) (Vector Double) Double where
 (<>) = dot

instance Contractible (Vector (Complex Double)) (Vector (Complex Double)) (Complex Double) where
 (<>) = dot

instance Contractible (Vector Double) (Vector (Complex Double)) (Complex Double) where
 a <> b = comp a <> b

instance Contractible (Vector (Complex Double)) (Vector Double) (Complex Double) where
 (<>) = flip (<>)

--------------------------------- scaling vectors  

instance Contractible Double (Vector Double) (Vector Double) where
 (<>) = scale

instance Contractible (Vector Double) Double (Vector Double) where
 (<>) = flip (<>)

instance Contractible (Complex Double) (Vector (Complex Double)) (Vector (Complex Double)) where
 (<>) = scale

instance Contractible (Vector (Complex Double)) (Complex Double) (Vector (Complex Double)) where
 (<>) = flip (<>)

instance Contractible Double (Vector (Complex Double)) (Vector (Complex Double)) where
 a <> v = (a:+0) <> v

instance Contractible (Vector (Complex Double)) Double (Vector (Complex Double)) where
 (<>) = flip (<>)

instance Contractible (Complex Double) (Vector Double) (Vector (Complex Double)) where
 a <> v = a <> comp v

instance Contractible (Vector Double) (Complex Double) (Vector (Complex Double)) where
 (<>) = flip (<>)

--------------------------------- scaling matrices

instance Contractible Double (Matrix Double) (Matrix Double) where
 (<>) a = liftMatrix (a <>)

instance Contractible (Matrix Double) Double (Matrix Double) where
 (<>) = flip (<>)

instance Contractible (Complex Double) (Matrix (Complex Double)) (Matrix (Complex Double)) where
 (<>) a = liftMatrix (a <>)

instance Contractible (Matrix (Complex Double)) (Complex Double) (Matrix (Complex Double)) where
 (<>) = flip (<>)

instance Contractible Double (Matrix (Complex Double)) (Matrix (Complex Double)) where
 a <> m = (a:+0) <> m

instance Contractible (Matrix (Complex Double)) Double (Matrix (Complex Double)) where
 (<>) = flip (<>)

instance Contractible (Complex Double) (Matrix Double) (Matrix (Complex Double)) where
 a <> m = a <> liftMatrix comp m

instance Contractible (Matrix Double) (Complex Double) (Matrix (Complex Double)) where
 (<>) = flip (<>)
