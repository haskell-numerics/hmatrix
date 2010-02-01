{-# LANGUAGE UndecidableInstances, FlexibleInstances #-}
-----------------------------------------------------------------------------
{- |
Module      :  Numeric.LinearAlgebra.Instances
Copyright   :  (c) Alberto Ruiz 2006
License     :  GPL-style

Maintainer  :  Alberto Ruiz (aruiz at um dot es)
Stability   :  provisional
Portability :  portable

This module exports Show, Read, Eq, Num, Fractional, and Floating instances for Vector and Matrix.

In the context of the standard numeric operators, one-component vectors and matrices automatically expand to match the dimensions of the other operand.

-}
-----------------------------------------------------------------------------

module Numeric.LinearAlgebra.Instances(
) where

import Numeric.LinearAlgebra.Linear
import Numeric.GSL.Vector
import Data.Packed.Matrix
import Complex
import Data.List(transpose,intersperse)
import Foreign(Storable)
import Data.Monoid
import Data.Packed.Internal.Vector
-- import Control.Parallel.Strategies

------------------------------------------------------------------

instance (Show a, Element a) => (Show (Matrix a)) where
    show m = (sizes++) . dsp . map (map show) . toLists $ m
        where sizes = "("++show (rows m)++"><"++show (cols m)++")\n"

dsp as = (++" ]") . (" ["++) . init . drop 2 . unlines . map (" , "++) . map unwords' $ transpose mtp
    where
        mt = transpose as
        longs = map (maximum . map length) mt
        mtp = zipWith (\a b -> map (pad a) b) longs mt
        pad n str = replicate (n - length str) ' ' ++ str
        unwords' = concat . intersperse ", "

instance (Show a, Storable a) => (Show (Vector a)) where
    show v = (show (dim v))++" |> " ++ show (toList v)

------------------------------------------------------------------

instance (Element a, Read a) => Read (Matrix a) where
    readsPrec _ s = [((rs><cs) . read $ listnums, rest)]
        where (thing,rest) = breakAt ']' s
              (dims,listnums) = breakAt ')' thing
              cs = read . init . fst. breakAt ')' . snd . breakAt '<' $ dims
              rs = read . snd . breakAt '(' .init . fst . breakAt '>' $ dims

instance (Element a, Read a) => Read (Vector a) where
    readsPrec _ s = [((d |>) . read $ listnums, rest)]
        where (thing,rest) = breakAt ']' s
              (dims,listnums) = breakAt '>' thing
              d = read . init . fst . breakAt '|' $ dims

breakAt c l = (a++[c],tail b) where
    (a,b) = break (==c) l

------------------------------------------------------------------

adaptScalar f1 f2 f3 x y
    | dim x == 1 = f1   (x@>0) y
    | dim y == 1 = f3 x (y@>0)
    | otherwise = f2 x y


instance Linear Vector a => Eq (Vector a) where
    (==) = equal

instance Num (Vector Double) where
    (+) = adaptScalar addConstant add (flip addConstant)
    negate = scale (-1)
    (*) = adaptScalar scale mul (flip scale)
    signum = vectorMapR Sign
    abs = vectorMapR Abs
    fromInteger = fromList . return . fromInteger

instance Num (Vector (Complex Double)) where
    (+) = adaptScalar addConstant add (flip addConstant)
    negate = scale (-1)
    (*) = adaptScalar scale mul (flip scale)
    signum = vectorMapC Sign
    abs = vectorMapC Abs
    fromInteger = fromList . return . fromInteger

instance Linear Matrix a => Eq (Matrix a) where
    (==) = equal

instance (Linear Matrix a, Num (Vector a)) => Num (Matrix a) where
    (+) = liftMatrix2Auto (+)
    (-) = liftMatrix2Auto (-)
    negate = liftMatrix negate
    (*) = liftMatrix2Auto (*)
    signum = liftMatrix signum
    abs = liftMatrix abs
    fromInteger = (1><1) . return . fromInteger

---------------------------------------------------

instance (Linear Vector a, Num (Vector a)) => Fractional (Vector a) where
    fromRational n = fromList [fromRational n]
    (/) = adaptScalar f divide g where
        r `f` v = scaleRecip r v
        v `g` r = scale (recip r) v

-------------------------------------------------------

instance (Linear Vector a, Fractional (Vector a), Num (Matrix a)) => Fractional (Matrix a) where
    fromRational n = (1><1) [fromRational n]
    (/) = liftMatrix2Auto (/)

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

-----------------------------------------------------------

instance (Linear Vector a, Floating (Vector a), Fractional (Matrix a)) => Floating (Matrix a) where
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
    (**)  = liftMatrix2Auto (**)
    sqrt  = liftMatrix sqrt
    pi    = (1><1) [pi]

---------------------------------------------------------------

instance (Storable a, Num (Vector a)) => Monoid (Vector a) where
    mempty = 0 { idim = 0 }
    mappend a b = mconcat [a,b]
    mconcat = j . filter ((>0).dim)
        where j [] = mempty
              j l  = join l

---------------------------------------------------------------

-- instance (NFData a, Storable a) => NFData (Vector a) where
--     rnf = rnf . (@>0)
--
-- instance (NFData a, Element a) => NFData (Matrix a) where
--     rnf = rnf . flatten

