{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Numeric.Matrix
-- Copyright   :  (c) Alberto Ruiz 2014
-- License     :  BSD3
--
-- Maintainer  :  Alberto Ruiz
-- Stability   :  provisional
--
-- Provides instances of standard classes 'Show', 'Read', 'Eq',
-- 'Num', 'Fractional', and 'Floating' for 'Matrix'.
--
-- In arithmetic operations one-component
-- vectors and matrices automatically expand to match the dimensions of the other operand.

-----------------------------------------------------------------------------

module Numeric.Matrix (
                      ) where

-------------------------------------------------------------------

import Internal.Vector
import Internal.Matrix
import Internal.Element
import Internal.Numeric
import qualified Data.Monoid as M
import Data.List(partition)
import qualified Data.Foldable as F
import qualified Data.Semigroup as S
import Internal.Chain
import Foreign.Storable(Storable)


-------------------------------------------------------------------

instance Container Matrix a => Eq (Matrix a) where
    (==) = equal

instance (Container Matrix a, Num a, Num (Vector a)) => Num (Matrix a) where
    (+) = liftMatrix2Auto (+)
    (-) = liftMatrix2Auto (-)
    negate = liftMatrix negate
    (*) = liftMatrix2Auto (*)
    signum = liftMatrix signum
    abs = liftMatrix abs
    fromInteger = (1><1) . return . fromInteger

---------------------------------------------------

instance (Container Vector a, Fractional a, Fractional (Vector a), Num (Matrix a)) => Fractional (Matrix a) where
    fromRational n = (1><1) [fromRational n]
    (/) = liftMatrix2Auto (/)

---------------------------------------------------------

instance (Floating a, Container Vector a, Floating (Vector a), Fractional (Matrix a)) => Floating (Matrix a) where
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

--------------------------------------------------------------------------------

isScalar :: Matrix t -> Bool
isScalar m = rows m == 1 && cols m == 1

adaptScalarM :: (Foreign.Storable.Storable t1, Foreign.Storable.Storable t2)
             => (t1 -> Matrix t2 -> t)
             -> (Matrix t1 -> Matrix t2 -> t)
             -> (Matrix t1 -> t2 -> t)
             -> Matrix t1
             -> Matrix t2
             -> t
adaptScalarM f1 f2 f3 x y
    | isScalar x = f1   (x @@>(0,0) ) y
    | isScalar y = f3 x (y @@>(0,0) )
    | otherwise = f2 x y

instance (Container Vector t, Eq t, Num (Vector t), Product t) => S.Semigroup (Matrix t)
  where
    (<>) = mappend
    sconcat = mconcat . F.toList

instance (Container Vector t, Eq t, Num (Vector t), Product t) => M.Monoid (Matrix t)
  where
    mempty = 1
    mappend = adaptScalarM scale mXm (flip scale)

    mconcat xs = work (partition isScalar xs)
      where
        work (ss,[]) = product ss
        work (ss,ms) = scl (product ss) (optimiseMult ms)
        scl x m
            | isScalar x && x00 == 1 = m
            | otherwise              = scale x00 m
          where
            x00 = x @@> (0,0)
