{-# OPTIONS #-}
-----------------------------------------------------------------------------
{- |
Module      :  Numeric.LinearAlgebra.Tests.Properties
Copyright   :  (c) Alberto Ruiz 2008
License     :  GPL-style

Maintainer  :  Alberto Ruiz (aruiz at um dot es)
Stability   :  provisional
Portability :  portable

Arbitrary instances for vectors, matrices.

-}

module Numeric.LinearAlgebra.Tests.Properties

where

import Numeric.LinearAlgebra
import Numeric.LinearAlgebra.Tests.Instances(Sq(..),Her(..),Rot(..))

-- relative error
dist :: (Normed t, Num t) => t -> t -> Double
dist a b = r
    where norm = pnorm Infinity
          na = norm a
          nb = norm b
          nab = norm (a-b)
          mx = max na nb
          mn = min na nb
          r = if mn < eps
                then mx
                else nab/mx

infixl 4 |~|
a |~| b = a :~10~: b
--a |~| b = dist a b < 10^^(-10)

data Aprox a = (:~) a Int
(~:) :: (Normed a, Num a) => Aprox a -> a -> Bool
a :~n~: b = dist a b < 10^^(-n)

------------------------------------------------------

square m = rows m == cols m

unitary m = square m && m <> ctrans m |~| ident (rows m)

hermitian m = square m && m |~| ctrans m

degenerate m = rank m < min (rows m) (cols m)

wellCond m = rcond m > 1/100

-----------------------------------------------------

luTest m = m |~| p <> l <> u && det p == s
    where (l,u,p,s) = lu m
