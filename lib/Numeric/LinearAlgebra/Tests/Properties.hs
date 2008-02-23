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
import Test.QuickCheck

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

positiveDefinite m = minimum (toList e) > 0
    where (e,v) = eigSH m

upperTriang m = rows m == 1 || down == z
    where down = fromList $ concat $ zipWith drop [1..] (toLists (ctrans m))
          z = constant 0 (dim down)

upperHessenberg m = rows m < 3 || down == z
    where down = fromList $ concat $ zipWith drop [2..] (toLists (ctrans m))
          z = constant 0 (dim down)

zeros (r,c) = reshape c (constant 0 (r*c))

ones (r,c) = zeros (r,c) + 1

-----------------------------------------------------

luProp m = m |~| p <> l <> u && det p == s
    where (l,u,p,s) = lu m

invProp m = m <> inv m |~| ident (rows m)

pinvProp m =  m <> p <> m |~| m
           && p <> m <> p |~| p
           && hermitian (m<>p)
           && hermitian (p<>m)
    where p = pinv m

detProp m = s d1 |~| s d2
    where d1 = det m
          d2 = det' m * det q
          det' m = product $ toList $ takeDiag r
          (q,r) = qr m
          s x = fromList [x]

nullspaceProp m = null nl `trivial` (null nl || m <> n |~| zeros (r,c))
    where nl = nullspacePrec 1 m
          n = fromColumns nl
          r = rows m
          c = cols m - rank m

svdProp1 m = u <> real d <> trans v |~| m
          && unitary u && unitary v
    where (u,d,v) = full svd m

svdProp2 m = (m |~| 0) `trivial` ((m |~| 0) || u <> real (diag s) <> trans v |~| m)
    where (u,s,v) = economy svd m

eigProp m = complex m <> v |~| v <> diag s
    where (s, v) = eig m

eigSHProp m = m <> v |~| v <> real (diag s)
              && unitary v
              && m |~| v <> real (diag s) <> ctrans v
    where (s, v) = eigSH m

qrProp m = q <> r |~| m && unitary q && upperTriang r
    where (q,r) = qr m

hessProp m = m |~| p <> h <> ctrans p && unitary p && upperHessenberg h
    where (p,h) = hess m

schurProp1 m = m |~| u <> s <> ctrans u && unitary u && upperTriang s
    where (u,s) = schur m

schurProp2 m = m |~| u <> s <> ctrans u && unitary u && upperHessenberg s -- fixme
    where (u,s) = schur m

cholProp m = m |~| ctrans c <> c && upperTriang c
    where c = chol m
          pos = positiveDefinite m
