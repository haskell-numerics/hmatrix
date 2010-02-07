{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
-----------------------------------------------------------------------------
{- |
Module      :  Numeric.LinearAlgebra.Tests.Properties
Copyright   :  (c) Alberto Ruiz 2008
License     :  GPL-style

Maintainer  :  Alberto Ruiz (aruiz at um dot es)
Stability   :  provisional
Portability :  portable

Testing properties.

-}

module Numeric.LinearAlgebra.Tests.Properties (
    dist, (|~|), (~:), Aprox((:~)),
    zeros, ones,
    square,
    unitary,
    hermitian,
    wellCond,
    positiveDefinite,
    upperTriang,
    upperHessenberg,
    luProp,
    invProp,
    pinvProp,
    detProp,
    nullspaceProp,
    svdProp1, svdProp1a, svdProp2, svdProp3, svdProp4,
    svdProp5a, svdProp5b, svdProp6a, svdProp6b, svdProp7,
    eigProp, eigSHProp, eigProp2, eigSHProp2,
    qrProp, rqProp,
    hessProp,
    schurProp1, schurProp2,
    cholProp,
    expmDiagProp,
    multProp1, multProp2,
    linearSolveProp, linearSolveProp2
) where

import Numeric.LinearAlgebra
import Numeric.LinearAlgebra.LAPACK
import Debug.Trace
#include "quickCheckCompat.h"


debug x = trace (show x) x

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

-- orthonormal columns
orthonormal m = ctrans m <> m |~| ident (cols m)

unitary m = square m && orthonormal m

hermitian m = square m && m |~| ctrans m

wellCond m = rcond m > 1/100

positiveDefinite m = minimum (toList e) > 0
    where (e,_v) = eigSH m

upperTriang m = rows m == 1 || down == z
    where down = fromList $ concat $ zipWith drop [1..] (toLists (ctrans m))
          z = constant 0 (dim down)

upperHessenberg m = rows m < 3 || down == z
    where down = fromList $ concat $ zipWith drop [2..] (toLists (ctrans m))
          z = constant 0 (dim down)

zeros (r,c) = reshape c (constant 0 (r*c))

ones (r,c) = zeros (r,c) + 1

-----------------------------------------------------

luProp m = m |~| p <> l <> u && f (det p) |~| f s
    where (l,u,p,s) = lu m
          f x = fromList [x]

invProp m = m <> inv m |~| ident (rows m)

pinvProp m =  m <> p <> m |~| m
           && p <> m <> p |~| p
           && hermitian (m<>p)
           && hermitian (p<>m)
    where p = pinv m

detProp m = s d1 |~| s d2
    where d1 = det m
          d2 = det' * det q
          det' = product $ toList $ takeDiag r
          (q,r) = qr m
          s x = fromList [x]

nullspaceProp m = null nl `trivial` (null nl || m <> n |~| zeros (r,c))
    where nl = nullspacePrec 1 m
          n = fromColumns nl
          r = rows m
          c = cols m - rank m

------------------------------------------------------------------

-- fullSVD
svdProp1 m = m |~| u <> real d <> trans v && unitary u && unitary v
    where (u,d,v) = fullSVD m

svdProp1a svdfun m = m |~| u <> real d <> trans v && unitary u && unitary v where
    (u,s,v) = svdfun m
    d = diagRect s (rows m) (cols m)

-- thinSVD
svdProp2 thinSVDfun m = m |~| u <> diag (real s) <> trans v && orthonormal u && orthonormal v && dim s == min (rows m) (cols m)
    where (u,s,v) = thinSVDfun m

-- compactSVD
svdProp3 m = (m |~| u <> real (diag s) <> trans v
             && orthonormal u && orthonormal v)
    where (u,s,v) = compactSVD m

svdProp4 m' = m |~| u <> real (diag s) <> trans v
           && orthonormal u && orthonormal v
           && (dim s == r || r == 0 && dim s == 1)
    where (u,s,v) = compactSVD m
          m = fromBlocks [[m'],[m']]
          r = rank m'

svdProp5a m = and (map (s1|~|) [s2,s3,s4,s5,s6]) where
    s1       = svR  m
    s2       = svRd m
    (_,s3,_) = svdR m
    (_,s4,_) = svdRd m
    (_,s5,_) = thinSVDR m
    (_,s6,_) = thinSVDRd m

svdProp5b m = and (map (s1|~|) [s2,s3,s4,s5,s6]) where
    s1       = svC  m
    s2       = svCd m
    (_,s3,_) = svdC m
    (_,s4,_) = svdCd m
    (_,s5,_) = thinSVDC m
    (_,s6,_) = thinSVDCd m

svdProp6a m = s |~| s' && v |~| v' && s |~| s'' && u |~| u'
    where (u,s,v) = svdR m
          (s',v') = rightSVR m
          (u',s'') = leftSVR m

svdProp6b m = s |~| s' && v |~| v' && s |~| s'' && u |~| u'
    where (u,s,v) = svdC m
          (s',v') = rightSVC m
          (u',s'') = leftSVC m

svdProp7 m = s |~| s' && u |~| u' && v |~| v' && s |~| s'''
    where (u,s,v) = svd m
          (s',v') = rightSV m
          (u',s'') = leftSV m
          s''' = singularValues m

------------------------------------------------------------------

eigProp m = complex m <> v |~| v <> diag s
    where (s, v) = eig m

eigSHProp m = m <> v |~| v <> real (diag s)
              && unitary v
              && m |~| v <> real (diag s) <> ctrans v
    where (s, v) = eigSH m

eigProp2 m = fst (eig m) |~| eigenvalues m

eigSHProp2 m = fst (eigSH m) |~| eigenvaluesSH m

------------------------------------------------------------------

qrProp m = q <> r |~| m && unitary q && upperTriang r
    where (q,r) = qr m

rqProp m = r <> q |~| m && unitary q && utr
    where (r,q) = rq m
          upptr f c = buildMatrix f c $ \(r',c') -> if r'-t > c' then 0 else 1
              where t = f-c
          utr = upptr (rows r) (cols r) * r |~| r

upperTriang' m = rows m == 1 || down |~| z
    where down = fromList $ concat $ zipWith drop [1..] (toLists (ctrans m))
          z = constant 0 (dim down)

hessProp m = m |~| p <> h <> ctrans p && unitary p && upperHessenberg h
    where (p,h) = hess m

schurProp1 m = m |~| u <> s <> ctrans u && unitary u && upperTriang s
    where (u,s) = schur m

schurProp2 m = m |~| u <> s <> ctrans u && unitary u && upperHessenberg s -- fixme
    where (u,s) = schur m

cholProp m = m |~| ctrans c <> c && upperTriang c
    where c = chol m
          -- pos = positiveDefinite m

expmDiagProp m = expm (logm m) :~ 7 ~: complex m
    where logm = matFunc log

-- reference multiply
mulH a b = fromLists [[ doth ai bj | bj <- toColumns b] | ai <- toRows a ]
    where doth u v = sum $ zipWith (*) (toList u) (toList v)

multProp1 (a,b) = a <> b |~| mulH a b

multProp2 (a,b) = ctrans (a <> b) |~| ctrans b <> ctrans a

linearSolveProp f m = f m m |~| ident (rows m)

linearSolveProp2 f (a,x) = not wc `trivial` (not wc || a <> f a b |~| b)
    where q = min (rows a) (cols a)
          b = a <> x
          wc = rank a == q
