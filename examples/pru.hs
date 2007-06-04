--{-# OPTIONS_GHC  #-}
--module Main where

import Data.Packed.Internal.Vector
import Data.Packed.Internal.Matrix
import Data.Packed.Internal.Tensor

import Complex
import Numeric(showGFloat)
import Data.List(transpose,intersperse)
import Foreign.Storable

r >< c = f where
    f l | dim v == r*c = matrixFromVector RowMajor c v
        | otherwise    = error $ "inconsistent list size = "
                                 ++show (dim v) ++"in ("++show r++"><"++show c++")"
        where v = fromList l

r >|< c = f where
    f l | dim v == r*c = matrixFromVector ColumnMajor c v
        | otherwise    = error $ "inconsistent list size = "
                                 ++show (dim v) ++"in ("++show r++"><"++show c++")"
        where v = fromList l



vr = fromList [1..15::Double]
vc = fromList (map (\x->x :+ (x+1)) [1..15::Double])

mi = (2 >< 3) [1 .. 6::Int]
mz = (2 >< 3) [1,2,3,4,5,6:+(1::Double)]

ac = (2><3) [1 .. 6::Double]
bc = (3><4) [7 .. 18::Double]

af = (2>|<3) [1,4,2,5,3,6::Double]
bf = (3>|<4) [7,11,15,8,12,16,9,13,17,10,14,18::Double]


a |=| b = rows a == rows b &&
          cols a == cols b &&
          toList (dat a) == toList (dat b)

mulC a b = multiply RowMajor a b
mulF a b = multiply ColumnMajor a b

cc = mulC ac bf
cf = mulF af bc

r = mulC cc (trans cf)

rd = (2><2)
 [  43492.0,  50572.0
 , 102550.0, 119242.0 ]

main = do
    print $ r |=| rd
    print $ foldl part t [("p",1),("r",2),("q",0)]

t = T [(4,(Covariant,"p")),(2,(Covariant,"q")),(3,(Contravariant,"r"))] $ fromList [1..24::Double]


findIdx name t = ((d1,d2),m) where
    (d1,d2) = span (\(_,(_,n)) -> n /=name) (dims t)
    c = product (map fst (tail d2))
    m = matrixFromVector RowMajor c (ten t)


putFirstIdx name t =
    if null d1
        then (nd,m)
        else (nd,m')
    where ((d1,d2),m) = findIdx name t
          m' = trans $ matrixFromVector RowMajor (fst $ head d2) $ dat m
          nd = d2++d1

part t (name,k) = if k<0 || k>=l
                    then error $ "part "++show (name,k)++" out of range in "++show t
                    else T {dims = ds, ten = toRows m !! k}
    where (d:ds,m) = putFirstIdx name t
          (l,_) = d

parts t name = map f (toRows m) 
    where (d:ds,m) = putFirstIdx name t
          (l,_) = d
          f t = T {dims=ds, ten=t}

t1 = T [(4,(Covariant,"p")),(4,(Contravariant,"q")),(2,(Covariant,"r"))] $ fromList [1..32::Double]
t2 = T [(4,(Covariant,"p")),(4,(Contravariant,"q"))] $ fromList [1..16::Double]

--contract1 t name1 name2 = map head $ zipWith drop [0..] (map (flip parts name2) (parts t name1))

--sumT ls = foldl (zipWith (+)) [0,0..] (map (toList.ten) ls)

on f g = \x y -> f (g x) (g y)