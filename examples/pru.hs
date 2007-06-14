--{-# OPTIONS_GHC  #-}
--module Main where

import Data.Packed.Internal
import Data.Packed.Internal.Vector
import Data.Packed.Internal.Matrix
import Data.Packed.Internal.Tensor
import Data.Packed.Matrix
import GSL.Vector
import LAPACK

import Complex
import Numeric(showGFloat)
import Data.List(transpose,intersperse,sort)
import Foreign.Storable


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
          toList (cdat a) == toList (cdat b)

mulC a b = multiply RowMajor a b
mulF a b = multiply ColumnMajor a b

cc = mulC ac bf
cf = mulF af bc

r = mulC cc (trans cf)

rd = (2><2)
 [ 27736.0,  65356.0
 , 65356.0, 154006.0 ]



main = do
    print $ r |=| rd
    print $ foldl part t [("p",1),("q",0),("r",2)]
    print $ foldl part t [("p",1),("r",2),("q",0)]
    print $ foldl part t $ reverse [("p",1),("r",2),("q",0)]

t = T [(4,(Covariant,"p")),(2,(Covariant,"q")),(3,(Contravariant,"r"))] $ fromList [1..24::Double]



t1 = T [(4,(Covariant,"p")),(4,(Contravariant,"q")),(2,(Covariant,"r"))] $ fromList [1..32::Double]
t2 = T [(4,(Covariant,"p")),(4,(Contravariant,"q"))] $ fromList [1..16::Double]






delta i j | i==j      = 1
          | otherwise = 0

e i n = fromList [ delta k i | k <- [1..n]]

diagl = diag.fromList


tensorFromVector idx v = T {dims = [(dim v,idx)], ten = v}
tensorFromMatrix idxr idxc m = T {dims = [(rows m,idxr),(cols m,idxc)], ten = cdat m}

td = tensorFromMatrix (Contravariant,"i") (Covariant,"j") $ diagl [1..4] :: Tensor Double

tn = tensorFromMatrix (Contravariant,"i") (Covariant,"j") $ (2><3) [1..6] :: Tensor Double
tt = tensorFromMatrix (Contravariant,"i") (Covariant,"j") $ (2><3) [1..6] :: Tensor Double

tq = T [(3,(Covariant,"p")),(2,(Covariant,"q")),(2,(Covariant,"r"))] $ fromList [11 .. 22] :: Tensor Double

r1 = contraction tt "j" tq "p"
r1' = contraction' tt "j" tq "p"

pru = do
    mapM_ (putStrLn.shdims.dims.normal) (contractions t1 t2)
    let t1 = contraction tt "i" tq "q"
    print $ normal t1
    print $ foldl part t1 [("j",0),("p'",1),("r'",1)]
    let t2 = contraction' tt "i" tq "q"
    print $ normal t2
    print $ foldl part t2 [("j",0),("p'",1),("r'",1)]
    let t1 = contraction tq "q" tt "i"
    print $ normal t1
    print $ foldl part t1 [("j'",0),("p",1),("r",1)]
    let t2 = contraction' tq "q" tt "i"
    print $ normal t2
    print $ foldl part t2 [("j'",0),("p",1),("r",1)]

