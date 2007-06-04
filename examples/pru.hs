--{-# OPTIONS_GHC  #-}
--module Main where

import Data.Packed.Internal
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

main = print $ r |=| rd
