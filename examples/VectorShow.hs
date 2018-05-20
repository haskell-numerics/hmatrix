{-# LANGUAGE DataKinds #-}

module Main
    ( main
    ) where

import Numeric.LinearAlgebra.Static
import qualified Numeric.LinearAlgebra as LA
import qualified Numeric.GSL.Minimization as Min

u :: R 4
u = vec4 10 20 30 40

v :: R 5
v = vec2 5 0 & 0 & 3 & 7

b :: L 4 3
b = matrix
    [ 2, 0,-1
    , 1, 1, 7
    , 5, 3, 1
    , 2, 8, 0 ] :: L 4 3

w :: R 10
w = vector [1..10] :: R 10

f :: [Double] -> Double
f [x,y] = 10*(x-1)^(2::Int) + 20*(y-2)^(2::Int) + 30
f _     = error "f only defined for exactly 2 elements"

main :: IO ()
main = do
    print u
    print v
    print b
    print w
    print $ diag u
    print (eye + 2 :: Sq 4)
    print $ LA.diag (LA.fromList [1,2,3 :: Double])
    --
    let (s,p) = Min.minimize Min.NMSimplex2 1E-2 30 [1,1] f [5,7]
    print s
    print p
