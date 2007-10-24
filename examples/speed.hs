{- speed tests

$ ghc --make -O speed
In my machine:
$ ./speed 5 100000 1
(3><3)
 [ -1.7877285611885504e-2, 0.0,    -0.9998401885597121
 ,                    0.0, 1.0,                    0.0
 ,     0.9998401885597168, 0.0, -1.7877285611891697e-2 ]
0.29 CPU seconds

GNU-Octave:
./speed.m
  -0.017877255967426   0.000000000000000  -0.999840189089781
   0.000000000000000   1.000000000000000   0.000000000000000
   0.999840189089763   0.000000000000000  -0.017877255967417
9.69 seconds

-}

import Numeric.LinearAlgebra
import System
import Data.List(foldl1')
import System.CPUTime
import Text.Printf
import Debug.Trace

debug x = trace (show x) x

timing act = do
    t0 <- getCPUTime
    act
    t1 <- getCPUTime
    printf "%.2f CPU seconds\n" $ (fromIntegral ((t1 - t0) `div` (10^10)) / 100 :: Double)

op a b = trans $ (trans a) <> (trans b)

op2 a b = trans $ (trans a) + (trans b)

rot' :: Double -> Matrix Double
rot' a = matrix [[ c,0,s],
                 [ 0,1,0],
                 [-s,0,c]]
    where c = cos a
          s = sin a
          matrix = fromLists

rot :: Double -> Matrix Double
rot a = (3><3) [ c,0,s
               , 0,1,0
               ,-s,0,c ]
    where c = cos a
          s = sin a


fun n r = foldl1' (<>) (map r [0,delta..1]) where delta = 1 /(fromIntegral n-1)


main = do
    args <- getArgs
    let [p,n,d] = map read args
    let ms = replicate n ((ident d :: Matrix Double))
    let mz = replicate n (diag (constant (0::Double) d))
    timing $ case p of
        0 -> print $ foldl1' (<>) ms
        1 -> print $ foldl1' (<>) (map trans ms)
        2 -> print $ foldl1' op ms
        3 -> print $ foldl1' op2 mz
        4 -> print $ fun n rot'
        5 -> print $ fun n rot
