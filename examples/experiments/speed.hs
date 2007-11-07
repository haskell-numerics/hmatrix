{- speed tests

GNU-Octave (see speed.m in this folder):

./speed.m
  -0.017877255967426   0.000000000000000  -0.999840189089781
   0.000000000000000   1.000000000000000   0.000000000000000
   0.999840189089763   0.000000000000000  -0.017877255967417
9.69 seconds

Mathematica:

rot[a_]:={{ Cos[a], 0, Sin[a]},
          { 0,      1,      0},
          { -Sin[a],0,Cos[a]}}//N

test := Timing[
    n = 100000;
    angles = Range[0.,n-1]/(n-1);
    Fold[Dot,IdentityMatrix[3],rot/@angles] // MatrixForm
]

2.08013 Second
     {{\(-0.017877255967432837`\), "0.`", \(-0.9998401890898042`\)},
      {"0.`", "1.`", "0.`"},
      {"0.9998401890898042`", "0.`", \(-0.017877255967432837`\)}}

$ ghc --make -O speed

$ ./speed 5 100000 1
(3><3)
 [ -1.7877255967425523e-2, 0.0,    -0.9998401890897632
 ,                    0.0, 1.0,                    0.0
 ,      0.999840189089781, 0.0, -1.7877255967416586e-2 ]
0.33 CPU seconds

cos 50000 = -0.0178772559665563
sin 50000 = -0.999840189089790

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
    printf "%.2f CPU seconds\n" $ (fromIntegral ((t1 - t0) `div` (10^10)) / 100 :: Double) :: IO ()

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

fun n r = foldl1' (<>) (map r angles)
    where angles = toList $ linspace n (0,1)

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
