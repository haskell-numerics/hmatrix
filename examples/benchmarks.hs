{-# OPTIONS -fbang-patterns #-}

-- compile as:
-- ghc --make -O2 -optc-O2 -fvia-C benchmarks.hs
-- ghc --make -O benchmarks.hs

import Numeric.LinearAlgebra
import System.Time
import System.CPUTime
import Text.Printf
import Data.List(foldl1')


time act = do
    t0 <- getCPUTime
    act
    t1 <- getCPUTime
    printf "%.3f s CPU\n" $ (fromIntegral (t1 - t0) / (10^12 :: Double)) :: IO ()

--------------------------------------------------------------------------------

main = sequence_ [bench1,bench2]

w :: Vector Double
w = constant 1 30000000

bench1 = do
    putStrLn "Sum of a vector with 30M doubles:"
    print$ vectorMax w -- evaluate it
    time $ printf "     BLAS: %.2f: " $ sumVB w
    time $ printf "  Haskell: %.2f: " $ sumVH w
    time $ printf "     BLAS: %.2f: " $ sumVB w
    time $ printf "  Haskell: %.2f: " $ sumVH w

sumVB v = constant 1 (dim v) <.> v

sumVH v = go (d - 1) 0
     where
       d = dim v
       go :: Int -> Double -> Double
       go 0 s = s + (v @> 0)
       go !j !s = go (j - 1) (s + (v @> j))

--------------------------------------------------------------------------------

bench2 = do
    putStrLn "-------------------------------------------------------"
    putStrLn "Multiplication of 1M different 3x3 matrices:"
--     putStrLn "from [[]]"
--     time $ print $ fun (10^6) rot'
--     putStrLn "from []"
    time $ print $ manymult (10^6) rot
    print $ cos (10^6/2)


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

manymult n r = foldl1' (<>) (map r angles)
    where angles = toList $ linspace n (0,1)
