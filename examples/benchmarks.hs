{-# OPTIONS -fbang-patterns #-}

-- compile as:
-- ghc --make -O2 -optc-O2 -fvia-C  benchmarks.hs -o benchmarks
-- ghc --make -O benchmarks.hs -o benchmarks

import Numeric.LinearAlgebra
import System.Time
import System.CPUTime
import Text.Printf
import Data.List(foldl1')


time act = do
    t0 <- getCPUTime
    act
    t1 <- getCPUTime
    printf "%.2f CPU seconds\n" $ (fromIntegral ((t1 - t0) `div` (10^10)) / 100 :: Double) :: IO ()

time' act = do
    t0 <- getClockTime
    act
    t1 <- getClockTime
    putStrLn $ timeDiffToString $ normalizeTimeDiff $ diffClockTimes t1 t0

--------------------------------------------------------------------------------

main = sequence_ [bench1,bench2]

w :: Vector Double
w = constant 1 30000000

bench1 = do
    putStrLn "sum of a vector with 30 million doubles:"
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
    putStrLn "multiplication of one million different 3x3 matrices"
    putStrLn "from [[]]"
    time $ print $ fun (10^6) rot'
    putStrLn "from []"
    time $ print $ fun (10^6) rot
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

fun n r = foldl1' (<>) (map r angles)
    where angles = toList $ linspace n (0,1)
