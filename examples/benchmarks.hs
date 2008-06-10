{-# OPTIONS -fbang-patterns #-}

-- compile as:
-- ghc --make -O2 -optc-O2 -fvia-C  benchmarks.hs -o benchmarks

import Numeric.LinearAlgebra
import System.Time
import System.CPUTime
import Text.Printf


time act = do
    t0 <- getCPUTime
    act
    t1 <- getCPUTime
    putStrLn $ show ((t1-t0) `div` 10^10) ++ " cs"

time' act = do
    t0 <- getClockTime
    act
    t1 <- getClockTime
    putStrLn $ timeDiffToString $ normalizeTimeDiff $ diffClockTimes t1 t0

--------------------------------------------------------------------------------

main = sequence_ [bench1]

bench1 = do
    putStrLn "sum of a vector with 30 million doubles:"
    time $ printf "     BLAS: %.2f: " $ sumV sumVB 30000000
    time $ printf "  Haskell: %.2f: " $ sumV sumVH 30000000
    time $ printf "     BLAS: %.2f: " $ sumV sumVB 30000000
    time $ printf "  Haskell: %.2f: " $ sumV sumVH 30000000

sumV f n = f (constant (1::Double) n)

sumVB v = constant 1 (dim v) <.> v

sumVH v = go (d - 1) 0
     where
       d = dim v
       go :: Int -> Double -> Double
       go 0 s = s + (v @> 0)
       go !j !s = go (j - 1) (s + (v @> j))
