{-# LANGUAGE BangPatterns #-}

-- $ ghc --make -O2 benchmarks.hs


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

main = sequence_ [bench1,bench2,bench4,
                  bench5 1000000 3,
                  bench5 100000 50]

w :: Vector Double
w = constant 1 5000000
w2 = 1 * w

v = flatten $ ident 500 :: Vector Double


bench1 = do
    time $ print$ vectorMax (w+w2) -- evaluate it
    putStrLn "Sum of a vector with 5M doubles:"
    print $ vectorMax v  -- evaluate it
--     time $ printf "     BLAS: %.2f: " $ sumVB w
    time $ printf "   Haskell: %.2f: " $ sumVH w
    time $ printf "      BLAS: %.2f: " $ w <.> w2
    time $ printf "   Haskell: %.2f: " $ sumVH w
    time $ printf "    innerH: %.2f: " $ innerH w w2
    time $ printf "foldVector: %.2f: " $ sumVector w
    let getPos k s = if k `mod` 500 < 200 && w@>k > 0 then k:s else s
    putStrLn "foldLoop for element selection:"
    time $ print $ (`divMod` 500) $ maximum $ foldLoop getPos [] (dim w)
    putStrLn "constant 5M:"
    time $ print $ constant (1::Double) 5000001 @> 7
    time $ print $ constant           i 5000001 @> 7
    time $ print $ conj (constant i 5000001) @> 7
    putStrLn "some zips:"
    time $ print $ (w / w2) @> 7
    time $ print $ (zipVector (/) w w2) @> 7
    putStrLn "some folds:"
    let t = constant (1::Double) 5000002
    print $ t @> 7
    time $ print $ foldVector max (t@>0) t
    time $ print $ vectorMax t
    time $ print $ sqrt $ foldVector (\v s -> v*v+s) 0 t
    time $ print $ pnorm PNorm2 t

sumVH v = go (d - 1) 0
     where
       d = dim v
       go :: Int -> Double -> Double
       go 0 s = s + (v @> 0)
       go !j !s = go (j - 1) (s + (v @> j))

innerH u v = go (d - 1) 0
     where
       d = min (dim u) (dim v)
       go :: Int -> Double -> Double
       go 0 s = s + (u @> 0) * (v @> 0)
       go !j !s = go (j - 1) (s + (u @> j) * (v @> j))


-- sumVector = foldVectorG (\k v s -> v k + s) 0.0
sumVector = foldVector (+) 0.0

--------------------------------------------------------------------------------

bench2 = do
    putStrLn "-------------------------------------------------------"
    putStrLn "Multiplication of 1M different 3x3 matrices:"
--    putStrLn "from [[]]"
--    time $ print $ manymult (10^6) rot'
--    putStrLn "from (3><3) []"
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
          -- angles = map (k*) [0..n']
          -- n' = fromIntegral n - 1
          -- k  = recip n'

--------------------------------------------------------------------------------

bench4 = do
    putStrLn "-------------------------------------------------------"
    putStrLn "1000x1000 inverse"
    let a = ident 1000 :: Matrix Double
    let b = 2*a
    print $ vectorMax $ flatten (a+b) -- evaluate it
    time $ print $ vectorMax $ flatten $ linearSolve a b

--------------------------------------------------------------------------------

op1 a b = a <> trans b
op2 a b = a  + trans b

timep = time . print . vectorMax . flatten

bench5 n d = do
    putStrLn "-------------------------------------------------------"
    putStrLn "transpose in add"
    let ms = replicate n ((ident d :: Matrix Double))
    timep $ foldl1' (+)  ms
    timep $ foldl1' op2  ms
    putStrLn "-------------------------------------------------------"
    putStrLn "transpose in multiply"

    timep $ foldl1' (<>) ms
    timep $ foldl1' op1  ms
