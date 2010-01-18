-- $ runhaskell parallel.hs 2000

import System(getArgs)
import Numeric.LinearAlgebra
import Control.Parallel.Strategies
import System.Time

inParallel = parMap rwhnf id
--  rdeepseq (or older rnf) not needed in this case

-- matrix product decomposed into p parallel subtasks
parMul p x y = fromBlocks [ inParallel ( map (x <>) ys ) ]
    where ys = splitColumns p y

-- x <||> y = fromColumns . inParallel . map (x <>) . toColumns $ y

main = do
    n <- (read . head) `fmap` getArgs
    let m = ident n :: Matrix Double
    time $ print $ vectorMax $ takeDiag $ m <> m
--    time $ print $ vectorMax $ takeDiag $ m <||> m
    time $ print $ vectorMax $ takeDiag $ parMul 2 m m
    time $ print $ vectorMax $ takeDiag $ parMul 4 m m
    time $ print $ vectorMax $ takeDiag $ parMul 8 m m

time act = do
    t0 <- getClockTime
    act
    t1 <- getClockTime
    print $ tdSec $ normalizeTimeDiff $ diffClockTimes t1 t0

splitColumns n m = splitColumns' (f n (cols m)) m
    where
    splitColumns' [] m = []
    splitColumns' ((a,b):rest) m = subMatrix (0,a) (rows m, b-a+1) m : splitColumns' rest m

    f :: Int -> Int -> [(Int,Int)]
    f n c = zip ks (map pred $ tail ks)
        where ks = map round $ toList $ linspace (fromIntegral n+1) (0,fromIntegral c)

splitRowsAt p m    = (takeRows p m, dropRows p m)
splitColumnsAt p m = (takeColumns p m, dropColumns p m)
