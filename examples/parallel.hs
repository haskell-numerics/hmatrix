-- $ runhaskell parallel.hs 2000

import System(getArgs)
import Numeric.LinearAlgebra
import Control.Parallel.Strategies
import System.Time

inParallel = parMap rwhnf id


-- matrix product decomposed into p parallel subtasks
parMul p x y = fromBlocks [ inParallel ( map (x <>) ys ) ]
    where [ys] = toBlocksEvery (rows y) (cols y `div` p) y

main = do
    n <- (read . head) `fmap` getArgs
    let m = ident n :: Matrix Double
    time $ print $ vectorMax $ takeDiag $ m <> m
    time $ print $ vectorMax $ takeDiag $ parMul 2 m m
    time $ print $ vectorMax $ takeDiag $ parMul 4 m m
    time $ print $ vectorMax $ takeDiag $ parMul 8 m m

time act = do
    t0 <- getClockTime
    act
    t1 <- getClockTime
    print $ tdSec $ normalizeTimeDiff $ diffClockTimes t1 t0
