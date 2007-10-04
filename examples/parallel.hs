import System(getArgs)
import Numeric.LinearAlgebra
import Control.Parallel.Strategies
import System.Time

inParallel = parMap rwhnf id

parMul x y = fromBlocks [[ay],[by]]
    where p = rows x `div` 2
          a = takeRows p x
          b = dropRows p x
          [ay,by] = inParallel [a<>y,b<>y]

main = do
    n <- (read . head) `fmap` getArgs
    let m = ident n :: Matrix Double
    time $ print $ vectorMax $ takeDiag $ parMul m m
    time $ print $ vectorMax $ takeDiag $ m <> m

a = (2><3) [1..6::Double]
b = (3><4) [1..12::Double]

time act = do
    t0 <- getClockTime
    act
    t1 <- getClockTime
    print $ tdSec $ normalizeTimeDiff $ diffClockTimes t1 t0
