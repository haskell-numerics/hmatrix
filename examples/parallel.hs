import System(getArgs)
import Numeric.LinearAlgebra
import Control.Parallel.Strategies
import System.Time

inParallel = parMap rwhnf id

parMul x y = fromBlocks [inParallel[x <> y1, x <> y2]]
    where p = cols y `div` 2
          (y1,y2) = splitColumnsAt p y

main = do
    n <- (read . head) `fmap` getArgs
    let m = ident n :: Matrix Double
    time $ print $ vectorMax $ takeDiag $ parMul m m
    time $ print $ vectorMax $ takeDiag $ m <> m

a = (2><3) [1..6::Double]
b = (3><4) [1..12::Double]

splitRowsAt p m    = (takeRows p m, dropRows p m)
splitColumnsAt p m = (takeColumns p m, dropColumns p m)

time act = do
    t0 <- getClockTime
    act
    t1 <- getClockTime
    print $ tdSec $ normalizeTimeDiff $ diffClockTimes t1 t0
