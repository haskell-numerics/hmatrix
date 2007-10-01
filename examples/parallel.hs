import System(getArgs)
import Numeric.LinearAlgebra
import Control.Parallel.Strategies

work k = vectorMax s
    where (_,s,_) = svd (ident k :: Matrix Double)

main = do
    args <- (read . head) `fmap` getArgs
    print $ sum $ parMap rnf work args