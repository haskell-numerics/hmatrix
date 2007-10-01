-- initial check for the polynomial model example
import Numeric.LinearAlgebra


prepSyst :: Int -> Matrix Double -> (Matrix Double, Vector Double)
prepSyst n d = (a,b) where
    [x,b] = toColumns d
    a = fromColumns $ 1+0*x : map (x^) [1 .. n]

main = do
    dat <- readMatrix `fmap` readFile "data.txt"
    let (a,b) = prepSyst 3 dat
    putStr "Coefficient matrix:\n"
    dispR 3 a
    putStr "Desired values:\n"
    print b
    putStr "\nLeast Squares Solution:\n"
    print $ pinv a <> b -- equivalent to: linearSolveLSR a (fromColumns [b])
