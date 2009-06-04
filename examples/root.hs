-- root finding examples
import Numeric.GSL
import Numeric.LinearAlgebra
import Graphics.Plot
import Text.Printf(printf)

rosenbrock a b [x,y] = [ a*(1-x), b*(y-x^2) ]

disp = putStrLn . format "  " (printf "%.3f")

-- Numerical estimation of the gradient
gradient f v = [partialDerivative k f v | k <- [0 .. length v -1]]

partialDerivative n f v = fst (derivCentral 0.01 g (v!!n)) where
    g x = f (concat [a,x:b])
    (a,_:b) = splitAt n v

test method = do
    print method
    let (s,p) = root method 1E-7 30 (rosenbrock 1 10) [-10,-5]
    print s -- solution
    disp p -- evolution of the algorithm
--    let [x,y] = tail (toColumns p)
--    mplot [x,y]  -- path from the starting point to the solution

main = do
    test Hybrids
    test Hybrid
    test DNewton
    test Broyden
