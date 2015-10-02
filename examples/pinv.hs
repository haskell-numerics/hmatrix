import Numeric.LinearAlgebra
import Text.Printf(printf)

expand :: Int -> Vector R -> Matrix R
expand n x = fromColumns $ map (x^) [0 .. n]

polynomialModel :: Vector R -> Vector R -> Int
                -> (Vector R -> Vector R)
polynomialModel x y n = f where
    f z = expand n z #> ws
    ws  = expand n x <\> y

main = do
    [x,y] <- toColumns <$> loadMatrix "data.txt"
    let pol = polynomialModel x y
    let view = [x, y, pol 1 x, pol 2 x, pol 3 x]
    putStrLn $ "  x      y      p 1    p 2    p 3"
    putStrLn $ format "  " (printf "%.2f") $ fromColumns view

