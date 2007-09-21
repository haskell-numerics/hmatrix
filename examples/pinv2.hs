-- MSE polynomial model using the pseudoinverse
import LinearAlgebra
import Graphics.Plot

expand :: Int -> Vector Double -> Matrix Double
expand n x = fromColumns $ constant 1 (dim x): map (x^) [1 .. n]

polynomialModel :: Matrix Double -> Int -> (Vector Double -> Vector Double)
polynomialModel d n = f where
    f z = expand n z <> ws 
    ws  = pinv a <> b
    [x,b] = toColumns d 
    a = expand n x

mk fv = f where
    f x = fv (fromList [x]) @> 0

main = do
    d <- readMatrix `fmap` readFile "data.txt"
    let [x,y] = toColumns d
    let pol = polynomialModel d
    let view = [x, y, pol 1 x, pol 2 x, pol 3 x]
    dispR 2 $ fromColumns view
    mplot view
    let f = mk (pol 2)
    print (f 2.5)
