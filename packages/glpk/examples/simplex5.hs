import Numeric.LinearProgramming

-- This is a linear program from the paper "Picking vs. Guessing Secrets: A Game-theoretic Analysis"

gamma = 100000 :: Double
sigma = 1 :: Double
n = 64 :: Int
cost_fun :: Int -> Double
cost_fun i = (fromIntegral i) / (fromIntegral n)
size_fun :: Int -> Double
size_fun i = 2^(fromIntegral i)

prob = Minimize $ map cost_fun [1..n]
bnds = [i :&: (0,1) | i <- [1..n]]

constr1 = [[1 # i | i <- [1..n]] :==: 1] ++
          [[1/(size_fun i) # i,
            -1/(size_fun (i+1)) # i+1] :>=: 0 | i <- [1..n-1]] ++
          [(
            [gamma#i | i <- [1..k]] ++
            (concat [[sigma*(size_fun i) # j | j <- [1..i-1]] | i <- [1..k]]) ++
            [((size_fun i) - 1)/2 # i | i <- [1..k]])
           :<=: (sigma * (foldr (+) 0 (map size_fun [1..k]))) | k <- [1..n]]

main = do
  print $ simplex prob (General constr1) bnds -- NoFeasible
  print $ exact   prob (General constr1) bnds -- solution found
