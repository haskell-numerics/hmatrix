-- the multidimensional minimization example in the GSL manual
import GSL
import LinearAlgebra
import Graphics.Plot

-- the function to be minimized 
f [x,y] = 10*(x-1)^2 + 20*(y-2)^2 + 30

-- its gradient
df [x,y] = [20*(x-1), 40*(y-2)]

-- the conjugate gradient method
minimizeCG = minimizeConjugateGradient 1E-2 1E-4 1E-3 30

-- a minimization algorithm which does not require the gradient
minimizeS f xi = minimizeNMSimplex f xi (replicate (length xi) 1) 1E-2 100

-- Numerical estimation of the gradient
gradient f v = [partialDerivative k f v | k <- [0 .. length v -1]]

partialDerivative n f v = fst (derivCentral 0.01 g (v!!n)) where
    g x = f (concat [a,x:b])
    (a,_:b) = splitAt n v

main = do
    -- conjugate gradient with true gradient
    let (s,p) = minimizeCG f df [5,7]
    print s -- solution
    dispR 2 p -- evolution of the algorithm
    let [x,y] = drop 2 (toColumns p)
    mplot [x,y]  -- path from the starting point to the solution

    -- conjugate gradient with estimated gradient
    let (s,p) = minimizeCG f (gradient f) [5,7]
    print s
    dispR 2 p
    mplot $ drop 2 (toColumns p)

    -- without gradient, using the NM Simplex method
    let (s,p) = minimizeS f [5,7]
    print s
    dispR 2 p
    mplot $ drop 3 (toColumns p)
