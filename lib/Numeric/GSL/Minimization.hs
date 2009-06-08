{-# OPTIONS_GHC -fglasgow-exts #-}
-----------------------------------------------------------------------------
{- |
Module      :  Numeric.GSL.Minimization
Copyright   :  (c) Alberto Ruiz 2006-9
License     :  GPL-style

Maintainer  :  Alberto Ruiz (aruiz at um dot es)
Stability   :  provisional
Portability :  uses ffi

Minimization of a multidimensional function using some of the algorithms described in:

<http://www.gnu.org/software/gsl/manual/html_node/Multidimensional-Minimization.html>

The example in the GSL manual:

@

f [x,y] = 10*(x-1)^2 + 20*(y-2)^2 + 30

main = do
    let (s,p) = minimize NMSimplex2 1E-2 30 [1,1] f [5,7]
    print s
    print p

\> main
[0.9920430849306288,1.9969168063253182]
 0.000  512.500  1.130  6.500  5.000
 1.000  290.625  1.409  5.250  4.000
 2.000  290.625  1.409  5.250  4.000
 3.000  252.500  1.409  5.500  1.000
 ...
22.000   30.001  0.013  0.992  1.997
23.000   30.001  0.008  0.992  1.997
@

The path to the solution can be graphically shown by means of:

@'Graphics.Plot.mplot' $ drop 3 ('toColumns' p)@

Taken from the GSL manual:

The vector Broyden-Fletcher-Goldfarb-Shanno (BFGS) algorithm is a quasi-Newton method which builds up an approximation to the second derivatives of the function f using the difference between successive gradient vectors. By combining the first and second derivatives the algorithm is able to take Newton-type steps towards the function minimum, assuming quadratic behavior in that region.

The bfgs2 version of this minimizer is the most efficient version available, and is a faithful implementation of the line minimization scheme described in Fletcher's Practical Methods of Optimization, Algorithms 2.6.2 and 2.6.4. It supercedes the original bfgs routine and requires substantially fewer function and gradient evaluations. The user-supplied tolerance tol corresponds to the parameter \sigma used by Fletcher. A value of 0.1 is recommended for typical use (larger values correspond to less accurate line searches).

The nmsimplex2 version is a new O(N) implementation of the earlier O(N^2) nmsimplex minimiser. It calculates the size of simplex as the rms distance of each vertex from the center rather than the mean distance, which has the advantage of allowing a linear update.

-}

-----------------------------------------------------------------------------
module Numeric.GSL.Minimization (
    minimize, MinimizeMethod(..),
    minimizeD, MinimizeMethodD(..),
    minimizeNMSimplex,
    minimizeConjugateGradient,
    minimizeVectorBFGS2
) where


import Data.Packed.Internal
import Data.Packed.Matrix
import Foreign
import Foreign.C.Types(CInt)

------------------------------------------------------------------------

{-# DEPRECATED minimizeNMSimplex "use minimize NMSimplex2 eps maxit sizes f xi" #-}
minimizeNMSimplex f xi szs eps maxit = minimize NMSimplex eps maxit szs f xi

{-# DEPRECATED minimizeConjugateGradient "use minimizeD ConjugateFR eps maxit step tol f g xi" #-}
minimizeConjugateGradient step tol eps maxit f g xi = minimizeD ConjugateFR eps maxit step tol f g xi

{-# DEPRECATED minimizeVectorBFGS2 "use minimizeD VectorBFGS2 eps maxit step tol f g xi" #-}
minimizeVectorBFGS2 step tol eps maxit f g xi = minimizeD VectorBFGS2 eps maxit step tol f g xi

-------------------------------------------------------------------------

data MinimizeMethod = NMSimplex
                    | NMSimplex2
                    deriving (Enum,Eq,Show)

-- | Minimization without derivatives.
minimize :: MinimizeMethod
         -> Double              -- ^ desired precision of the solution (size test)
         -> Int                 -- ^ maximum number of iterations allowed
         -> [Double]            -- ^ sizes of the initial search box
         -> ([Double] -> Double) -- ^ function to minimize
         -> [Double]            -- ^ starting point
         -> ([Double], Matrix Double) -- ^ solution vector and optimization path

minimize method = minimizeGen (fi (fromEnum method))

data MinimizeMethodD = ConjugateFR
                     | ConjugatePR
                     | VectorBFGS
                     | VectorBFGS2
                     | SteepestDescent
                     deriving (Enum,Eq,Show)

-- | Minimization with derivatives.
minimizeD :: MinimizeMethodD
    -> Double                 -- ^ desired precision of the solution (gradient test)
    -> Int                    -- ^ maximum number of iterations allowed
    -> Double                 -- ^ size of the first trial step
    -> Double                 -- ^ tol (precise meaning depends on method)
    -> ([Double] -> Double)   -- ^ function to minimize
    -> ([Double] -> [Double]) -- ^ gradient
    -> [Double]               -- ^ starting point
    -> ([Double], Matrix Double) -- ^ solution vector and optimization path

minimizeD method = minimizeDGen (fi (fromEnum method))

-------------------------------------------------------------------------



minimizeGen method eps maxit sz f xi = unsafePerformIO $ do
    let xiv = fromList xi
        szv = fromList sz
        n   = dim xiv
    fp <- mkVecfun (iv (f.toList))
    rawpath <- ww2 withVector xiv withVector szv $ \xiv' szv' ->
                   createMIO maxit (n+3)
                         (c_minimize method fp eps (fi maxit) // xiv' // szv')
                         "minimize"
    let it = round (rawpath @@> (maxit-1,0))
        path = takeRows it rawpath
        [sol] = toLists $ dropRows (it-1) path
    freeHaskellFunPtr fp
    return (drop 3 sol, path)


foreign import ccall "gsl-aux.h minimize"
    c_minimize:: CInt -> FunPtr (CInt -> Ptr Double -> Double) -> Double -> CInt -> TVVM

----------------------------------------------------------------------------------



minimizeDGen method eps maxit istep tol f df xi = unsafePerformIO $ do
    let xiv = fromList xi
        n = dim xiv
        f' = f . toList
        df' = (fromList . df . toList)
    fp <- mkVecfun (iv f')
    dfp <- mkVecVecfun (aux_vTov df')
    rawpath <- withVector xiv $ \xiv' ->
                    createMIO maxit (n+2)
                         (c_minimizeWithDeriv method fp dfp istep tol eps (fi maxit) // xiv')
                         "minimizeDerivV"
    let it = round (rawpath @@> (maxit-1,0))
        path = takeRows it rawpath
        sol = toList $ cdat $ dropColumns 2 $ dropRows (it-1) path
    freeHaskellFunPtr fp
    freeHaskellFunPtr dfp
    return (sol,path)

foreign import ccall "gsl-aux.h minimizeD"
    c_minimizeWithDeriv :: CInt -> FunPtr (CInt -> Ptr Double -> Double)
                                -> FunPtr (CInt -> Ptr Double -> Ptr Double -> IO ())
                                -> Double -> Double -> Double -> CInt
                                -> TVM

---------------------------------------------------------------------
iv :: (Vector Double -> Double) -> (CInt -> Ptr Double -> Double)
iv f n p = f (createV (fromIntegral n) copy "iv") where
    copy n' q = do
        copyArray q p (fromIntegral n')
        return 0

-- | conversion of Haskell functions into function pointers that can be used in the C side
foreign import ccall "wrapper"
    mkVecfun :: (CInt -> Ptr Double -> Double)
             -> IO( FunPtr (CInt -> Ptr Double -> Double))

-- | another required conversion
foreign import ccall "wrapper"
    mkVecVecfun :: (CInt -> Ptr Double -> Ptr Double -> IO ())
                -> IO (FunPtr (CInt -> Ptr Double -> Ptr Double->IO()))

aux_vTov :: (Vector Double -> Vector Double) -> (CInt -> Ptr Double -> Ptr Double -> IO())
aux_vTov f n p r = g where
    V {fptr = pr} = f x
    x = createV (fromIntegral n) copy "aux_vTov"
    copy n' q = do
        copyArray q p (fromIntegral n')
        return 0
    g = withForeignPtr pr $ \p' -> copyArray r p' (fromIntegral n)

--------------------------------------------------------------------

createV n fun msg = unsafePerformIO $ do
    r <- createVector n
    app1 fun vec r msg
    return r

createMIO r c fun msg = do
    res <- createMatrix RowMajor r c
    app1 fun mat res msg
    return res
