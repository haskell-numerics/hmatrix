{- |
Module      :  Numeric.GSL.Root
Copyright   :  (c) Alberto Ruiz 2009
License     :  GPL

Maintainer  :  Alberto Ruiz (aruiz at um dot es)
Stability   :  provisional
Portability :  uses ffi

Multidimensional root finding.

<http://www.gnu.org/software/gsl/manual/html_node/Multidimensional-Root_002dFinding.html>

The example in the GSL manual:

@import Numeric.GSL
import Numeric.LinearAlgebra(format)
import Text.Printf(printf)

rosenbrock a b [x,y] = [ a*(1-x), b*(y-x^2) ]

disp = putStrLn . format \"  \" (printf \"%.3f\")

main = do
    let (sol,path) = root Hybrids 1E-7 30 (rosenbrock 1 10) [-10,-5]
    print sol
    disp path

\> main
[1.0,1.0]
 0.000  -10.000  -5.000  11.000  -1050.000
 1.000   -3.976  24.827   4.976     90.203
 2.000   -3.976  24.827   4.976     90.203
 3.000   -3.976  24.827   4.976     90.203
 4.000   -1.274  -5.680   2.274    -73.018
 5.000   -1.274  -5.680   2.274    -73.018
 6.000    0.249   0.298   0.751      2.359
 7.000    0.249   0.298   0.751      2.359
 8.000    1.000   0.878  -0.000     -1.218
 9.000    1.000   0.989  -0.000     -0.108
10.000    1.000   1.000   0.000      0.000
@

-}
-----------------------------------------------------------------------------

module Numeric.GSL.Root (
    root, RootMethod(..)
) where

import Data.Packed.Internal
import Data.Packed.Matrix
import Foreign
import Foreign.C.Types(CInt)

-------------------------------------------------------------------------

data RootMethod = Hybrids
                | Hybrid
                | DNewton
                | Broyden
                deriving (Enum,Eq,Show)

-- | Nonlinear multidimensional root finding using algorithms that do not require 
-- any derivative information to be supplied by the user.
-- Any derivatives needed are approximated by finite differences.
root :: RootMethod
     -> Double                     -- ^ maximum residual
     -> Int                        -- ^ maximum number of iterations allowed
     -> ([Double] -> [Double])     -- ^ function to minimize
     -> [Double]                   -- ^ starting point
     -> ([Double], Matrix Double)  -- ^ solution vector and optimization path

root method epsabs maxit fun xinit = rootGen (fi (fromEnum method)) fun xinit epsabs maxit

rootGen m f xi epsabs maxit = unsafePerformIO $ do
    let xiv = fromList xi
        n   = dim xiv
    fp <- mkVecVecfun (aux_vTov (fromList.f.toList))
    rawpath <- withVector xiv $ \xiv' ->
                   createMIO maxit (2*n+1)
                         (c_root m fp epsabs (fi maxit) // xiv')
                         "root"
    let it = round (rawpath @@> (maxit-1,0))
        path = takeRows it rawpath
        [sol] = toLists $ dropRows (it-1) path
    freeHaskellFunPtr fp
    return (take n $ drop 1 sol, path)


foreign import ccall "root"
    c_root:: CInt -> FunPtr (CInt -> Ptr Double -> Ptr Double -> IO ()) -> Double -> CInt -> TVM

---------------------------------------------------------------------

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

createV n fun msg = unsafePerformIO $ do
    r <- createVector n
    app1 fun vec r msg
    return r

createMIO r c fun msg = do
    res <- createMatrix RowMajor r c
    app1 fun mat res msg
    return res
