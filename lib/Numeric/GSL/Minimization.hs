{-# OPTIONS_GHC -fglasgow-exts #-}
-----------------------------------------------------------------------------
{- |
Module      :  Numeric.GSL.Minimization
Copyright   :  (c) Alberto Ruiz 2006
License     :  GPL-style

Maintainer  :  Alberto Ruiz (aruiz at um dot es)
Stability   :  provisional
Portability :  uses ffi

Minimization of a multidimensional function Minimization of a multidimensional function using some of the algorithms described in:

<http://www.gnu.org/software/gsl/manual/html_node/Multidimensional-Minimization.html>

-}
-----------------------------------------------------------------------------
module Numeric.GSL.Minimization (
    minimizeConjugateGradient,
    minimizeNMSimplex
) where


import Data.Packed.Internal
import Data.Packed.Matrix
import Foreign
import Complex


-------------------------------------------------------------------------

{- | The method of Nelder and Mead, implemented by /gsl_multimin_fminimizer_nmsimplex/. The gradient of the function is not required. This is the example in the GSL manual:

@minimize f xi = minimizeNMSimplex f xi (replicate (length xi) 1) 1e-2 100
\ 
f [x,y] = 10*(x-1)^2 + 20*(y-2)^2 + 30
\ 
main = do
    let (s,p) = minimize f [5,7]
    print s
    print p
\ 
\> main
[0.9920430849306285,1.9969168063253164]
0. 512.500    1.082 6.500    5.
 1. 290.625    1.372 5.250    4.
 2. 290.625    1.372 5.250    4.
 3. 252.500    1.372 5.500    1.
 4. 101.406    1.823 2.625 3.500
 5. 101.406    1.823 2.625 3.500
 6.     60.    1.823    0.    3.
 7.  42.275    1.303 2.094 1.875
 8.  42.275    1.303 2.094 1.875
 9.  35.684    1.026 0.258 1.906
10.  35.664    0.804 0.588 2.445
11.  30.680    0.467 1.258 2.025
12.  30.680    0.356 1.258 2.025
13.  30.539    0.285 1.093 1.849
14.  30.137    0.168 0.883 2.004
15.  30.137    0.123 0.883 2.004
16.  30.090    0.100 0.958 2.060
17.  30.005 6.051e-2 1.022 2.004
18.  30.005 4.249e-2 1.022 2.004
19.  30.005 4.249e-2 1.022 2.004
20.  30.005 2.742e-2 1.022 2.004
21.  30.005 2.119e-2 1.022 2.004
22.  30.001 1.530e-2 0.992 1.997
23.  30.001 1.259e-2 0.992 1.997
24.  30.001 7.663e-3 0.992 1.997@

The path to the solution can be graphically shown by means of:

@'GSL.Plot.mplot' $ drop 3 ('toColumns' p)@

-}
minimizeNMSimplex :: ([Double] -> Double) -- ^ function to minimize
          -> [Double]            -- ^ starting point
          -> [Double]            -- ^ sizes of the initial search box
          -> Double              -- ^ desired precision of the solution
          -> Int                 -- ^ maximum number of iterations allowed
          -> ([Double], Matrix Double)
          -- ^ solution vector, and the optimization trajectory followed by the algorithm
minimizeNMSimplex f xi sz tol maxit = unsafePerformIO $ do
    let xiv = fromList xi
        szv = fromList sz
        n   = dim xiv
    fp <- mkVecfun (iv (f.toList))
    rawpath <- createMIO maxit (n+3)
                         (c_minimizeNMSimplex fp tol maxit // vec xiv // vec szv) 
                         "minimizeNMSimplex" [xiv,szv]
    let it = round (rawpath @@> (maxit-1,0))
        path = takeRows it rawpath
        [sol] = toLists $ dropRows (it-1) path
    freeHaskellFunPtr fp
    return (drop 3 sol, path)


foreign import ccall "gsl-aux.h minimize"
    c_minimizeNMSimplex:: FunPtr (Int -> Ptr Double -> Double) -> Double -> Int
                       -> TVVM

----------------------------------------------------------------------------------

{- | The Fletcher-Reeves conjugate gradient algorithm /gsl_multimin_fminimizer_conjugate_fr/. This is the example in the GSL manual:

@minimize = minimizeConjugateGradient 1E-2 1E-4 1E-3 30
f [x,y] = 10*(x-1)^2 + 20*(y-2)^2 + 30
\ 
df [x,y] = [20*(x-1), 40*(y-2)]
\  
main = do
    let (s,p) = minimize f df [5,7]
    print s
    print p
\ 
\> main
[1.0,2.0]
 0. 687.848 4.996 6.991
 1. 683.555 4.989 6.972
 2. 675.013 4.974 6.935
 3. 658.108 4.944 6.861
 4. 625.013 4.885 6.712
 5. 561.684 4.766 6.415
 6. 446.467 4.528 5.821
 7. 261.794 4.053 4.632
 8.  75.498 3.102 2.255
 9.  67.037 2.852 1.630
10.  45.316 2.191 1.762
11.  30.186 0.869 2.026
12.     30.    1.    2.@

The path to the solution can be graphically shown by means of:

@'GSL.Plot.mplot' $ drop 2 ('toColumns' p)@

-}     
minimizeConjugateGradient ::
       Double        -- ^ initial step size
    -> Double        -- ^ minimization parameter   
    -> Double        -- ^ desired precision of the solution (gradient test)
    -> Int           -- ^ maximum number of iterations allowed
    -> ([Double] -> Double) -- ^ function to minimize
    -> ([Double] -> [Double])      -- ^ gradient
    -> [Double]             -- ^ starting point
    -> ([Double], Matrix Double)        -- ^ solution vector, and the optimization trajectory followed by the algorithm
minimizeConjugateGradient istep minimpar tol maxit f df xi = unsafePerformIO $ do
    let xiv = fromList xi
        n = dim xiv
        f' = f . toList
        df' = (fromList . df . toList)
    fp <- mkVecfun (iv f')
    dfp <- mkVecVecfun (aux_vTov df')
    rawpath <- createMIO maxit (n+2)
                         (c_minimizeConjugateGradient fp dfp istep minimpar tol maxit // vec xiv)
                         "minimizeDerivV" [xiv]
    let it = round (rawpath @@> (maxit-1,0))
        path = takeRows it rawpath
        sol = toList $ cdat $ dropColumns 2 $ dropRows (it-1) path
    freeHaskellFunPtr fp
    freeHaskellFunPtr dfp
    return (sol,path)


foreign import ccall "gsl-aux.h minimizeWithDeriv"
    c_minimizeConjugateGradient :: FunPtr (Int -> Ptr Double -> Double)
                                -> FunPtr (Int -> Ptr Double -> Ptr Double -> IO ())
                                -> Double -> Double -> Double -> Int
                                -> TVM

---------------------------------------------------------------------
iv :: (Vector Double -> Double) -> (Int -> Ptr Double -> Double)
iv f n p = f (createV n copy "iv" []) where
    copy n q = do 
        copyArray q p n
        return 0

-- | conversion of Haskell functions into function pointers that can be used in the C side
foreign import ccall "wrapper"
    mkVecfun :: (Int -> Ptr Double -> Double)
             -> IO( FunPtr (Int -> Ptr Double -> Double))

-- | another required conversion
foreign import ccall "wrapper"
    mkVecVecfun :: (Int -> Ptr Double -> Ptr Double -> IO ())
                -> IO (FunPtr (Int -> Ptr Double -> Ptr Double->IO()))

aux_vTov :: (Vector Double -> Vector Double) -> (Int -> Ptr Double -> Ptr Double -> IO())
aux_vTov f n p r = g where
    V {fptr = pr, ptr = t} = f x
    x = createV n copy "aux_vTov" []
    copy n q = do
        copyArray q p n
        return 0
    g = withForeignPtr pr $ \_ -> copyArray r t n

--------------------------------------------------------------------

createV n fun msg ptrs = unsafePerformIO $ do
    r <- createVector n
    fun // vec r // check msg ptrs
    return r

createM r c fun msg ptrs = unsafePerformIO $ do
    r <- createMatrix RowMajor r c
    fun // mat cdat r // check msg ptrs
    return r

createMIO r c fun msg ptrs = do
    r <- createMatrix RowMajor r c
    fun // mat cdat r // check msg ptrs
    return r
