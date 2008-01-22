{-# OPTIONS #-}
-----------------------------------------------------------------------------
{- |
Module      :  Numeric.GSL.Integration
Copyright   :  (c) Alberto Ruiz 2006
License     :  GPL-style

Maintainer  :  Alberto Ruiz (aruiz at um dot es)
Stability   :  provisional
Portability :  uses ffi

Numerical integration routines.

<http://www.gnu.org/software/gsl/manual/html_node/Numerical-Integration.html#Numerical-Integration>
-}
-----------------------------------------------------------------------------

module Numeric.GSL.Integration (
    integrateQNG,
    integrateQAGS
) where

import Foreign
import Foreign.C.Types(CInt)
import Data.Packed.Internal(check,(//))


{- | conversion of Haskell functions into function pointers that can be used in the C side
-}
foreign import ccall "wrapper" mkfun:: (Double -> Ptr() -> Double) -> IO( FunPtr (Double -> Ptr() -> Double)) 

--------------------------------------------------------------------
{- | Numerical integration using /gsl_integration_qags/ (adaptive integration with singularities). For example:

@\> let quad = integrateQAGS 1E-9 1000 
\> let f a x = x**(-0.5) * log (a*x)
\> quad (f 1) 0 1
(-3.999999999999974,4.871658632055187e-13)@
 
-}

integrateQAGS :: Double               -- ^ precision (e.g. 1E-9)
                 -> Int               -- ^ size of auxiliary workspace (e.g. 1000)
                 -> (Double -> Double) -- ^ function to be integrated on the interval (a,b)
                 -> Double           -- ^ a
                 -> Double           -- ^ b
                 -> (Double, Double) -- ^ result of the integration and error
integrateQAGS prec n f a b = unsafePerformIO $ do
    r <- malloc
    e <- malloc
    fp <- mkfun (\x _ -> f x) 
    c_integrate_qags fp a b prec (fromIntegral n) r e // check "integrate_qags"
    vr <- peek r
    ve <- peek e
    let result = (vr,ve)
    free r
    free e
    freeHaskellFunPtr fp
    return result

foreign import ccall "gsl-aux.h integrate_qags" 
 c_integrate_qags :: FunPtr (Double-> Ptr() -> Double) -> Double -> Double -> Double -> CInt
                     -> Ptr Double -> Ptr Double -> IO CInt

-----------------------------------------------------------------
{- | Numerical integration using /gsl_integration_qng/ (useful for fast integration of smooth functions). For example:

@\> let quad = integrateQNG 1E-6 
\> quad (\\x -> 4\/(1+x*x)) 0 1 
(3.141592653589793,3.487868498008632e-14)@
 
-}

integrateQNG :: Double               -- ^ precision (e.g. 1E-9)
                 -> (Double -> Double) -- ^ function to be integrated on the interval (a,b)
                 -> Double           -- ^ a
                 -> Double           -- ^ b
                 -> (Double, Double) -- ^ result of the integration and error
integrateQNG prec f a b = unsafePerformIO $ do
    r <- malloc
    e <- malloc
    fp <- mkfun (\x _ -> f x) 
    c_integrate_qng fp a b prec r e  // check "integrate_qng"
    vr <- peek r
    ve <- peek e
    let result = (vr,ve)
    free r
    free e
    freeHaskellFunPtr fp
    return result

foreign import ccall "gsl-aux.h integrate_qng" 
 c_integrate_qng :: FunPtr (Double-> Ptr() -> Double) -> Double -> Double -> Double 
                    -> Ptr Double -> Ptr Double -> IO CInt
