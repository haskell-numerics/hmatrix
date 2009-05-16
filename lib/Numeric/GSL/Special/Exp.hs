------------------------------------------------------------
{- |
Module      :  Numeric.GSL.Special.Exp
Copyright   :  (c) Alberto Ruiz 2006
License     :  GPL-style
Maintainer  :  Alberto Ruiz (aruiz at um dot es)
Stability   :  provisional
Portability :  uses ffi

Wrappers for selected functions described at:

<http://www.google.com/search?q=gsl_sf_exp.h&as_sitesearch=www.gnu.org/software/gsl/manual&btnI=Lucky>

-}
------------------------------------------------------------

module Numeric.GSL.Special.Exp(
  exp_e
, Numeric.GSL.Special.Exp.exp
, exp_e10_e
, exp_mult_e
, exp_mult
, exp_mult_e10_e
, expm1_e
, expm1
, exprel_e
, exprel
, exprel_2_e
, exprel_2
, exprel_n_e
, exprel_n
, exprel_n_CF_e
, exp_err_e
, exp_err_e10_e
, exp_mult_err_e
, exp_mult_err_e10_e
) where

import Foreign(Ptr)
import Foreign.C.Types(CInt)
import Numeric.GSL.Special.Internal

-- | wrapper for int gsl_sf_exp_e(double x,gsl_sf_result* result);
--
--   <http://www.google.com/search?q=gsl_sf_exp_e&as_sitesearch=www.gnu.org/software/gsl/manual&btnI=Lucky>
exp_e :: Double -> (Double,Double)
exp_e x = createSFR "exp_e" $ gsl_sf_exp_e x
foreign import ccall "gsl_sf_exp_e" gsl_sf_exp_e :: Double -> Ptr () -> IO CInt

-- | wrapper for double gsl_sf_exp(double x);
--
--   <http://www.google.com/search?q=gsl_sf_exp&as_sitesearch=www.gnu.org/software/gsl/manual&btnI=Lucky>
exp :: Double -> Double
exp = gsl_sf_exp
foreign import ccall "gsl_sf_exp" gsl_sf_exp :: Double -> Double

-- | wrapper for int gsl_sf_exp_e10_e(double x,gsl_sf_result_e10* result);
--
--   <http://www.google.com/search?q=gsl_sf_exp_e10_e&as_sitesearch=www.gnu.org/software/gsl/manual&btnI=Lucky>
exp_e10_e :: Double -> (Double,Int,Double)
exp_e10_e x = createSFR_E10 "exp_e10_e" $ gsl_sf_exp_e10_e x
foreign import ccall "gsl_sf_exp_e10_e" gsl_sf_exp_e10_e :: Double -> Ptr () -> IO CInt

-- | wrapper for int gsl_sf_exp_mult_e(double x,double y,gsl_sf_result* result);
--
--   <http://www.google.com/search?q=gsl_sf_exp_mult_e&as_sitesearch=www.gnu.org/software/gsl/manual&btnI=Lucky>
exp_mult_e :: Double -> Double -> (Double,Double)
exp_mult_e x y = createSFR "exp_mult_e" $ gsl_sf_exp_mult_e x y
foreign import ccall "gsl_sf_exp_mult_e" gsl_sf_exp_mult_e :: Double -> Double -> Ptr () -> IO CInt

-- | wrapper for double gsl_sf_exp_mult(double x,double y);
--
--   <http://www.google.com/search?q=gsl_sf_exp_mult&as_sitesearch=www.gnu.org/software/gsl/manual&btnI=Lucky>
exp_mult :: Double -> Double -> Double
exp_mult = gsl_sf_exp_mult
foreign import ccall "gsl_sf_exp_mult" gsl_sf_exp_mult :: Double -> Double -> Double

-- | wrapper for int gsl_sf_exp_mult_e10_e(double x,double y,gsl_sf_result_e10* result);
--
--   <http://www.google.com/search?q=gsl_sf_exp_mult_e10_e&as_sitesearch=www.gnu.org/software/gsl/manual&btnI=Lucky>
exp_mult_e10_e :: Double -> Double -> (Double,Int,Double)
exp_mult_e10_e x y = createSFR_E10 "exp_mult_e10_e" $ gsl_sf_exp_mult_e10_e x y
foreign import ccall "gsl_sf_exp_mult_e10_e" gsl_sf_exp_mult_e10_e :: Double -> Double -> Ptr () -> IO CInt

-- | wrapper for int gsl_sf_expm1_e(double x,gsl_sf_result* result);
--
--   <http://www.google.com/search?q=gsl_sf_expm1_e&as_sitesearch=www.gnu.org/software/gsl/manual&btnI=Lucky>
expm1_e :: Double -> (Double,Double)
expm1_e x = createSFR "expm1_e" $ gsl_sf_expm1_e x
foreign import ccall "gsl_sf_expm1_e" gsl_sf_expm1_e :: Double -> Ptr () -> IO CInt

-- | wrapper for double gsl_sf_expm1(double x);
--
--   <http://www.google.com/search?q=gsl_sf_expm1&as_sitesearch=www.gnu.org/software/gsl/manual&btnI=Lucky>
expm1 :: Double -> Double
expm1 = gsl_sf_expm1
foreign import ccall "gsl_sf_expm1" gsl_sf_expm1 :: Double -> Double

-- | wrapper for int gsl_sf_exprel_e(double x,gsl_sf_result* result);
--
--   <http://www.google.com/search?q=gsl_sf_exprel_e&as_sitesearch=www.gnu.org/software/gsl/manual&btnI=Lucky>
exprel_e :: Double -> (Double,Double)
exprel_e x = createSFR "exprel_e" $ gsl_sf_exprel_e x
foreign import ccall "gsl_sf_exprel_e" gsl_sf_exprel_e :: Double -> Ptr () -> IO CInt

-- | wrapper for double gsl_sf_exprel(double x);
--
--   <http://www.google.com/search?q=gsl_sf_exprel&as_sitesearch=www.gnu.org/software/gsl/manual&btnI=Lucky>
exprel :: Double -> Double
exprel = gsl_sf_exprel
foreign import ccall "gsl_sf_exprel" gsl_sf_exprel :: Double -> Double

-- | wrapper for int gsl_sf_exprel_2_e(double x,gsl_sf_result* result);
--
--   <http://www.google.com/search?q=gsl_sf_exprel_2_e&as_sitesearch=www.gnu.org/software/gsl/manual&btnI=Lucky>
exprel_2_e :: Double -> (Double,Double)
exprel_2_e x = createSFR "exprel_2_e" $ gsl_sf_exprel_2_e x
foreign import ccall "gsl_sf_exprel_2_e" gsl_sf_exprel_2_e :: Double -> Ptr () -> IO CInt

-- | wrapper for double gsl_sf_exprel_2(double x);
--
--   <http://www.google.com/search?q=gsl_sf_exprel_2&as_sitesearch=www.gnu.org/software/gsl/manual&btnI=Lucky>
exprel_2 :: Double -> Double
exprel_2 = gsl_sf_exprel_2
foreign import ccall "gsl_sf_exprel_2" gsl_sf_exprel_2 :: Double -> Double

-- | wrapper for int gsl_sf_exprel_n_e(int n,double x,gsl_sf_result* result);
--
--   <http://www.google.com/search?q=gsl_sf_exprel_n_e&as_sitesearch=www.gnu.org/software/gsl/manual&btnI=Lucky>
exprel_n_e :: CInt -> Double -> (Double,Double)
exprel_n_e n x = createSFR "exprel_n_e" $ gsl_sf_exprel_n_e n x
foreign import ccall "gsl_sf_exprel_n_e" gsl_sf_exprel_n_e :: CInt -> Double -> Ptr () -> IO CInt

-- | wrapper for double gsl_sf_exprel_n(int n,double x);
--
--   <http://www.google.com/search?q=gsl_sf_exprel_n&as_sitesearch=www.gnu.org/software/gsl/manual&btnI=Lucky>
exprel_n :: CInt -> Double -> Double
exprel_n = gsl_sf_exprel_n
foreign import ccall "gsl_sf_exprel_n" gsl_sf_exprel_n :: CInt -> Double -> Double

-- | wrapper for int gsl_sf_exprel_n_CF_e(double n,double x,gsl_sf_result* result);
--
--   <http://www.google.com/search?q=gsl_sf_exprel_n_CF_e&as_sitesearch=www.gnu.org/software/gsl/manual&btnI=Lucky>
exprel_n_CF_e :: Double -> Double -> (Double,Double)
exprel_n_CF_e n x = createSFR "exprel_n_CF_e" $ gsl_sf_exprel_n_CF_e n x
foreign import ccall "gsl_sf_exprel_n_CF_e" gsl_sf_exprel_n_CF_e :: Double -> Double -> Ptr () -> IO CInt

-- | wrapper for int gsl_sf_exp_err_e(double x,double dx,gsl_sf_result* result);
--
--   <http://www.google.com/search?q=gsl_sf_exp_err_e&as_sitesearch=www.gnu.org/software/gsl/manual&btnI=Lucky>
exp_err_e :: Double -> Double -> (Double,Double)
exp_err_e x dx = createSFR "exp_err_e" $ gsl_sf_exp_err_e x dx
foreign import ccall "gsl_sf_exp_err_e" gsl_sf_exp_err_e :: Double -> Double -> Ptr () -> IO CInt

-- | wrapper for int gsl_sf_exp_err_e10_e(double x,double dx,gsl_sf_result_e10* result);
--
--   <http://www.google.com/search?q=gsl_sf_exp_err_e10_e&as_sitesearch=www.gnu.org/software/gsl/manual&btnI=Lucky>
exp_err_e10_e :: Double -> Double -> (Double,Int,Double)
exp_err_e10_e x dx = createSFR_E10 "exp_err_e10_e" $ gsl_sf_exp_err_e10_e x dx
foreign import ccall "gsl_sf_exp_err_e10_e" gsl_sf_exp_err_e10_e :: Double -> Double -> Ptr () -> IO CInt

-- | wrapper for int gsl_sf_exp_mult_err_e(double x,double dx,double y,double dy,gsl_sf_result* result);
--
--   <http://www.google.com/search?q=gsl_sf_exp_mult_err_e&as_sitesearch=www.gnu.org/software/gsl/manual&btnI=Lucky>
exp_mult_err_e :: Double -> Double -> Double -> Double -> (Double,Double)
exp_mult_err_e x dx y dy = createSFR "exp_mult_err_e" $ gsl_sf_exp_mult_err_e x dx y dy
foreign import ccall "gsl_sf_exp_mult_err_e" gsl_sf_exp_mult_err_e :: Double -> Double -> Double -> Double -> Ptr () -> IO CInt

-- | wrapper for int gsl_sf_exp_mult_err_e10_e(double x,double dx,double y,double dy,gsl_sf_result_e10* result);
--
--   <http://www.google.com/search?q=gsl_sf_exp_mult_err_e10_e&as_sitesearch=www.gnu.org/software/gsl/manual&btnI=Lucky>
exp_mult_err_e10_e :: Double -> Double -> Double -> Double -> (Double,Int,Double)
exp_mult_err_e10_e x dx y dy = createSFR_E10 "exp_mult_err_e10_e" $ gsl_sf_exp_mult_err_e10_e x dx y dy
foreign import ccall "gsl_sf_exp_mult_err_e10_e" gsl_sf_exp_mult_err_e10_e :: Double -> Double -> Double -> Double -> Ptr () -> IO CInt
