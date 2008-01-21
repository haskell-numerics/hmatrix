------------------------------------------------------------
{- |
Module      :  Numeric.GSL.Special.Expint
Copyright   :  (c) Alberto Ruiz 2006
License     :  GPL-style
Maintainer  :  Alberto Ruiz (aruiz at um dot es)
Stability   :  provisional
Portability :  uses ffi

Wrappers for selected functions described at:

<http://www.google.com/search?q=gsl_sf_expint.h&as_sitesearch=www.gnu.org/software/gsl/manual&btnI=Lucky>

-}
------------------------------------------------------------

module Numeric.GSL.Special.Expint(
  expint_E1_e
, expint_E1
, expint_E2_e
, expint_E2
, expint_E1_scaled_e
, expint_E1_scaled
, expint_E2_scaled_e
, expint_E2_scaled
, expint_Ei_e
, expint_Ei
, expint_Ei_scaled_e
, expint_Ei_scaled
, shi_e
, shi
, chi_e
, chi
, expint_3_e
, expint_3
, si_e
, si
, ci_e
, ci
, atanint_e
, atanint
) where

import Foreign(Ptr)
import Foreign.C.Types(CInt)
import Numeric.GSL.Special.Internal

-- | wrapper for int gsl_sf_expint_E1_e(double x,gsl_sf_result* result);
--
--   <http://www.google.com/search?q=gsl_sf_expint_E1_e&as_sitesearch=www.gnu.org/software/gsl/manual&btnI=Lucky>
expint_E1_e :: Double -> (Double,Double)
expint_E1_e x = createSFR "expint_E1_e" $ gsl_sf_expint_E1_e x
foreign import ccall "expint.h gsl_sf_expint_E1_e" gsl_sf_expint_E1_e :: Double -> Ptr () -> IO CInt

-- | wrapper for double gsl_sf_expint_E1(double x);
--
--   <http://www.google.com/search?q=gsl_sf_expint_E1&as_sitesearch=www.gnu.org/software/gsl/manual&btnI=Lucky>
expint_E1 :: Double -> Double
expint_E1 = gsl_sf_expint_E1
foreign import ccall "expint.h gsl_sf_expint_E1" gsl_sf_expint_E1 :: Double -> Double

-- | wrapper for int gsl_sf_expint_E2_e(double x,gsl_sf_result* result);
--
--   <http://www.google.com/search?q=gsl_sf_expint_E2_e&as_sitesearch=www.gnu.org/software/gsl/manual&btnI=Lucky>
expint_E2_e :: Double -> (Double,Double)
expint_E2_e x = createSFR "expint_E2_e" $ gsl_sf_expint_E2_e x
foreign import ccall "expint.h gsl_sf_expint_E2_e" gsl_sf_expint_E2_e :: Double -> Ptr () -> IO CInt

-- | wrapper for double gsl_sf_expint_E2(double x);
--
--   <http://www.google.com/search?q=gsl_sf_expint_E2&as_sitesearch=www.gnu.org/software/gsl/manual&btnI=Lucky>
expint_E2 :: Double -> Double
expint_E2 = gsl_sf_expint_E2
foreign import ccall "expint.h gsl_sf_expint_E2" gsl_sf_expint_E2 :: Double -> Double

-- | wrapper for int gsl_sf_expint_E1_scaled_e(double x,gsl_sf_result* result);
--
--   <http://www.google.com/search?q=gsl_sf_expint_E1_scaled_e&as_sitesearch=www.gnu.org/software/gsl/manual&btnI=Lucky>
expint_E1_scaled_e :: Double -> (Double,Double)
expint_E1_scaled_e x = createSFR "expint_E1_scaled_e" $ gsl_sf_expint_E1_scaled_e x
foreign import ccall "expint.h gsl_sf_expint_E1_scaled_e" gsl_sf_expint_E1_scaled_e :: Double -> Ptr () -> IO CInt

-- | wrapper for double gsl_sf_expint_E1_scaled(double x);
--
--   <http://www.google.com/search?q=gsl_sf_expint_E1_scaled&as_sitesearch=www.gnu.org/software/gsl/manual&btnI=Lucky>
expint_E1_scaled :: Double -> Double
expint_E1_scaled = gsl_sf_expint_E1_scaled
foreign import ccall "expint.h gsl_sf_expint_E1_scaled" gsl_sf_expint_E1_scaled :: Double -> Double

-- | wrapper for int gsl_sf_expint_E2_scaled_e(double x,gsl_sf_result* result);
--
--   <http://www.google.com/search?q=gsl_sf_expint_E2_scaled_e&as_sitesearch=www.gnu.org/software/gsl/manual&btnI=Lucky>
expint_E2_scaled_e :: Double -> (Double,Double)
expint_E2_scaled_e x = createSFR "expint_E2_scaled_e" $ gsl_sf_expint_E2_scaled_e x
foreign import ccall "expint.h gsl_sf_expint_E2_scaled_e" gsl_sf_expint_E2_scaled_e :: Double -> Ptr () -> IO CInt

-- | wrapper for double gsl_sf_expint_E2_scaled(double x);
--
--   <http://www.google.com/search?q=gsl_sf_expint_E2_scaled&as_sitesearch=www.gnu.org/software/gsl/manual&btnI=Lucky>
expint_E2_scaled :: Double -> Double
expint_E2_scaled = gsl_sf_expint_E2_scaled
foreign import ccall "expint.h gsl_sf_expint_E2_scaled" gsl_sf_expint_E2_scaled :: Double -> Double

-- | wrapper for int gsl_sf_expint_Ei_e(double x,gsl_sf_result* result);
--
--   <http://www.google.com/search?q=gsl_sf_expint_Ei_e&as_sitesearch=www.gnu.org/software/gsl/manual&btnI=Lucky>
expint_Ei_e :: Double -> (Double,Double)
expint_Ei_e x = createSFR "expint_Ei_e" $ gsl_sf_expint_Ei_e x
foreign import ccall "expint.h gsl_sf_expint_Ei_e" gsl_sf_expint_Ei_e :: Double -> Ptr () -> IO CInt

-- | wrapper for double gsl_sf_expint_Ei(double x);
--
--   <http://www.google.com/search?q=gsl_sf_expint_Ei&as_sitesearch=www.gnu.org/software/gsl/manual&btnI=Lucky>
expint_Ei :: Double -> Double
expint_Ei = gsl_sf_expint_Ei
foreign import ccall "expint.h gsl_sf_expint_Ei" gsl_sf_expint_Ei :: Double -> Double

-- | wrapper for int gsl_sf_expint_Ei_scaled_e(double x,gsl_sf_result* result);
--
--   <http://www.google.com/search?q=gsl_sf_expint_Ei_scaled_e&as_sitesearch=www.gnu.org/software/gsl/manual&btnI=Lucky>
expint_Ei_scaled_e :: Double -> (Double,Double)
expint_Ei_scaled_e x = createSFR "expint_Ei_scaled_e" $ gsl_sf_expint_Ei_scaled_e x
foreign import ccall "expint.h gsl_sf_expint_Ei_scaled_e" gsl_sf_expint_Ei_scaled_e :: Double -> Ptr () -> IO CInt

-- | wrapper for double gsl_sf_expint_Ei_scaled(double x);
--
--   <http://www.google.com/search?q=gsl_sf_expint_Ei_scaled&as_sitesearch=www.gnu.org/software/gsl/manual&btnI=Lucky>
expint_Ei_scaled :: Double -> Double
expint_Ei_scaled = gsl_sf_expint_Ei_scaled
foreign import ccall "expint.h gsl_sf_expint_Ei_scaled" gsl_sf_expint_Ei_scaled :: Double -> Double

-- | wrapper for int gsl_sf_Shi_e(double x,gsl_sf_result* result);
--
--   <http://www.google.com/search?q=gsl_sf_Shi_e&as_sitesearch=www.gnu.org/software/gsl/manual&btnI=Lucky>
shi_e :: Double -> (Double,Double)
shi_e x = createSFR "shi_e" $ gsl_sf_Shi_e x
foreign import ccall "expint.h gsl_sf_Shi_e" gsl_sf_Shi_e :: Double -> Ptr () -> IO CInt

-- | wrapper for double gsl_sf_Shi(double x);
--
--   <http://www.google.com/search?q=gsl_sf_Shi&as_sitesearch=www.gnu.org/software/gsl/manual&btnI=Lucky>
shi :: Double -> Double
shi = gsl_sf_Shi
foreign import ccall "expint.h gsl_sf_Shi" gsl_sf_Shi :: Double -> Double

-- | wrapper for int gsl_sf_Chi_e(double x,gsl_sf_result* result);
--
--   <http://www.google.com/search?q=gsl_sf_Chi_e&as_sitesearch=www.gnu.org/software/gsl/manual&btnI=Lucky>
chi_e :: Double -> (Double,Double)
chi_e x = createSFR "chi_e" $ gsl_sf_Chi_e x
foreign import ccall "expint.h gsl_sf_Chi_e" gsl_sf_Chi_e :: Double -> Ptr () -> IO CInt

-- | wrapper for double gsl_sf_Chi(double x);
--
--   <http://www.google.com/search?q=gsl_sf_Chi&as_sitesearch=www.gnu.org/software/gsl/manual&btnI=Lucky>
chi :: Double -> Double
chi = gsl_sf_Chi
foreign import ccall "expint.h gsl_sf_Chi" gsl_sf_Chi :: Double -> Double

-- | wrapper for int gsl_sf_expint_3_e(double x,gsl_sf_result* result);
--
--   <http://www.google.com/search?q=gsl_sf_expint_3_e&as_sitesearch=www.gnu.org/software/gsl/manual&btnI=Lucky>
expint_3_e :: Double -> (Double,Double)
expint_3_e x = createSFR "expint_3_e" $ gsl_sf_expint_3_e x
foreign import ccall "expint.h gsl_sf_expint_3_e" gsl_sf_expint_3_e :: Double -> Ptr () -> IO CInt

-- | wrapper for double gsl_sf_expint_3(double x);
--
--   <http://www.google.com/search?q=gsl_sf_expint_3&as_sitesearch=www.gnu.org/software/gsl/manual&btnI=Lucky>
expint_3 :: Double -> Double
expint_3 = gsl_sf_expint_3
foreign import ccall "expint.h gsl_sf_expint_3" gsl_sf_expint_3 :: Double -> Double

-- | wrapper for int gsl_sf_Si_e(double x,gsl_sf_result* result);
--
--   <http://www.google.com/search?q=gsl_sf_Si_e&as_sitesearch=www.gnu.org/software/gsl/manual&btnI=Lucky>
si_e :: Double -> (Double,Double)
si_e x = createSFR "si_e" $ gsl_sf_Si_e x
foreign import ccall "expint.h gsl_sf_Si_e" gsl_sf_Si_e :: Double -> Ptr () -> IO CInt

-- | wrapper for double gsl_sf_Si(double x);
--
--   <http://www.google.com/search?q=gsl_sf_Si&as_sitesearch=www.gnu.org/software/gsl/manual&btnI=Lucky>
si :: Double -> Double
si = gsl_sf_Si
foreign import ccall "expint.h gsl_sf_Si" gsl_sf_Si :: Double -> Double

-- | wrapper for int gsl_sf_Ci_e(double x,gsl_sf_result* result);
--
--   <http://www.google.com/search?q=gsl_sf_Ci_e&as_sitesearch=www.gnu.org/software/gsl/manual&btnI=Lucky>
ci_e :: Double -> (Double,Double)
ci_e x = createSFR "ci_e" $ gsl_sf_Ci_e x
foreign import ccall "expint.h gsl_sf_Ci_e" gsl_sf_Ci_e :: Double -> Ptr () -> IO CInt

-- | wrapper for double gsl_sf_Ci(double x);
--
--   <http://www.google.com/search?q=gsl_sf_Ci&as_sitesearch=www.gnu.org/software/gsl/manual&btnI=Lucky>
ci :: Double -> Double
ci = gsl_sf_Ci
foreign import ccall "expint.h gsl_sf_Ci" gsl_sf_Ci :: Double -> Double

-- | wrapper for int gsl_sf_atanint_e(double x,gsl_sf_result* result);
--
--   <http://www.google.com/search?q=gsl_sf_atanint_e&as_sitesearch=www.gnu.org/software/gsl/manual&btnI=Lucky>
atanint_e :: Double -> (Double,Double)
atanint_e x = createSFR "atanint_e" $ gsl_sf_atanint_e x
foreign import ccall "expint.h gsl_sf_atanint_e" gsl_sf_atanint_e :: Double -> Ptr () -> IO CInt

-- | wrapper for double gsl_sf_atanint(double x);
--
--   <http://www.google.com/search?q=gsl_sf_atanint&as_sitesearch=www.gnu.org/software/gsl/manual&btnI=Lucky>
atanint :: Double -> Double
atanint = gsl_sf_atanint
foreign import ccall "expint.h gsl_sf_atanint" gsl_sf_atanint :: Double -> Double
