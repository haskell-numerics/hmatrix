------------------------------------------------------------
{- |
Module      :  Numeric.GSL.Special.Fermi_dirac
Copyright   :  (c) Alberto Ruiz 2006
License     :  GPL-style
Maintainer  :  Alberto Ruiz (aruiz at um dot es)
Stability   :  provisional
Portability :  uses ffi

Wrappers for selected functions described at:

<http://www.google.com/search?q=gsl_sf_fermi_dirac.h&as_sitesearch=www.gnu.org/software/gsl/manual&btnI=Lucky>

-}
------------------------------------------------------------

module Numeric.GSL.Special.Fermi_dirac(
  fermi_dirac_m1_e
, fermi_dirac_m1
, fermi_dirac_0_e
, fermi_dirac_0
, fermi_dirac_1_e
, fermi_dirac_1
, fermi_dirac_2_e
, fermi_dirac_2
, fermi_dirac_int_e
, fermi_dirac_int
, fermi_dirac_mhalf_e
, fermi_dirac_mhalf
, fermi_dirac_half_e
, fermi_dirac_half
, fermi_dirac_3half_e
, fermi_dirac_3half
, fermi_dirac_inc_0_e
, fermi_dirac_inc_0
) where

import Foreign(Ptr)
import Foreign.C.Types(CInt)
import Numeric.GSL.Special.Internal

-- | wrapper for int gsl_sf_fermi_dirac_m1_e(double x,gsl_sf_result* result);
--
--   <http://www.google.com/search?q=gsl_sf_fermi_dirac_m1_e&as_sitesearch=www.gnu.org/software/gsl/manual&btnI=Lucky>
fermi_dirac_m1_e :: Double -> (Double,Double)
fermi_dirac_m1_e x = createSFR "fermi_dirac_m1_e" $ gsl_sf_fermi_dirac_m1_e x
foreign import ccall "gsl_sf_fermi_dirac_m1_e" gsl_sf_fermi_dirac_m1_e :: Double -> Ptr () -> IO CInt

-- | wrapper for double gsl_sf_fermi_dirac_m1(double x);
--
--   <http://www.google.com/search?q=gsl_sf_fermi_dirac_m1&as_sitesearch=www.gnu.org/software/gsl/manual&btnI=Lucky>
fermi_dirac_m1 :: Double -> Double
fermi_dirac_m1 = gsl_sf_fermi_dirac_m1
foreign import ccall "gsl_sf_fermi_dirac_m1" gsl_sf_fermi_dirac_m1 :: Double -> Double

-- | wrapper for int gsl_sf_fermi_dirac_0_e(double x,gsl_sf_result* result);
--
--   <http://www.google.com/search?q=gsl_sf_fermi_dirac_0_e&as_sitesearch=www.gnu.org/software/gsl/manual&btnI=Lucky>
fermi_dirac_0_e :: Double -> (Double,Double)
fermi_dirac_0_e x = createSFR "fermi_dirac_0_e" $ gsl_sf_fermi_dirac_0_e x
foreign import ccall "gsl_sf_fermi_dirac_0_e" gsl_sf_fermi_dirac_0_e :: Double -> Ptr () -> IO CInt

-- | wrapper for double gsl_sf_fermi_dirac_0(double x);
--
--   <http://www.google.com/search?q=gsl_sf_fermi_dirac_0&as_sitesearch=www.gnu.org/software/gsl/manual&btnI=Lucky>
fermi_dirac_0 :: Double -> Double
fermi_dirac_0 = gsl_sf_fermi_dirac_0
foreign import ccall "gsl_sf_fermi_dirac_0" gsl_sf_fermi_dirac_0 :: Double -> Double

-- | wrapper for int gsl_sf_fermi_dirac_1_e(double x,gsl_sf_result* result);
--
--   <http://www.google.com/search?q=gsl_sf_fermi_dirac_1_e&as_sitesearch=www.gnu.org/software/gsl/manual&btnI=Lucky>
fermi_dirac_1_e :: Double -> (Double,Double)
fermi_dirac_1_e x = createSFR "fermi_dirac_1_e" $ gsl_sf_fermi_dirac_1_e x
foreign import ccall "gsl_sf_fermi_dirac_1_e" gsl_sf_fermi_dirac_1_e :: Double -> Ptr () -> IO CInt

-- | wrapper for double gsl_sf_fermi_dirac_1(double x);
--
--   <http://www.google.com/search?q=gsl_sf_fermi_dirac_1&as_sitesearch=www.gnu.org/software/gsl/manual&btnI=Lucky>
fermi_dirac_1 :: Double -> Double
fermi_dirac_1 = gsl_sf_fermi_dirac_1
foreign import ccall "gsl_sf_fermi_dirac_1" gsl_sf_fermi_dirac_1 :: Double -> Double

-- | wrapper for int gsl_sf_fermi_dirac_2_e(double x,gsl_sf_result* result);
--
--   <http://www.google.com/search?q=gsl_sf_fermi_dirac_2_e&as_sitesearch=www.gnu.org/software/gsl/manual&btnI=Lucky>
fermi_dirac_2_e :: Double -> (Double,Double)
fermi_dirac_2_e x = createSFR "fermi_dirac_2_e" $ gsl_sf_fermi_dirac_2_e x
foreign import ccall "gsl_sf_fermi_dirac_2_e" gsl_sf_fermi_dirac_2_e :: Double -> Ptr () -> IO CInt

-- | wrapper for double gsl_sf_fermi_dirac_2(double x);
--
--   <http://www.google.com/search?q=gsl_sf_fermi_dirac_2&as_sitesearch=www.gnu.org/software/gsl/manual&btnI=Lucky>
fermi_dirac_2 :: Double -> Double
fermi_dirac_2 = gsl_sf_fermi_dirac_2
foreign import ccall "gsl_sf_fermi_dirac_2" gsl_sf_fermi_dirac_2 :: Double -> Double

-- | wrapper for int gsl_sf_fermi_dirac_int_e(int j,double x,gsl_sf_result* result);
--
--   <http://www.google.com/search?q=gsl_sf_fermi_dirac_int_e&as_sitesearch=www.gnu.org/software/gsl/manual&btnI=Lucky>
fermi_dirac_int_e :: CInt -> Double -> (Double,Double)
fermi_dirac_int_e j x = createSFR "fermi_dirac_int_e" $ gsl_sf_fermi_dirac_int_e j x
foreign import ccall "gsl_sf_fermi_dirac_int_e" gsl_sf_fermi_dirac_int_e :: CInt -> Double -> Ptr () -> IO CInt

-- | wrapper for double gsl_sf_fermi_dirac_int(int j,double x);
--
--   <http://www.google.com/search?q=gsl_sf_fermi_dirac_int&as_sitesearch=www.gnu.org/software/gsl/manual&btnI=Lucky>
fermi_dirac_int :: CInt -> Double -> Double
fermi_dirac_int = gsl_sf_fermi_dirac_int
foreign import ccall "gsl_sf_fermi_dirac_int" gsl_sf_fermi_dirac_int :: CInt -> Double -> Double

-- | wrapper for int gsl_sf_fermi_dirac_mhalf_e(double x,gsl_sf_result* result);
--
--   <http://www.google.com/search?q=gsl_sf_fermi_dirac_mhalf_e&as_sitesearch=www.gnu.org/software/gsl/manual&btnI=Lucky>
fermi_dirac_mhalf_e :: Double -> (Double,Double)
fermi_dirac_mhalf_e x = createSFR "fermi_dirac_mhalf_e" $ gsl_sf_fermi_dirac_mhalf_e x
foreign import ccall "gsl_sf_fermi_dirac_mhalf_e" gsl_sf_fermi_dirac_mhalf_e :: Double -> Ptr () -> IO CInt

-- | wrapper for double gsl_sf_fermi_dirac_mhalf(double x);
--
--   <http://www.google.com/search?q=gsl_sf_fermi_dirac_mhalf&as_sitesearch=www.gnu.org/software/gsl/manual&btnI=Lucky>
fermi_dirac_mhalf :: Double -> Double
fermi_dirac_mhalf = gsl_sf_fermi_dirac_mhalf
foreign import ccall "gsl_sf_fermi_dirac_mhalf" gsl_sf_fermi_dirac_mhalf :: Double -> Double

-- | wrapper for int gsl_sf_fermi_dirac_half_e(double x,gsl_sf_result* result);
--
--   <http://www.google.com/search?q=gsl_sf_fermi_dirac_half_e&as_sitesearch=www.gnu.org/software/gsl/manual&btnI=Lucky>
fermi_dirac_half_e :: Double -> (Double,Double)
fermi_dirac_half_e x = createSFR "fermi_dirac_half_e" $ gsl_sf_fermi_dirac_half_e x
foreign import ccall "gsl_sf_fermi_dirac_half_e" gsl_sf_fermi_dirac_half_e :: Double -> Ptr () -> IO CInt

-- | wrapper for double gsl_sf_fermi_dirac_half(double x);
--
--   <http://www.google.com/search?q=gsl_sf_fermi_dirac_half&as_sitesearch=www.gnu.org/software/gsl/manual&btnI=Lucky>
fermi_dirac_half :: Double -> Double
fermi_dirac_half = gsl_sf_fermi_dirac_half
foreign import ccall "gsl_sf_fermi_dirac_half" gsl_sf_fermi_dirac_half :: Double -> Double

-- | wrapper for int gsl_sf_fermi_dirac_3half_e(double x,gsl_sf_result* result);
--
--   <http://www.google.com/search?q=gsl_sf_fermi_dirac_3half_e&as_sitesearch=www.gnu.org/software/gsl/manual&btnI=Lucky>
fermi_dirac_3half_e :: Double -> (Double,Double)
fermi_dirac_3half_e x = createSFR "fermi_dirac_3half_e" $ gsl_sf_fermi_dirac_3half_e x
foreign import ccall "gsl_sf_fermi_dirac_3half_e" gsl_sf_fermi_dirac_3half_e :: Double -> Ptr () -> IO CInt

-- | wrapper for double gsl_sf_fermi_dirac_3half(double x);
--
--   <http://www.google.com/search?q=gsl_sf_fermi_dirac_3half&as_sitesearch=www.gnu.org/software/gsl/manual&btnI=Lucky>
fermi_dirac_3half :: Double -> Double
fermi_dirac_3half = gsl_sf_fermi_dirac_3half
foreign import ccall "gsl_sf_fermi_dirac_3half" gsl_sf_fermi_dirac_3half :: Double -> Double

-- | wrapper for int gsl_sf_fermi_dirac_inc_0_e(double x,double b,gsl_sf_result* result);
--
--   <http://www.google.com/search?q=gsl_sf_fermi_dirac_inc_0_e&as_sitesearch=www.gnu.org/software/gsl/manual&btnI=Lucky>
fermi_dirac_inc_0_e :: Double -> Double -> (Double,Double)
fermi_dirac_inc_0_e x b = createSFR "fermi_dirac_inc_0_e" $ gsl_sf_fermi_dirac_inc_0_e x b
foreign import ccall "gsl_sf_fermi_dirac_inc_0_e" gsl_sf_fermi_dirac_inc_0_e :: Double -> Double -> Ptr () -> IO CInt

-- | wrapper for double gsl_sf_fermi_dirac_inc_0(double x,double b);
--
--   <http://www.google.com/search?q=gsl_sf_fermi_dirac_inc_0&as_sitesearch=www.gnu.org/software/gsl/manual&btnI=Lucky>
fermi_dirac_inc_0 :: Double -> Double -> Double
fermi_dirac_inc_0 = gsl_sf_fermi_dirac_inc_0
foreign import ccall "gsl_sf_fermi_dirac_inc_0" gsl_sf_fermi_dirac_inc_0 :: Double -> Double -> Double
