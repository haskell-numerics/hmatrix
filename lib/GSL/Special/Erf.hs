------------------------------------------------------------
{- |
Module      :  GSL.Special.Erf
Copyright   :  (c) Alberto Ruiz 2006
License     :  GPL-style
Maintainer  :  Alberto Ruiz (aruiz at um dot es)
Stability   :  provisional
Portability :  uses ffi

Wrappers for selected functions described at:

<http://www.google.com/search?q=gsl_sf_erf.h&as_sitesearch=www.gnu.org/software/gsl/manual&btnI=Lucky>

-}
------------------------------------------------------------

module GSL.Special.Erf(
  erfc_e
, erfc
, log_erfc_e
, log_erfc
, erf_e
, erf
, erf_Z_e
, erf_Q_e
, erf_Z
, erf_Q
, hazard_e
, hazard
) where

import Foreign(Ptr)
import GSL.Special.Internal

-- | wrapper for int gsl_sf_erfc_e(double x,gsl_sf_result* result);
--
--   <http://www.google.com/search?q=gsl_sf_erfc_e&as_sitesearch=www.gnu.org/software/gsl/manual&btnI=Lucky>
erfc_e :: Double -> (Double,Double)
erfc_e x = createSFR "erfc_e" $ gsl_sf_erfc_e x
foreign import ccall "erf.h gsl_sf_erfc_e" gsl_sf_erfc_e :: Double -> Ptr Double -> IO(Int)

-- | wrapper for double gsl_sf_erfc(double x);
--
--   <http://www.google.com/search?q=gsl_sf_erfc&as_sitesearch=www.gnu.org/software/gsl/manual&btnI=Lucky>
erfc :: Double -> Double
erfc = gsl_sf_erfc
foreign import ccall "erf.h gsl_sf_erfc" gsl_sf_erfc :: Double -> Double

-- | wrapper for int gsl_sf_log_erfc_e(double x,gsl_sf_result* result);
--
--   <http://www.google.com/search?q=gsl_sf_log_erfc_e&as_sitesearch=www.gnu.org/software/gsl/manual&btnI=Lucky>
log_erfc_e :: Double -> (Double,Double)
log_erfc_e x = createSFR "log_erfc_e" $ gsl_sf_log_erfc_e x
foreign import ccall "erf.h gsl_sf_log_erfc_e" gsl_sf_log_erfc_e :: Double -> Ptr Double -> IO(Int)

-- | wrapper for double gsl_sf_log_erfc(double x);
--
--   <http://www.google.com/search?q=gsl_sf_log_erfc&as_sitesearch=www.gnu.org/software/gsl/manual&btnI=Lucky>
log_erfc :: Double -> Double
log_erfc = gsl_sf_log_erfc
foreign import ccall "erf.h gsl_sf_log_erfc" gsl_sf_log_erfc :: Double -> Double

-- | wrapper for int gsl_sf_erf_e(double x,gsl_sf_result* result);
--
--   <http://www.google.com/search?q=gsl_sf_erf_e&as_sitesearch=www.gnu.org/software/gsl/manual&btnI=Lucky>
erf_e :: Double -> (Double,Double)
erf_e x = createSFR "erf_e" $ gsl_sf_erf_e x
foreign import ccall "erf.h gsl_sf_erf_e" gsl_sf_erf_e :: Double -> Ptr Double -> IO(Int)

-- | wrapper for double gsl_sf_erf(double x);
--
--   <http://www.google.com/search?q=gsl_sf_erf&as_sitesearch=www.gnu.org/software/gsl/manual&btnI=Lucky>
erf :: Double -> Double
erf = gsl_sf_erf
foreign import ccall "erf.h gsl_sf_erf" gsl_sf_erf :: Double -> Double

-- | wrapper for int gsl_sf_erf_Z_e(double x,gsl_sf_result* result);
--
--   <http://www.google.com/search?q=gsl_sf_erf_Z_e&as_sitesearch=www.gnu.org/software/gsl/manual&btnI=Lucky>
erf_Z_e :: Double -> (Double,Double)
erf_Z_e x = createSFR "erf_Z_e" $ gsl_sf_erf_Z_e x
foreign import ccall "erf.h gsl_sf_erf_Z_e" gsl_sf_erf_Z_e :: Double -> Ptr Double -> IO(Int)

-- | wrapper for int gsl_sf_erf_Q_e(double x,gsl_sf_result* result);
--
--   <http://www.google.com/search?q=gsl_sf_erf_Q_e&as_sitesearch=www.gnu.org/software/gsl/manual&btnI=Lucky>
erf_Q_e :: Double -> (Double,Double)
erf_Q_e x = createSFR "erf_Q_e" $ gsl_sf_erf_Q_e x
foreign import ccall "erf.h gsl_sf_erf_Q_e" gsl_sf_erf_Q_e :: Double -> Ptr Double -> IO(Int)

-- | wrapper for double gsl_sf_erf_Z(double x);
--
--   <http://www.google.com/search?q=gsl_sf_erf_Z&as_sitesearch=www.gnu.org/software/gsl/manual&btnI=Lucky>
erf_Z :: Double -> Double
erf_Z = gsl_sf_erf_Z
foreign import ccall "erf.h gsl_sf_erf_Z" gsl_sf_erf_Z :: Double -> Double

-- | wrapper for double gsl_sf_erf_Q(double x);
--
--   <http://www.google.com/search?q=gsl_sf_erf_Q&as_sitesearch=www.gnu.org/software/gsl/manual&btnI=Lucky>
erf_Q :: Double -> Double
erf_Q = gsl_sf_erf_Q
foreign import ccall "erf.h gsl_sf_erf_Q" gsl_sf_erf_Q :: Double -> Double

-- | wrapper for int gsl_sf_hazard_e(double x,gsl_sf_result* result);
--
--   <http://www.google.com/search?q=gsl_sf_hazard_e&as_sitesearch=www.gnu.org/software/gsl/manual&btnI=Lucky>
hazard_e :: Double -> (Double,Double)
hazard_e x = createSFR "hazard_e" $ gsl_sf_hazard_e x
foreign import ccall "erf.h gsl_sf_hazard_e" gsl_sf_hazard_e :: Double -> Ptr Double -> IO(Int)

-- | wrapper for double gsl_sf_hazard(double x);
--
--   <http://www.google.com/search?q=gsl_sf_hazard&as_sitesearch=www.gnu.org/software/gsl/manual&btnI=Lucky>
hazard :: Double -> Double
hazard = gsl_sf_hazard
foreign import ccall "erf.h gsl_sf_hazard" gsl_sf_hazard :: Double -> Double
