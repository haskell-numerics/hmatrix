------------------------------------------------------------
{- |
Module      :  GSL.Special.Dilog
Copyright   :  (c) Alberto Ruiz 2006
License     :  GPL-style
Maintainer  :  Alberto Ruiz (aruiz at um dot es)
Stability   :  provisional
Portability :  uses ffi

Wrappers for selected functions described at:

<http://www.google.com/search?q=gsl_sf_dilog.h&as_sitesearch=www.gnu.org/software/gsl/manual&btnI=Lucky>

-}
------------------------------------------------------------

module GSL.Special.Dilog(
  dilog_e
, dilog
) where

import Foreign(Ptr)
import GSL.Special.Internal

-- | wrapper for int gsl_sf_dilog_e(double x,gsl_sf_result* result);
--
--   <http://www.google.com/search?q=gsl_sf_dilog_e&as_sitesearch=www.gnu.org/software/gsl/manual&btnI=Lucky>
dilog_e :: Double -> (Double,Double)
dilog_e x = createSFR "dilog_e" $ gsl_sf_dilog_e x
foreign import ccall "dilog.h gsl_sf_dilog_e" gsl_sf_dilog_e :: Double -> Ptr Double -> IO(Int)

-- | wrapper for double gsl_sf_dilog(double x);
--
--   <http://www.google.com/search?q=gsl_sf_dilog&as_sitesearch=www.gnu.org/software/gsl/manual&btnI=Lucky>
dilog :: Double -> Double
dilog = gsl_sf_dilog
foreign import ccall "dilog.h gsl_sf_dilog" gsl_sf_dilog :: Double -> Double

-- | wrapper for int gsl_sf_complex_dilog_xy_e(double x,double y,gsl_sf_result* result_re,gsl_sf_result* result_im);
--
--   <http://www.google.com/search?q=gsl_sf_complex_dilog_xy_e&as_sitesearch=www.gnu.org/software/gsl/manual&btnI=Lucky>
complex_dilog_xy_e :: Double -> Double -> Ptr Double -> (Double,Double)
complex_dilog_xy_e x y result_re = createSFR "complex_dilog_xy_e" $ gsl_sf_complex_dilog_xy_e x y result_re
foreign import ccall "dilog.h gsl_sf_complex_dilog_xy_e" gsl_sf_complex_dilog_xy_e :: Double -> Double -> Ptr Double -> Ptr Double -> IO(Int)

-- | wrapper for int gsl_sf_complex_dilog_e(double r,double theta,gsl_sf_result* result_re,gsl_sf_result* result_im);
--
--   <http://www.google.com/search?q=gsl_sf_complex_dilog_e&as_sitesearch=www.gnu.org/software/gsl/manual&btnI=Lucky>
complex_dilog_e :: Double -> Double -> Ptr Double -> (Double,Double)
complex_dilog_e r theta result_re = createSFR "complex_dilog_e" $ gsl_sf_complex_dilog_e r theta result_re
foreign import ccall "dilog.h gsl_sf_complex_dilog_e" gsl_sf_complex_dilog_e :: Double -> Double -> Ptr Double -> Ptr Double -> IO(Int)

-- | wrapper for int gsl_sf_complex_spence_xy_e(double x,double y,gsl_sf_result* real_sp,gsl_sf_result* imag_sp);
--
--   <http://www.google.com/search?q=gsl_sf_complex_spence_xy_e&as_sitesearch=www.gnu.org/software/gsl/manual&btnI=Lucky>
complex_spence_xy_e :: Double -> Double -> Ptr Double -> (Double,Double)
complex_spence_xy_e x y real_sp = createSFR "complex_spence_xy_e" $ gsl_sf_complex_spence_xy_e x y real_sp
foreign import ccall "dilog.h gsl_sf_complex_spence_xy_e" gsl_sf_complex_spence_xy_e :: Double -> Double -> Ptr Double -> Ptr Double -> IO(Int)
