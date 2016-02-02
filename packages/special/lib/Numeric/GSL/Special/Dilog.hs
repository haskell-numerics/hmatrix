{-# LANGUAGE CPP #-}
------------------------------------------------------------
-- |
-- Module      :  Numeric.GSL.Special.Dilog
-- Copyright   :  (c) Alberto Ruiz 2006-11
-- License     :  GPL
-- Maintainer  :  Alberto Ruiz (aruiz at um dot es)
-- Stability   :  provisional
-- Portability :  uses ffi
--
-- Wrappers for selected functions described at:
--
-- <http://www.google.com/search?q=gsl_sf_dilog.h&as_sitesearch=www.gnu.org/software/gsl/manual&btnI=Lucky>
------------------------------------------------------------

module Numeric.GSL.Special.Dilog(
  dilog_e
, dilog
, complex_dilog_xy_e
, complex_dilog_e
, complex_spence_xy_e
) where

import Foreign(Ptr)
import Foreign.C.Types
import Numeric.GSL.Special.Internal

dilog_e :: Double -> (Double,Double)
dilog_e x = createSFR "dilog_e" $ gsl_sf_dilog_e x
foreign import ccall SAFE_CHEAP "gsl_sf_dilog_e" gsl_sf_dilog_e :: Double -> Ptr () -> IO CInt

dilog :: Double -> Double
dilog = gsl_sf_dilog
foreign import ccall SAFE_CHEAP "gsl_sf_dilog" gsl_sf_dilog :: Double -> Double

complex_dilog_xy_e :: Double -> Double -> ((Double,Double),(Double,Double))
complex_dilog_xy_e x y = create2SFR "complex_dilog_xy_e" $ gsl_sf_complex_dilog_xy_e x y
foreign import ccall SAFE_CHEAP "gsl_sf_complex_dilog_xy_e" gsl_sf_complex_dilog_xy_e :: Double -> Double -> Ptr () -> Ptr () -> IO CInt

complex_dilog_e :: Double -> Double -> ((Double,Double),(Double,Double))
complex_dilog_e r theta = create2SFR "complex_dilog_e" $ gsl_sf_complex_dilog_e r theta
foreign import ccall SAFE_CHEAP "gsl_sf_complex_dilog_e" gsl_sf_complex_dilog_e :: Double -> Double -> Ptr () -> Ptr () -> IO CInt

complex_spence_xy_e :: Double -> Double -> ((Double,Double),(Double,Double))
complex_spence_xy_e x y = create2SFR "complex_spence_xy_e" $ gsl_sf_complex_spence_xy_e x y
foreign import ccall SAFE_CHEAP "gsl_sf_complex_spence_xy_e" gsl_sf_complex_spence_xy_e :: Double -> Double -> Ptr () -> Ptr () -> IO CInt
