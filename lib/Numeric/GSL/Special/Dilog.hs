------------------------------------------------------------
-- |
-- Module      :  Numeric.GSL.Special.Dilog
-- Copyright   :  (c) Alberto Ruiz 2006
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
) where

import Foreign(Ptr)
import Foreign.C.Types(CInt)
import Numeric.GSL.Special.Internal

dilog_e :: Double -> (Double,Double)
dilog_e x = createSFR "dilog_e" $ gsl_sf_dilog_e x
foreign import ccall "gsl_sf_dilog_e" gsl_sf_dilog_e :: Double -> Ptr () -> IO CInt

dilog :: Double -> Double
dilog = gsl_sf_dilog
foreign import ccall "gsl_sf_dilog" gsl_sf_dilog :: Double -> Double

complex_dilog_xy_e :: Double -> Double -> Ptr () -> (Double,Double)
complex_dilog_xy_e x y result_re = createSFR "complex_dilog_xy_e" $ gsl_sf_complex_dilog_xy_e x y result_re
foreign import ccall "gsl_sf_complex_dilog_xy_e" gsl_sf_complex_dilog_xy_e :: Double -> Double -> Ptr () -> Ptr () -> IO CInt

complex_dilog_e :: Double -> Double -> Ptr () -> (Double,Double)
complex_dilog_e r theta result_re = createSFR "complex_dilog_e" $ gsl_sf_complex_dilog_e r theta result_re
foreign import ccall "gsl_sf_complex_dilog_e" gsl_sf_complex_dilog_e :: Double -> Double -> Ptr () -> Ptr () -> IO CInt

complex_spence_xy_e :: Double -> Double -> Ptr () -> (Double,Double)
complex_spence_xy_e x y real_sp = createSFR "complex_spence_xy_e" $ gsl_sf_complex_spence_xy_e x y real_sp
foreign import ccall "gsl_sf_complex_spence_xy_e" gsl_sf_complex_spence_xy_e :: Double -> Double -> Ptr () -> Ptr () -> IO CInt
