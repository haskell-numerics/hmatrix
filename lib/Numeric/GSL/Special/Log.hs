------------------------------------------------------------
-- |
-- Module      :  Numeric.GSL.Special.Log
-- Copyright   :  (c) Alberto Ruiz 2006
-- License     :  GPL
-- Maintainer  :  Alberto Ruiz (aruiz at um dot es)
-- Stability   :  provisional
-- Portability :  uses ffi
--
-- Wrappers for selected functions described at:
--
-- <http://www.google.com/search?q=gsl_sf_log.h&as_sitesearch=www.gnu.org/software/gsl/manual&btnI=Lucky>
------------------------------------------------------------

module Numeric.GSL.Special.Log(
  log_e
, Numeric.GSL.Special.Log.log
, log_abs_e
, log_abs
, log_1plusx_e
, log_1plusx
, log_1plusx_mx_e
, log_1plusx_mx
) where

import Foreign(Ptr)
import Foreign.C.Types(CInt)
import Numeric.GSL.Special.Internal

log_e :: Double -> (Double,Double)
log_e x = createSFR "log_e" $ gsl_sf_log_e x
foreign import ccall "gsl_sf_log_e" gsl_sf_log_e :: Double -> Ptr () -> IO CInt

log :: Double -> Double
log = gsl_sf_log
foreign import ccall "gsl_sf_log" gsl_sf_log :: Double -> Double

log_abs_e :: Double -> (Double,Double)
log_abs_e x = createSFR "log_abs_e" $ gsl_sf_log_abs_e x
foreign import ccall "gsl_sf_log_abs_e" gsl_sf_log_abs_e :: Double -> Ptr () -> IO CInt

log_abs :: Double -> Double
log_abs = gsl_sf_log_abs
foreign import ccall "gsl_sf_log_abs" gsl_sf_log_abs :: Double -> Double

complex_log_e :: Double -> Double -> Ptr () -> (Double,Double)
complex_log_e zr zi lnr = createSFR "complex_log_e" $ gsl_sf_complex_log_e zr zi lnr
foreign import ccall "gsl_sf_complex_log_e" gsl_sf_complex_log_e :: Double -> Double -> Ptr () -> Ptr () -> IO CInt

log_1plusx_e :: Double -> (Double,Double)
log_1plusx_e x = createSFR "log_1plusx_e" $ gsl_sf_log_1plusx_e x
foreign import ccall "gsl_sf_log_1plusx_e" gsl_sf_log_1plusx_e :: Double -> Ptr () -> IO CInt

log_1plusx :: Double -> Double
log_1plusx = gsl_sf_log_1plusx
foreign import ccall "gsl_sf_log_1plusx" gsl_sf_log_1plusx :: Double -> Double

log_1plusx_mx_e :: Double -> (Double,Double)
log_1plusx_mx_e x = createSFR "log_1plusx_mx_e" $ gsl_sf_log_1plusx_mx_e x
foreign import ccall "gsl_sf_log_1plusx_mx_e" gsl_sf_log_1plusx_mx_e :: Double -> Ptr () -> IO CInt

log_1plusx_mx :: Double -> Double
log_1plusx_mx = gsl_sf_log_1plusx_mx
foreign import ccall "gsl_sf_log_1plusx_mx" gsl_sf_log_1plusx_mx :: Double -> Double
