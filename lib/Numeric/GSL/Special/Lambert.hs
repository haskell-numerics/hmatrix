------------------------------------------------------------
-- |
-- Module      :  Numeric.GSL.Special.Lambert
-- Copyright   :  (c) Alberto Ruiz 2006
-- License     :  GPL
-- Maintainer  :  Alberto Ruiz (aruiz at um dot es)
-- Stability   :  provisional
-- Portability :  uses ffi
--
-- Wrappers for selected functions described at:
--
-- <http://www.google.com/search?q=gsl_sf_lambert.h&as_sitesearch=www.gnu.org/software/gsl/manual&btnI=Lucky>
------------------------------------------------------------

module Numeric.GSL.Special.Lambert(
  lambert_W0_e
, lambert_W0
, lambert_Wm1_e
, lambert_Wm1
) where

import Foreign(Ptr)
import Foreign.C.Types(CInt)
import Numeric.GSL.Special.Internal

lambert_W0_e :: Double -> (Double,Double)
lambert_W0_e x = createSFR "lambert_W0_e" $ gsl_sf_lambert_W0_e x
foreign import ccall "gsl_sf_lambert_W0_e" gsl_sf_lambert_W0_e :: Double -> Ptr () -> IO CInt

lambert_W0 :: Double -> Double
lambert_W0 = gsl_sf_lambert_W0
foreign import ccall "gsl_sf_lambert_W0" gsl_sf_lambert_W0 :: Double -> Double

lambert_Wm1_e :: Double -> (Double,Double)
lambert_Wm1_e x = createSFR "lambert_Wm1_e" $ gsl_sf_lambert_Wm1_e x
foreign import ccall "gsl_sf_lambert_Wm1_e" gsl_sf_lambert_Wm1_e :: Double -> Ptr () -> IO CInt

lambert_Wm1 :: Double -> Double
lambert_Wm1 = gsl_sf_lambert_Wm1
foreign import ccall "gsl_sf_lambert_Wm1" gsl_sf_lambert_Wm1 :: Double -> Double
