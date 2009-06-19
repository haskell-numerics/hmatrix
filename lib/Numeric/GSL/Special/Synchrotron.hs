------------------------------------------------------------
-- |
-- Module      :  Numeric.GSL.Special.Synchrotron
-- Copyright   :  (c) Alberto Ruiz 2006
-- License     :  GPL
-- Maintainer  :  Alberto Ruiz (aruiz at um dot es)
-- Stability   :  provisional
-- Portability :  uses ffi
--
-- Wrappers for selected functions described at:
--
-- <http://www.google.com/search?q=gsl_sf_synchrotron.h&as_sitesearch=www.gnu.org/software/gsl/manual&btnI=Lucky>
------------------------------------------------------------

module Numeric.GSL.Special.Synchrotron(
  synchrotron_1_e
, synchrotron_1
, synchrotron_2_e
, synchrotron_2
) where

import Foreign(Ptr)
import Foreign.C.Types(CInt)
import Numeric.GSL.Special.Internal

synchrotron_1_e :: Double -> (Double,Double)
synchrotron_1_e x = createSFR "synchrotron_1_e" $ gsl_sf_synchrotron_1_e x
foreign import ccall "gsl_sf_synchrotron_1_e" gsl_sf_synchrotron_1_e :: Double -> Ptr () -> IO CInt

synchrotron_1 :: Double -> Double
synchrotron_1 = gsl_sf_synchrotron_1
foreign import ccall "gsl_sf_synchrotron_1" gsl_sf_synchrotron_1 :: Double -> Double

synchrotron_2_e :: Double -> (Double,Double)
synchrotron_2_e x = createSFR "synchrotron_2_e" $ gsl_sf_synchrotron_2_e x
foreign import ccall "gsl_sf_synchrotron_2_e" gsl_sf_synchrotron_2_e :: Double -> Ptr () -> IO CInt

synchrotron_2 :: Double -> Double
synchrotron_2 = gsl_sf_synchrotron_2
foreign import ccall "gsl_sf_synchrotron_2" gsl_sf_synchrotron_2 :: Double -> Double
