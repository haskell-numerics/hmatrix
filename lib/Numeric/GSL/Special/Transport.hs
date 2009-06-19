------------------------------------------------------------
-- |
-- Module      :  Numeric.GSL.Special.Transport
-- Copyright   :  (c) Alberto Ruiz 2006
-- License     :  GPL
-- Maintainer  :  Alberto Ruiz (aruiz at um dot es)
-- Stability   :  provisional
-- Portability :  uses ffi
--
-- Wrappers for selected functions described at:
--
-- <http://www.google.com/search?q=gsl_sf_transport.h&as_sitesearch=www.gnu.org/software/gsl/manual&btnI=Lucky>
------------------------------------------------------------

module Numeric.GSL.Special.Transport(
  transport_2_e
, transport_2
, transport_3_e
, transport_3
, transport_4_e
, transport_4
, transport_5_e
, transport_5
) where

import Foreign(Ptr)
import Foreign.C.Types(CInt)
import Numeric.GSL.Special.Internal

transport_2_e :: Double -> (Double,Double)
transport_2_e x = createSFR "transport_2_e" $ gsl_sf_transport_2_e x
foreign import ccall "gsl_sf_transport_2_e" gsl_sf_transport_2_e :: Double -> Ptr () -> IO CInt

transport_2 :: Double -> Double
transport_2 = gsl_sf_transport_2
foreign import ccall "gsl_sf_transport_2" gsl_sf_transport_2 :: Double -> Double

transport_3_e :: Double -> (Double,Double)
transport_3_e x = createSFR "transport_3_e" $ gsl_sf_transport_3_e x
foreign import ccall "gsl_sf_transport_3_e" gsl_sf_transport_3_e :: Double -> Ptr () -> IO CInt

transport_3 :: Double -> Double
transport_3 = gsl_sf_transport_3
foreign import ccall "gsl_sf_transport_3" gsl_sf_transport_3 :: Double -> Double

transport_4_e :: Double -> (Double,Double)
transport_4_e x = createSFR "transport_4_e" $ gsl_sf_transport_4_e x
foreign import ccall "gsl_sf_transport_4_e" gsl_sf_transport_4_e :: Double -> Ptr () -> IO CInt

transport_4 :: Double -> Double
transport_4 = gsl_sf_transport_4
foreign import ccall "gsl_sf_transport_4" gsl_sf_transport_4 :: Double -> Double

transport_5_e :: Double -> (Double,Double)
transport_5_e x = createSFR "transport_5_e" $ gsl_sf_transport_5_e x
foreign import ccall "gsl_sf_transport_5_e" gsl_sf_transport_5_e :: Double -> Ptr () -> IO CInt

transport_5 :: Double -> Double
transport_5 = gsl_sf_transport_5
foreign import ccall "gsl_sf_transport_5" gsl_sf_transport_5 :: Double -> Double
