------------------------------------------------------------
-- |
-- Module      :  Numeric.GSL.Special.Debye
-- Copyright   :  (c) Alberto Ruiz 2006
-- License     :  GPL
-- Maintainer  :  Alberto Ruiz (aruiz at um dot es)
-- Stability   :  provisional
-- Portability :  uses ffi
--
-- Wrappers for selected functions described at:
--
-- <http://www.google.com/search?q=gsl_sf_debye.h&as_sitesearch=www.gnu.org/software/gsl/manual&btnI=Lucky>
------------------------------------------------------------

module Numeric.GSL.Special.Debye(
  debye_1_e
, debye_1
, debye_2_e
, debye_2
, debye_3_e
, debye_3
, debye_4_e
, debye_4
, debye_5_e
, debye_5
, debye_6_e
, debye_6
) where

import Foreign(Ptr)
import Foreign.C.Types(CInt)
import Numeric.GSL.Special.Internal

debye_1_e :: Double -> (Double,Double)
debye_1_e x = createSFR "debye_1_e" $ gsl_sf_debye_1_e x
foreign import ccall "gsl_sf_debye_1_e" gsl_sf_debye_1_e :: Double -> Ptr () -> IO CInt

debye_1 :: Double -> Double
debye_1 = gsl_sf_debye_1
foreign import ccall "gsl_sf_debye_1" gsl_sf_debye_1 :: Double -> Double

debye_2_e :: Double -> (Double,Double)
debye_2_e x = createSFR "debye_2_e" $ gsl_sf_debye_2_e x
foreign import ccall "gsl_sf_debye_2_e" gsl_sf_debye_2_e :: Double -> Ptr () -> IO CInt

debye_2 :: Double -> Double
debye_2 = gsl_sf_debye_2
foreign import ccall "gsl_sf_debye_2" gsl_sf_debye_2 :: Double -> Double

debye_3_e :: Double -> (Double,Double)
debye_3_e x = createSFR "debye_3_e" $ gsl_sf_debye_3_e x
foreign import ccall "gsl_sf_debye_3_e" gsl_sf_debye_3_e :: Double -> Ptr () -> IO CInt

debye_3 :: Double -> Double
debye_3 = gsl_sf_debye_3
foreign import ccall "gsl_sf_debye_3" gsl_sf_debye_3 :: Double -> Double

debye_4_e :: Double -> (Double,Double)
debye_4_e x = createSFR "debye_4_e" $ gsl_sf_debye_4_e x
foreign import ccall "gsl_sf_debye_4_e" gsl_sf_debye_4_e :: Double -> Ptr () -> IO CInt

debye_4 :: Double -> Double
debye_4 = gsl_sf_debye_4
foreign import ccall "gsl_sf_debye_4" gsl_sf_debye_4 :: Double -> Double

debye_5_e :: Double -> (Double,Double)
debye_5_e x = createSFR "debye_5_e" $ gsl_sf_debye_5_e x
foreign import ccall "gsl_sf_debye_5_e" gsl_sf_debye_5_e :: Double -> Ptr () -> IO CInt

debye_5 :: Double -> Double
debye_5 = gsl_sf_debye_5
foreign import ccall "gsl_sf_debye_5" gsl_sf_debye_5 :: Double -> Double

debye_6_e :: Double -> (Double,Double)
debye_6_e x = createSFR "debye_6_e" $ gsl_sf_debye_6_e x
foreign import ccall "gsl_sf_debye_6_e" gsl_sf_debye_6_e :: Double -> Ptr () -> IO CInt

debye_6 :: Double -> Double
debye_6 = gsl_sf_debye_6
foreign import ccall "gsl_sf_debye_6" gsl_sf_debye_6 :: Double -> Double
