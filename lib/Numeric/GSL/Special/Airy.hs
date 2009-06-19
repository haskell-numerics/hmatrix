------------------------------------------------------------
-- |
-- Module      :  Numeric.GSL.Special.Airy
-- Copyright   :  (c) Alberto Ruiz 2006
-- License     :  GPL
-- Maintainer  :  Alberto Ruiz (aruiz at um dot es)
-- Stability   :  provisional
-- Portability :  uses ffi
--
-- Wrappers for selected functions described at:
--
-- <http://www.google.com/search?q=gsl_sf_airy.h&as_sitesearch=www.gnu.org/software/gsl/manual&btnI=Lucky>
------------------------------------------------------------

module Numeric.GSL.Special.Airy(
  airy_Ai_e
, airy_Ai
, airy_Bi_e
, airy_Bi
, airy_Ai_scaled_e
, airy_Ai_scaled
, airy_Bi_scaled_e
, airy_Bi_scaled
, airy_Ai_deriv_e
, airy_Ai_deriv
, airy_Bi_deriv_e
, airy_Bi_deriv
, airy_Ai_deriv_scaled_e
, airy_Ai_deriv_scaled
, airy_Bi_deriv_scaled_e
, airy_Bi_deriv_scaled
, airy_zero_Ai_e
, airy_zero_Ai
, airy_zero_Bi_e
, airy_zero_Bi
, airy_zero_Ai_deriv_e
, airy_zero_Ai_deriv
, airy_zero_Bi_deriv_e
, airy_zero_Bi_deriv
, Precision(..)
) where

import Foreign(Ptr)
import Foreign.C.Types(CInt)
import Numeric.GSL.Special.Internal

airy_Ai_e :: Double -> Precision -> (Double,Double)
airy_Ai_e x mode = createSFR "airy_Ai_e" $ gsl_sf_airy_Ai_e x  (precCode mode)
foreign import ccall "gsl_sf_airy_Ai_e" gsl_sf_airy_Ai_e :: Double -> Gsl_mode_t -> Ptr () -> IO CInt

airy_Ai :: Double -> Precision -> Double
airy_Ai x mode = gsl_sf_airy_Ai x  (precCode mode)
foreign import ccall "gsl_sf_airy_Ai" gsl_sf_airy_Ai :: Double -> Gsl_mode_t -> Double

airy_Bi_e :: Double -> Precision -> (Double,Double)
airy_Bi_e x mode = createSFR "airy_Bi_e" $ gsl_sf_airy_Bi_e x  (precCode mode)
foreign import ccall "gsl_sf_airy_Bi_e" gsl_sf_airy_Bi_e :: Double -> Gsl_mode_t -> Ptr () -> IO CInt

airy_Bi :: Double -> Precision -> Double
airy_Bi x mode = gsl_sf_airy_Bi x  (precCode mode)
foreign import ccall "gsl_sf_airy_Bi" gsl_sf_airy_Bi :: Double -> Gsl_mode_t -> Double

airy_Ai_scaled_e :: Double -> Precision -> (Double,Double)
airy_Ai_scaled_e x mode = createSFR "airy_Ai_scaled_e" $ gsl_sf_airy_Ai_scaled_e x  (precCode mode)
foreign import ccall "gsl_sf_airy_Ai_scaled_e" gsl_sf_airy_Ai_scaled_e :: Double -> Gsl_mode_t -> Ptr () -> IO CInt

airy_Ai_scaled :: Double -> Precision -> Double
airy_Ai_scaled x mode = gsl_sf_airy_Ai_scaled x  (precCode mode)
foreign import ccall "gsl_sf_airy_Ai_scaled" gsl_sf_airy_Ai_scaled :: Double -> Gsl_mode_t -> Double

airy_Bi_scaled_e :: Double -> Precision -> (Double,Double)
airy_Bi_scaled_e x mode = createSFR "airy_Bi_scaled_e" $ gsl_sf_airy_Bi_scaled_e x  (precCode mode)
foreign import ccall "gsl_sf_airy_Bi_scaled_e" gsl_sf_airy_Bi_scaled_e :: Double -> Gsl_mode_t -> Ptr () -> IO CInt

airy_Bi_scaled :: Double -> Precision -> Double
airy_Bi_scaled x mode = gsl_sf_airy_Bi_scaled x  (precCode mode)
foreign import ccall "gsl_sf_airy_Bi_scaled" gsl_sf_airy_Bi_scaled :: Double -> Gsl_mode_t -> Double

airy_Ai_deriv_e :: Double -> Precision -> (Double,Double)
airy_Ai_deriv_e x mode = createSFR "airy_Ai_deriv_e" $ gsl_sf_airy_Ai_deriv_e x  (precCode mode)
foreign import ccall "gsl_sf_airy_Ai_deriv_e" gsl_sf_airy_Ai_deriv_e :: Double -> Gsl_mode_t -> Ptr () -> IO CInt

airy_Ai_deriv :: Double -> Precision -> Double
airy_Ai_deriv x mode = gsl_sf_airy_Ai_deriv x  (precCode mode)
foreign import ccall "gsl_sf_airy_Ai_deriv" gsl_sf_airy_Ai_deriv :: Double -> Gsl_mode_t -> Double

airy_Bi_deriv_e :: Double -> Precision -> (Double,Double)
airy_Bi_deriv_e x mode = createSFR "airy_Bi_deriv_e" $ gsl_sf_airy_Bi_deriv_e x  (precCode mode)
foreign import ccall "gsl_sf_airy_Bi_deriv_e" gsl_sf_airy_Bi_deriv_e :: Double -> Gsl_mode_t -> Ptr () -> IO CInt

airy_Bi_deriv :: Double -> Precision -> Double
airy_Bi_deriv x mode = gsl_sf_airy_Bi_deriv x  (precCode mode)
foreign import ccall "gsl_sf_airy_Bi_deriv" gsl_sf_airy_Bi_deriv :: Double -> Gsl_mode_t -> Double

airy_Ai_deriv_scaled_e :: Double -> Precision -> (Double,Double)
airy_Ai_deriv_scaled_e x mode = createSFR "airy_Ai_deriv_scaled_e" $ gsl_sf_airy_Ai_deriv_scaled_e x  (precCode mode)
foreign import ccall "gsl_sf_airy_Ai_deriv_scaled_e" gsl_sf_airy_Ai_deriv_scaled_e :: Double -> Gsl_mode_t -> Ptr () -> IO CInt

airy_Ai_deriv_scaled :: Double -> Precision -> Double
airy_Ai_deriv_scaled x mode = gsl_sf_airy_Ai_deriv_scaled x  (precCode mode)
foreign import ccall "gsl_sf_airy_Ai_deriv_scaled" gsl_sf_airy_Ai_deriv_scaled :: Double -> Gsl_mode_t -> Double

airy_Bi_deriv_scaled_e :: Double -> Precision -> (Double,Double)
airy_Bi_deriv_scaled_e x mode = createSFR "airy_Bi_deriv_scaled_e" $ gsl_sf_airy_Bi_deriv_scaled_e x  (precCode mode)
foreign import ccall "gsl_sf_airy_Bi_deriv_scaled_e" gsl_sf_airy_Bi_deriv_scaled_e :: Double -> Gsl_mode_t -> Ptr () -> IO CInt

airy_Bi_deriv_scaled :: Double -> Precision -> Double
airy_Bi_deriv_scaled x mode = gsl_sf_airy_Bi_deriv_scaled x  (precCode mode)
foreign import ccall "gsl_sf_airy_Bi_deriv_scaled" gsl_sf_airy_Bi_deriv_scaled :: Double -> Gsl_mode_t -> Double

airy_zero_Ai_e :: CInt -> (Double,Double)
airy_zero_Ai_e s = createSFR "airy_zero_Ai_e" $ gsl_sf_airy_zero_Ai_e s
foreign import ccall "gsl_sf_airy_zero_Ai_e" gsl_sf_airy_zero_Ai_e :: CInt -> Ptr () -> IO CInt

airy_zero_Ai :: CInt -> Double
airy_zero_Ai = gsl_sf_airy_zero_Ai
foreign import ccall "gsl_sf_airy_zero_Ai" gsl_sf_airy_zero_Ai :: CInt -> Double

airy_zero_Bi_e :: CInt -> (Double,Double)
airy_zero_Bi_e s = createSFR "airy_zero_Bi_e" $ gsl_sf_airy_zero_Bi_e s
foreign import ccall "gsl_sf_airy_zero_Bi_e" gsl_sf_airy_zero_Bi_e :: CInt -> Ptr () -> IO CInt

airy_zero_Bi :: CInt -> Double
airy_zero_Bi = gsl_sf_airy_zero_Bi
foreign import ccall "gsl_sf_airy_zero_Bi" gsl_sf_airy_zero_Bi :: CInt -> Double

airy_zero_Ai_deriv_e :: CInt -> (Double,Double)
airy_zero_Ai_deriv_e s = createSFR "airy_zero_Ai_deriv_e" $ gsl_sf_airy_zero_Ai_deriv_e s
foreign import ccall "gsl_sf_airy_zero_Ai_deriv_e" gsl_sf_airy_zero_Ai_deriv_e :: CInt -> Ptr () -> IO CInt

airy_zero_Ai_deriv :: CInt -> Double
airy_zero_Ai_deriv = gsl_sf_airy_zero_Ai_deriv
foreign import ccall "gsl_sf_airy_zero_Ai_deriv" gsl_sf_airy_zero_Ai_deriv :: CInt -> Double

airy_zero_Bi_deriv_e :: CInt -> (Double,Double)
airy_zero_Bi_deriv_e s = createSFR "airy_zero_Bi_deriv_e" $ gsl_sf_airy_zero_Bi_deriv_e s
foreign import ccall "gsl_sf_airy_zero_Bi_deriv_e" gsl_sf_airy_zero_Bi_deriv_e :: CInt -> Ptr () -> IO CInt

airy_zero_Bi_deriv :: CInt -> Double
airy_zero_Bi_deriv = gsl_sf_airy_zero_Bi_deriv
foreign import ccall "gsl_sf_airy_zero_Bi_deriv" gsl_sf_airy_zero_Bi_deriv :: CInt -> Double
