------------------------------------------------------------
{- |
Module      :  GSL.Special.Airy
Copyright   :  (c) Alberto Ruiz 2006
License     :  GPL-style
Maintainer  :  Alberto Ruiz (aruiz at um dot es)
Stability   :  provisional
Portability :  uses ffi

Wrappers for selected functions described at:

<http://www.gnu.org/software/gsl/manual/html_node/Airy-Functions-and-Derivatives.html>

-}
------------------------------------------------------------

module GSL.Special.Airy(
  Precision (..)
, airy_Ai_e
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
) where

import Foreign(Ptr)
import GSL.Special.Internal

-- | wrapper for int gsl_sf_airy_Ai_e(double x,gsl_mode_t mode,gsl_sf_result* result);
airy_Ai_e :: Double -> Precision -> (Double,Double)
airy_Ai_e x mode = createSFR "airy_Ai_e" $ gsl_sf_airy_Ai_e x  (precCode mode)
foreign import ccall "airy.h gsl_sf_airy_Ai_e" gsl_sf_airy_Ai_e :: Double -> Gsl_mode_t -> Ptr Double -> IO(Int)

-- | wrapper for double gsl_sf_airy_Ai(double x,gsl_mode_t mode);
airy_Ai :: Double -> Precision -> Double
airy_Ai x mode = gsl_sf_airy_Ai x  (precCode mode)
foreign import ccall "airy.h gsl_sf_airy_Ai" gsl_sf_airy_Ai :: Double -> Gsl_mode_t -> Double

-- | wrapper for int gsl_sf_airy_Bi_e(double x,gsl_mode_t mode,gsl_sf_result* result);
airy_Bi_e :: Double -> Precision -> (Double,Double)
airy_Bi_e x mode = createSFR "airy_Bi_e" $ gsl_sf_airy_Bi_e x  (precCode mode)
foreign import ccall "airy.h gsl_sf_airy_Bi_e" gsl_sf_airy_Bi_e :: Double -> Gsl_mode_t -> Ptr Double -> IO(Int)

-- | wrapper for double gsl_sf_airy_Bi(double x,gsl_mode_t mode);
airy_Bi :: Double -> Precision -> Double
airy_Bi x mode = gsl_sf_airy_Bi x  (precCode mode)
foreign import ccall "airy.h gsl_sf_airy_Bi" gsl_sf_airy_Bi :: Double -> Gsl_mode_t -> Double

-- | wrapper for int gsl_sf_airy_Ai_scaled_e(double x,gsl_mode_t mode,gsl_sf_result* result);
airy_Ai_scaled_e :: Double -> Precision -> (Double,Double)
airy_Ai_scaled_e x mode = createSFR "airy_Ai_scaled_e" $ gsl_sf_airy_Ai_scaled_e x  (precCode mode)
foreign import ccall "airy.h gsl_sf_airy_Ai_scaled_e" gsl_sf_airy_Ai_scaled_e :: Double -> Gsl_mode_t -> Ptr Double -> IO(Int)

-- | wrapper for double gsl_sf_airy_Ai_scaled(double x,gsl_mode_t mode);
airy_Ai_scaled :: Double -> Precision -> Double
airy_Ai_scaled x mode = gsl_sf_airy_Ai_scaled x  (precCode mode)
foreign import ccall "airy.h gsl_sf_airy_Ai_scaled" gsl_sf_airy_Ai_scaled :: Double -> Gsl_mode_t -> Double

-- | wrapper for int gsl_sf_airy_Bi_scaled_e(double x,gsl_mode_t mode,gsl_sf_result* result);
airy_Bi_scaled_e :: Double -> Precision -> (Double,Double)
airy_Bi_scaled_e x mode = createSFR "airy_Bi_scaled_e" $ gsl_sf_airy_Bi_scaled_e x  (precCode mode)
foreign import ccall "airy.h gsl_sf_airy_Bi_scaled_e" gsl_sf_airy_Bi_scaled_e :: Double -> Gsl_mode_t -> Ptr Double -> IO(Int)

-- | wrapper for double gsl_sf_airy_Bi_scaled(double x,gsl_mode_t mode);
airy_Bi_scaled :: Double -> Precision -> Double
airy_Bi_scaled x mode = gsl_sf_airy_Bi_scaled x  (precCode mode)
foreign import ccall "airy.h gsl_sf_airy_Bi_scaled" gsl_sf_airy_Bi_scaled :: Double -> Gsl_mode_t -> Double

-- | wrapper for int gsl_sf_airy_Ai_deriv_e(double x,gsl_mode_t mode,gsl_sf_result* result);
airy_Ai_deriv_e :: Double -> Precision -> (Double,Double)
airy_Ai_deriv_e x mode = createSFR "airy_Ai_deriv_e" $ gsl_sf_airy_Ai_deriv_e x  (precCode mode)
foreign import ccall "airy.h gsl_sf_airy_Ai_deriv_e" gsl_sf_airy_Ai_deriv_e :: Double -> Gsl_mode_t -> Ptr Double -> IO(Int)

-- | wrapper for double gsl_sf_airy_Ai_deriv(double x,gsl_mode_t mode);
airy_Ai_deriv :: Double -> Precision -> Double
airy_Ai_deriv x mode = gsl_sf_airy_Ai_deriv x  (precCode mode)
foreign import ccall "airy.h gsl_sf_airy_Ai_deriv" gsl_sf_airy_Ai_deriv :: Double -> Gsl_mode_t -> Double

-- | wrapper for int gsl_sf_airy_Bi_deriv_e(double x,gsl_mode_t mode,gsl_sf_result* result);
airy_Bi_deriv_e :: Double -> Precision -> (Double,Double)
airy_Bi_deriv_e x mode = createSFR "airy_Bi_deriv_e" $ gsl_sf_airy_Bi_deriv_e x  (precCode mode)
foreign import ccall "airy.h gsl_sf_airy_Bi_deriv_e" gsl_sf_airy_Bi_deriv_e :: Double -> Gsl_mode_t -> Ptr Double -> IO(Int)

-- | wrapper for double gsl_sf_airy_Bi_deriv(double x,gsl_mode_t mode);
airy_Bi_deriv :: Double -> Precision -> Double
airy_Bi_deriv x mode = gsl_sf_airy_Bi_deriv x  (precCode mode)
foreign import ccall "airy.h gsl_sf_airy_Bi_deriv" gsl_sf_airy_Bi_deriv :: Double -> Gsl_mode_t -> Double

-- | wrapper for int gsl_sf_airy_Ai_deriv_scaled_e(double x,gsl_mode_t mode,gsl_sf_result* result);
airy_Ai_deriv_scaled_e :: Double -> Precision -> (Double,Double)
airy_Ai_deriv_scaled_e x mode = createSFR "airy_Ai_deriv_scaled_e" $ gsl_sf_airy_Ai_deriv_scaled_e x  (precCode mode)
foreign import ccall "airy.h gsl_sf_airy_Ai_deriv_scaled_e" gsl_sf_airy_Ai_deriv_scaled_e :: Double -> Gsl_mode_t -> Ptr Double -> IO(Int)

-- | wrapper for double gsl_sf_airy_Ai_deriv_scaled(double x,gsl_mode_t mode);
airy_Ai_deriv_scaled :: Double -> Precision -> Double
airy_Ai_deriv_scaled x mode = gsl_sf_airy_Ai_deriv_scaled x  (precCode mode)
foreign import ccall "airy.h gsl_sf_airy_Ai_deriv_scaled" gsl_sf_airy_Ai_deriv_scaled :: Double -> Gsl_mode_t -> Double

-- | wrapper for int gsl_sf_airy_Bi_deriv_scaled_e(double x,gsl_mode_t mode,gsl_sf_result* result);
airy_Bi_deriv_scaled_e :: Double -> Precision -> (Double,Double)
airy_Bi_deriv_scaled_e x mode = createSFR "airy_Bi_deriv_scaled_e" $ gsl_sf_airy_Bi_deriv_scaled_e x  (precCode mode)
foreign import ccall "airy.h gsl_sf_airy_Bi_deriv_scaled_e" gsl_sf_airy_Bi_deriv_scaled_e :: Double -> Gsl_mode_t -> Ptr Double -> IO(Int)

-- | wrapper for double gsl_sf_airy_Bi_deriv_scaled(double x,gsl_mode_t mode);
airy_Bi_deriv_scaled :: Double -> Precision -> Double
airy_Bi_deriv_scaled x mode = gsl_sf_airy_Bi_deriv_scaled x  (precCode mode)
foreign import ccall "airy.h gsl_sf_airy_Bi_deriv_scaled" gsl_sf_airy_Bi_deriv_scaled :: Double -> Gsl_mode_t -> Double

-- | wrapper for int gsl_sf_airy_zero_Ai_e(int s,gsl_sf_result* result);
airy_zero_Ai_e :: Int -> (Double,Double)
airy_zero_Ai_e s = createSFR "airy_zero_Ai_e" $ gsl_sf_airy_zero_Ai_e s
foreign import ccall "airy.h gsl_sf_airy_zero_Ai_e" gsl_sf_airy_zero_Ai_e :: Int -> Ptr Double -> IO(Int)

-- | wrapper for double gsl_sf_airy_zero_Ai(int s);
airy_zero_Ai :: Int -> Double
airy_zero_Ai = gsl_sf_airy_zero_Ai
foreign import ccall "airy.h gsl_sf_airy_zero_Ai" gsl_sf_airy_zero_Ai :: Int -> Double

-- | wrapper for int gsl_sf_airy_zero_Bi_e(int s,gsl_sf_result* result);
airy_zero_Bi_e :: Int -> (Double,Double)
airy_zero_Bi_e s = createSFR "airy_zero_Bi_e" $ gsl_sf_airy_zero_Bi_e s
foreign import ccall "airy.h gsl_sf_airy_zero_Bi_e" gsl_sf_airy_zero_Bi_e :: Int -> Ptr Double -> IO(Int)

-- | wrapper for double gsl_sf_airy_zero_Bi(int s);
airy_zero_Bi :: Int -> Double
airy_zero_Bi = gsl_sf_airy_zero_Bi
foreign import ccall "airy.h gsl_sf_airy_zero_Bi" gsl_sf_airy_zero_Bi :: Int -> Double

-- | wrapper for int gsl_sf_airy_zero_Ai_deriv_e(int s,gsl_sf_result* result);
airy_zero_Ai_deriv_e :: Int -> (Double,Double)
airy_zero_Ai_deriv_e s = createSFR "airy_zero_Ai_deriv_e" $ gsl_sf_airy_zero_Ai_deriv_e s
foreign import ccall "airy.h gsl_sf_airy_zero_Ai_deriv_e" gsl_sf_airy_zero_Ai_deriv_e :: Int -> Ptr Double -> IO(Int)

-- | wrapper for double gsl_sf_airy_zero_Ai_deriv(int s);
airy_zero_Ai_deriv :: Int -> Double
airy_zero_Ai_deriv = gsl_sf_airy_zero_Ai_deriv
foreign import ccall "airy.h gsl_sf_airy_zero_Ai_deriv" gsl_sf_airy_zero_Ai_deriv :: Int -> Double

-- | wrapper for int gsl_sf_airy_zero_Bi_deriv_e(int s,gsl_sf_result* result);
airy_zero_Bi_deriv_e :: Int -> (Double,Double)
airy_zero_Bi_deriv_e s = createSFR "airy_zero_Bi_deriv_e" $ gsl_sf_airy_zero_Bi_deriv_e s
foreign import ccall "airy.h gsl_sf_airy_zero_Bi_deriv_e" gsl_sf_airy_zero_Bi_deriv_e :: Int -> Ptr Double -> IO(Int)

-- | wrapper for double gsl_sf_airy_zero_Bi_deriv(int s);
airy_zero_Bi_deriv :: Int -> Double
airy_zero_Bi_deriv = gsl_sf_airy_zero_Bi_deriv
foreign import ccall "airy.h gsl_sf_airy_zero_Bi_deriv" gsl_sf_airy_zero_Bi_deriv :: Int -> Double
