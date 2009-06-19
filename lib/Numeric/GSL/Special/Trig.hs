------------------------------------------------------------
-- |
-- Module      :  Numeric.GSL.Special.Trig
-- Copyright   :  (c) Alberto Ruiz 2006
-- License     :  GPL
-- Maintainer  :  Alberto Ruiz (aruiz at um dot es)
-- Stability   :  provisional
-- Portability :  uses ffi
--
-- Wrappers for selected functions described at:
--
-- <http://www.google.com/search?q=gsl_sf_trig.h&as_sitesearch=www.gnu.org/software/gsl/manual&btnI=Lucky>
------------------------------------------------------------

module Numeric.GSL.Special.Trig(
  sin_e
, Numeric.GSL.Special.Trig.sin
, cos_e
, Numeric.GSL.Special.Trig.cos
, hypot_e
, hypot
, sinc_e
, sinc
, lnsinh_e
, lnsinh
, lncosh_e
, lncosh
, sin_err_e
, cos_err_e
, angle_restrict_symm
, angle_restrict_pos
, angle_restrict_symm_err_e
, angle_restrict_pos_err_e
) where

import Foreign(Ptr)
import Foreign.C.Types(CInt)
import Numeric.GSL.Special.Internal

sin_e :: Double -> (Double,Double)
sin_e x = createSFR "sin_e" $ gsl_sf_sin_e x
foreign import ccall "gsl_sf_sin_e" gsl_sf_sin_e :: Double -> Ptr () -> IO CInt

sin :: Double -> Double
sin = gsl_sf_sin
foreign import ccall "gsl_sf_sin" gsl_sf_sin :: Double -> Double

cos_e :: Double -> (Double,Double)
cos_e x = createSFR "cos_e" $ gsl_sf_cos_e x
foreign import ccall "gsl_sf_cos_e" gsl_sf_cos_e :: Double -> Ptr () -> IO CInt

cos :: Double -> Double
cos = gsl_sf_cos
foreign import ccall "gsl_sf_cos" gsl_sf_cos :: Double -> Double

hypot_e :: Double -> Double -> (Double,Double)
hypot_e x y = createSFR "hypot_e" $ gsl_sf_hypot_e x y
foreign import ccall "gsl_sf_hypot_e" gsl_sf_hypot_e :: Double -> Double -> Ptr () -> IO CInt

hypot :: Double -> Double -> Double
hypot = gsl_sf_hypot
foreign import ccall "gsl_sf_hypot" gsl_sf_hypot :: Double -> Double -> Double

complex_sin_e :: Double -> Double -> Ptr () -> (Double,Double)
complex_sin_e zr zi szr = createSFR "complex_sin_e" $ gsl_sf_complex_sin_e zr zi szr
foreign import ccall "gsl_sf_complex_sin_e" gsl_sf_complex_sin_e :: Double -> Double -> Ptr () -> Ptr () -> IO CInt

complex_cos_e :: Double -> Double -> Ptr () -> (Double,Double)
complex_cos_e zr zi czr = createSFR "complex_cos_e" $ gsl_sf_complex_cos_e zr zi czr
foreign import ccall "gsl_sf_complex_cos_e" gsl_sf_complex_cos_e :: Double -> Double -> Ptr () -> Ptr () -> IO CInt

complex_logsin_e :: Double -> Double -> Ptr () -> (Double,Double)
complex_logsin_e zr zi lszr = createSFR "complex_logsin_e" $ gsl_sf_complex_logsin_e zr zi lszr
foreign import ccall "gsl_sf_complex_logsin_e" gsl_sf_complex_logsin_e :: Double -> Double -> Ptr () -> Ptr () -> IO CInt

sinc_e :: Double -> (Double,Double)
sinc_e x = createSFR "sinc_e" $ gsl_sf_sinc_e x
foreign import ccall "gsl_sf_sinc_e" gsl_sf_sinc_e :: Double -> Ptr () -> IO CInt

sinc :: Double -> Double
sinc = gsl_sf_sinc
foreign import ccall "gsl_sf_sinc" gsl_sf_sinc :: Double -> Double

lnsinh_e :: Double -> (Double,Double)
lnsinh_e x = createSFR "lnsinh_e" $ gsl_sf_lnsinh_e x
foreign import ccall "gsl_sf_lnsinh_e" gsl_sf_lnsinh_e :: Double -> Ptr () -> IO CInt

lnsinh :: Double -> Double
lnsinh = gsl_sf_lnsinh
foreign import ccall "gsl_sf_lnsinh" gsl_sf_lnsinh :: Double -> Double

lncosh_e :: Double -> (Double,Double)
lncosh_e x = createSFR "lncosh_e" $ gsl_sf_lncosh_e x
foreign import ccall "gsl_sf_lncosh_e" gsl_sf_lncosh_e :: Double -> Ptr () -> IO CInt

lncosh :: Double -> Double
lncosh = gsl_sf_lncosh
foreign import ccall "gsl_sf_lncosh" gsl_sf_lncosh :: Double -> Double

polar_to_rect :: Double -> Double -> Ptr () -> (Double,Double)
polar_to_rect r theta x = createSFR "polar_to_rect" $ gsl_sf_polar_to_rect r theta x
foreign import ccall "gsl_sf_polar_to_rect" gsl_sf_polar_to_rect :: Double -> Double -> Ptr () -> Ptr () -> IO CInt

rect_to_polar :: Double -> Double -> Ptr () -> (Double,Double)
rect_to_polar x y r = createSFR "rect_to_polar" $ gsl_sf_rect_to_polar x y r
foreign import ccall "gsl_sf_rect_to_polar" gsl_sf_rect_to_polar :: Double -> Double -> Ptr () -> Ptr () -> IO CInt

sin_err_e :: Double -> Double -> (Double,Double)
sin_err_e x dx = createSFR "sin_err_e" $ gsl_sf_sin_err_e x dx
foreign import ccall "gsl_sf_sin_err_e" gsl_sf_sin_err_e :: Double -> Double -> Ptr () -> IO CInt

cos_err_e :: Double -> Double -> (Double,Double)
cos_err_e x dx = createSFR "cos_err_e" $ gsl_sf_cos_err_e x dx
foreign import ccall "gsl_sf_cos_err_e" gsl_sf_cos_err_e :: Double -> Double -> Ptr () -> IO CInt

angle_restrict_symm_e :: Ptr Double -> CInt
angle_restrict_symm_e = gsl_sf_angle_restrict_symm_e
foreign import ccall "gsl_sf_angle_restrict_symm_e" gsl_sf_angle_restrict_symm_e :: Ptr Double -> CInt

angle_restrict_symm :: Double -> Double
angle_restrict_symm = gsl_sf_angle_restrict_symm
foreign import ccall "gsl_sf_angle_restrict_symm" gsl_sf_angle_restrict_symm :: Double -> Double

angle_restrict_pos_e :: Ptr Double -> CInt
angle_restrict_pos_e = gsl_sf_angle_restrict_pos_e
foreign import ccall "gsl_sf_angle_restrict_pos_e" gsl_sf_angle_restrict_pos_e :: Ptr Double -> CInt

angle_restrict_pos :: Double -> Double
angle_restrict_pos = gsl_sf_angle_restrict_pos
foreign import ccall "gsl_sf_angle_restrict_pos" gsl_sf_angle_restrict_pos :: Double -> Double

angle_restrict_symm_err_e :: Double -> (Double,Double)
angle_restrict_symm_err_e theta = createSFR "angle_restrict_symm_err_e" $ gsl_sf_angle_restrict_symm_err_e theta
foreign import ccall "gsl_sf_angle_restrict_symm_err_e" gsl_sf_angle_restrict_symm_err_e :: Double -> Ptr () -> IO CInt

angle_restrict_pos_err_e :: Double -> (Double,Double)
angle_restrict_pos_err_e theta = createSFR "angle_restrict_pos_err_e" $ gsl_sf_angle_restrict_pos_err_e theta
foreign import ccall "gsl_sf_angle_restrict_pos_err_e" gsl_sf_angle_restrict_pos_err_e :: Double -> Ptr () -> IO CInt
