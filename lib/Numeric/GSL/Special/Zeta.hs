------------------------------------------------------------
-- |
-- Module      :  Numeric.GSL.Special.Zeta
-- Copyright   :  (c) Alberto Ruiz 2006
-- License     :  GPL
-- Maintainer  :  Alberto Ruiz (aruiz at um dot es)
-- Stability   :  provisional
-- Portability :  uses ffi
--
-- Wrappers for selected functions described at:
--
-- <http://www.google.com/search?q=gsl_sf_zeta.h&as_sitesearch=www.gnu.org/software/gsl/manual&btnI=Lucky>
------------------------------------------------------------

module Numeric.GSL.Special.Zeta(
  zeta_int_e
, zeta_int
, zeta_e
, zeta
, zetam1_e
, zetam1
, zetam1_int_e
, zetam1_int
, hzeta_e
, hzeta
, eta_int_e
, eta_int
, eta_e
, eta
) where

import Foreign(Ptr)
import Foreign.C.Types(CInt)
import Numeric.GSL.Special.Internal

zeta_int_e :: CInt -> (Double,Double)
zeta_int_e n = createSFR "zeta_int_e" $ gsl_sf_zeta_int_e n
foreign import ccall "gsl_sf_zeta_int_e" gsl_sf_zeta_int_e :: CInt -> Ptr () -> IO CInt

zeta_int :: CInt -> Double
zeta_int = gsl_sf_zeta_int
foreign import ccall "gsl_sf_zeta_int" gsl_sf_zeta_int :: CInt -> Double

zeta_e :: Double -> (Double,Double)
zeta_e s = createSFR "zeta_e" $ gsl_sf_zeta_e s
foreign import ccall "gsl_sf_zeta_e" gsl_sf_zeta_e :: Double -> Ptr () -> IO CInt

zeta :: Double -> Double
zeta = gsl_sf_zeta
foreign import ccall "gsl_sf_zeta" gsl_sf_zeta :: Double -> Double

zetam1_e :: Double -> (Double,Double)
zetam1_e s = createSFR "zetam1_e" $ gsl_sf_zetam1_e s
foreign import ccall "gsl_sf_zetam1_e" gsl_sf_zetam1_e :: Double -> Ptr () -> IO CInt

zetam1 :: Double -> Double
zetam1 = gsl_sf_zetam1
foreign import ccall "gsl_sf_zetam1" gsl_sf_zetam1 :: Double -> Double

zetam1_int_e :: CInt -> (Double,Double)
zetam1_int_e s = createSFR "zetam1_int_e" $ gsl_sf_zetam1_int_e s
foreign import ccall "gsl_sf_zetam1_int_e" gsl_sf_zetam1_int_e :: CInt -> Ptr () -> IO CInt

zetam1_int :: CInt -> Double
zetam1_int = gsl_sf_zetam1_int
foreign import ccall "gsl_sf_zetam1_int" gsl_sf_zetam1_int :: CInt -> Double

hzeta_e :: Double -> Double -> (Double,Double)
hzeta_e s q = createSFR "hzeta_e" $ gsl_sf_hzeta_e s q
foreign import ccall "gsl_sf_hzeta_e" gsl_sf_hzeta_e :: Double -> Double -> Ptr () -> IO CInt

hzeta :: Double -> Double -> Double
hzeta = gsl_sf_hzeta
foreign import ccall "gsl_sf_hzeta" gsl_sf_hzeta :: Double -> Double -> Double

eta_int_e :: CInt -> (Double,Double)
eta_int_e n = createSFR "eta_int_e" $ gsl_sf_eta_int_e n
foreign import ccall "gsl_sf_eta_int_e" gsl_sf_eta_int_e :: CInt -> Ptr () -> IO CInt

eta_int :: CInt -> Double
eta_int = gsl_sf_eta_int
foreign import ccall "gsl_sf_eta_int" gsl_sf_eta_int :: CInt -> Double

eta_e :: Double -> (Double,Double)
eta_e s = createSFR "eta_e" $ gsl_sf_eta_e s
foreign import ccall "gsl_sf_eta_e" gsl_sf_eta_e :: Double -> Ptr () -> IO CInt

eta :: Double -> Double
eta = gsl_sf_eta
foreign import ccall "gsl_sf_eta" gsl_sf_eta :: Double -> Double
