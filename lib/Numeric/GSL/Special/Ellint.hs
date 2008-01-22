------------------------------------------------------------
{- |
Module      :  Numeric.GSL.Special.Ellint
Copyright   :  (c) Alberto Ruiz 2006
License     :  GPL-style
Maintainer  :  Alberto Ruiz (aruiz at um dot es)
Stability   :  provisional
Portability :  uses ffi

Wrappers for selected functions described at:

<http://www.google.com/search?q=gsl_sf_ellint.h&as_sitesearch=www.gnu.org/software/gsl/manual&btnI=Lucky>

-}
------------------------------------------------------------

module Numeric.GSL.Special.Ellint(
  ellint_Kcomp_e
, ellint_Kcomp
, ellint_Ecomp_e
, ellint_Ecomp
--, ellint_Pcomp_e
--, ellint_Pcomp
--, ellint_Dcomp_e
--, ellint_Dcomp
, ellint_F_e
, ellint_F
, ellint_E_e
, ellint_E
, ellint_P_e
, ellint_P
, ellint_D_e
, ellint_D
, ellint_RC_e
, ellint_RC
, ellint_RD_e
, ellint_RD
, ellint_RF_e
, ellint_RF
, ellint_RJ_e
, ellint_RJ
) where

import Foreign(Ptr)
import Foreign.C.Types(CInt)
import Numeric.GSL.Special.Internal

-- | wrapper for int gsl_sf_ellint_Kcomp_e(double k,gsl_mode_t mode,gsl_sf_result* result);
--
--   <http://www.google.com/search?q=gsl_sf_ellint_Kcomp_e&as_sitesearch=www.gnu.org/software/gsl/manual&btnI=Lucky>
ellint_Kcomp_e :: Double -> Precision -> (Double,Double)
ellint_Kcomp_e k mode = createSFR "ellint_Kcomp_e" $ gsl_sf_ellint_Kcomp_e k  (precCode mode)
foreign import ccall "ellint.h gsl_sf_ellint_Kcomp_e" gsl_sf_ellint_Kcomp_e :: Double -> Gsl_mode_t -> Ptr () -> IO CInt

-- | wrapper for double gsl_sf_ellint_Kcomp(double k,gsl_mode_t mode);
--
--   <http://www.google.com/search?q=gsl_sf_ellint_Kcomp&as_sitesearch=www.gnu.org/software/gsl/manual&btnI=Lucky>
ellint_Kcomp :: Double -> Precision -> Double
ellint_Kcomp k mode = gsl_sf_ellint_Kcomp k  (precCode mode)
foreign import ccall "ellint.h gsl_sf_ellint_Kcomp" gsl_sf_ellint_Kcomp :: Double -> Gsl_mode_t -> Double

-- | wrapper for int gsl_sf_ellint_Ecomp_e(double k,gsl_mode_t mode,gsl_sf_result* result);
--
--   <http://www.google.com/search?q=gsl_sf_ellint_Ecomp_e&as_sitesearch=www.gnu.org/software/gsl/manual&btnI=Lucky>
ellint_Ecomp_e :: Double -> Precision -> (Double,Double)
ellint_Ecomp_e k mode = createSFR "ellint_Ecomp_e" $ gsl_sf_ellint_Ecomp_e k  (precCode mode)
foreign import ccall "ellint.h gsl_sf_ellint_Ecomp_e" gsl_sf_ellint_Ecomp_e :: Double -> Gsl_mode_t -> Ptr () -> IO CInt

-- | wrapper for double gsl_sf_ellint_Ecomp(double k,gsl_mode_t mode);
--
--   <http://www.google.com/search?q=gsl_sf_ellint_Ecomp&as_sitesearch=www.gnu.org/software/gsl/manual&btnI=Lucky>
ellint_Ecomp :: Double -> Precision -> Double
ellint_Ecomp k mode = gsl_sf_ellint_Ecomp k  (precCode mode)
foreign import ccall "ellint.h gsl_sf_ellint_Ecomp" gsl_sf_ellint_Ecomp :: Double -> Gsl_mode_t -> Double

-- | wrapper for int gsl_sf_ellint_Pcomp_e(double k,double n,gsl_mode_t mode,gsl_sf_result* result);
--
--   <http://www.google.com/search?q=gsl_sf_ellint_Pcomp_e&as_sitesearch=www.gnu.org/software/gsl/manual&btnI=Lucky>
--ellint_Pcomp_e :: Double -> Double -> Precision -> (Double,Double)
--ellint_Pcomp_e k n mode = createSFR "ellint_Pcomp_e" $ gsl_sf_ellint_Pcomp_e k n  (precCode mode)
--foreign import ccall "ellint.h gsl_sf_ellint_Pcomp_e" gsl_sf_ellint_Pcomp_e :: Double -> Double -> Gsl_mode_t -> Ptr () -> IO CInt

-- | wrapper for double gsl_sf_ellint_Pcomp(double k,double n,gsl_mode_t mode);
--
--   <http://www.google.com/search?q=gsl_sf_ellint_Pcomp&as_sitesearch=www.gnu.org/software/gsl/manual&btnI=Lucky>
--ellint_Pcomp :: Double -> Double -> Precision -> Double
--ellint_Pcomp k n mode = gsl_sf_ellint_Pcomp k n  (precCode mode)
--foreign import ccall "ellint.h gsl_sf_ellint_Pcomp" gsl_sf_ellint_Pcomp :: Double -> Double -> Gsl_mode_t -> Double

-- | wrapper for int gsl_sf_ellint_Dcomp_e(double k,gsl_mode_t mode,gsl_sf_result* result);
--
--   <http://www.google.com/search?q=gsl_sf_ellint_Dcomp_e&as_sitesearch=www.gnu.org/software/gsl/manual&btnI=Lucky>
--ellint_Dcomp_e :: Double -> Precision -> (Double,Double)
--ellint_Dcomp_e k mode = createSFR "ellint_Dcomp_e" $ gsl_sf_ellint_Dcomp_e k  (precCode mode)
--foreign import ccall "ellint.h gsl_sf_ellint_Dcomp_e" gsl_sf_ellint_Dcomp_e :: Double -> Gsl_mode_t -> Ptr () -> IO CInt

-- | wrapper for double gsl_sf_ellint_Dcomp(double k,gsl_mode_t mode);
--
--   <http://www.google.com/search?q=gsl_sf_ellint_Dcomp&as_sitesearch=www.gnu.org/software/gsl/manual&btnI=Lucky>
--ellint_Dcomp :: Double -> Precision -> Double
--ellint_Dcomp k mode = gsl_sf_ellint_Dcomp k  (precCode mode)
--foreign import ccall "ellint.h gsl_sf_ellint_Dcomp" gsl_sf_ellint_Dcomp :: Double -> Gsl_mode_t -> Double

-- | wrapper for int gsl_sf_ellint_F_e(double phi,double k,gsl_mode_t mode,gsl_sf_result* result);
--
--   <http://www.google.com/search?q=gsl_sf_ellint_F_e&as_sitesearch=www.gnu.org/software/gsl/manual&btnI=Lucky>
ellint_F_e :: Double -> Double -> Precision -> (Double,Double)
ellint_F_e phi k mode = createSFR "ellint_F_e" $ gsl_sf_ellint_F_e phi k  (precCode mode)
foreign import ccall "ellint.h gsl_sf_ellint_F_e" gsl_sf_ellint_F_e :: Double -> Double -> Gsl_mode_t -> Ptr () -> IO CInt

-- | wrapper for double gsl_sf_ellint_F(double phi,double k,gsl_mode_t mode);
--
--   <http://www.google.com/search?q=gsl_sf_ellint_F&as_sitesearch=www.gnu.org/software/gsl/manual&btnI=Lucky>
ellint_F :: Double -> Double -> Precision -> Double
ellint_F phi k mode = gsl_sf_ellint_F phi k  (precCode mode)
foreign import ccall "ellint.h gsl_sf_ellint_F" gsl_sf_ellint_F :: Double -> Double -> Gsl_mode_t -> Double

-- | wrapper for int gsl_sf_ellint_E_e(double phi,double k,gsl_mode_t mode,gsl_sf_result* result);
--
--   <http://www.google.com/search?q=gsl_sf_ellint_E_e&as_sitesearch=www.gnu.org/software/gsl/manual&btnI=Lucky>
ellint_E_e :: Double -> Double -> Precision -> (Double,Double)
ellint_E_e phi k mode = createSFR "ellint_E_e" $ gsl_sf_ellint_E_e phi k  (precCode mode)
foreign import ccall "ellint.h gsl_sf_ellint_E_e" gsl_sf_ellint_E_e :: Double -> Double -> Gsl_mode_t -> Ptr () -> IO CInt

-- | wrapper for double gsl_sf_ellint_E(double phi,double k,gsl_mode_t mode);
--
--   <http://www.google.com/search?q=gsl_sf_ellint_E&as_sitesearch=www.gnu.org/software/gsl/manual&btnI=Lucky>
ellint_E :: Double -> Double -> Precision -> Double
ellint_E phi k mode = gsl_sf_ellint_E phi k  (precCode mode)
foreign import ccall "ellint.h gsl_sf_ellint_E" gsl_sf_ellint_E :: Double -> Double -> Gsl_mode_t -> Double

-- | wrapper for int gsl_sf_ellint_P_e(double phi,double k,double n,gsl_mode_t mode,gsl_sf_result* result);
--
--   <http://www.google.com/search?q=gsl_sf_ellint_P_e&as_sitesearch=www.gnu.org/software/gsl/manual&btnI=Lucky>
ellint_P_e :: Double -> Double -> Double -> Precision -> (Double,Double)
ellint_P_e phi k n mode = createSFR "ellint_P_e" $ gsl_sf_ellint_P_e phi k n  (precCode mode)
foreign import ccall "ellint.h gsl_sf_ellint_P_e" gsl_sf_ellint_P_e :: Double -> Double -> Double -> Gsl_mode_t -> Ptr () -> IO CInt

-- | wrapper for double gsl_sf_ellint_P(double phi,double k,double n,gsl_mode_t mode);
--
--   <http://www.google.com/search?q=gsl_sf_ellint_P&as_sitesearch=www.gnu.org/software/gsl/manual&btnI=Lucky>
ellint_P :: Double -> Double -> Double -> Precision -> Double
ellint_P phi k n mode = gsl_sf_ellint_P phi k n  (precCode mode)
foreign import ccall "ellint.h gsl_sf_ellint_P" gsl_sf_ellint_P :: Double -> Double -> Double -> Gsl_mode_t -> Double

-- | wrapper for int gsl_sf_ellint_D_e(double phi,double k,double n,gsl_mode_t mode,gsl_sf_result* result);
--
--   <http://www.google.com/search?q=gsl_sf_ellint_D_e&as_sitesearch=www.gnu.org/software/gsl/manual&btnI=Lucky>
ellint_D_e :: Double -> Double -> Double -> Precision -> (Double,Double)
ellint_D_e phi k n mode = createSFR "ellint_D_e" $ gsl_sf_ellint_D_e phi k n  (precCode mode)
foreign import ccall "ellint.h gsl_sf_ellint_D_e" gsl_sf_ellint_D_e :: Double -> Double -> Double -> Gsl_mode_t -> Ptr () -> IO CInt

-- | wrapper for double gsl_sf_ellint_D(double phi,double k,double n,gsl_mode_t mode);
--
--   <http://www.google.com/search?q=gsl_sf_ellint_D&as_sitesearch=www.gnu.org/software/gsl/manual&btnI=Lucky>
ellint_D :: Double -> Double -> Double -> Precision -> Double
ellint_D phi k n mode = gsl_sf_ellint_D phi k n  (precCode mode)
foreign import ccall "ellint.h gsl_sf_ellint_D" gsl_sf_ellint_D :: Double -> Double -> Double -> Gsl_mode_t -> Double

-- | wrapper for int gsl_sf_ellint_RC_e(double x,double y,gsl_mode_t mode,gsl_sf_result* result);
--
--   <http://www.google.com/search?q=gsl_sf_ellint_RC_e&as_sitesearch=www.gnu.org/software/gsl/manual&btnI=Lucky>
ellint_RC_e :: Double -> Double -> Precision -> (Double,Double)
ellint_RC_e x y mode = createSFR "ellint_RC_e" $ gsl_sf_ellint_RC_e x y  (precCode mode)
foreign import ccall "ellint.h gsl_sf_ellint_RC_e" gsl_sf_ellint_RC_e :: Double -> Double -> Gsl_mode_t -> Ptr () -> IO CInt

-- | wrapper for double gsl_sf_ellint_RC(double x,double y,gsl_mode_t mode);
--
--   <http://www.google.com/search?q=gsl_sf_ellint_RC&as_sitesearch=www.gnu.org/software/gsl/manual&btnI=Lucky>
ellint_RC :: Double -> Double -> Precision -> Double
ellint_RC x y mode = gsl_sf_ellint_RC x y  (precCode mode)
foreign import ccall "ellint.h gsl_sf_ellint_RC" gsl_sf_ellint_RC :: Double -> Double -> Gsl_mode_t -> Double

-- | wrapper for int gsl_sf_ellint_RD_e(double x,double y,double z,gsl_mode_t mode,gsl_sf_result* result);
--
--   <http://www.google.com/search?q=gsl_sf_ellint_RD_e&as_sitesearch=www.gnu.org/software/gsl/manual&btnI=Lucky>
ellint_RD_e :: Double -> Double -> Double -> Precision -> (Double,Double)
ellint_RD_e x y z mode = createSFR "ellint_RD_e" $ gsl_sf_ellint_RD_e x y z  (precCode mode)
foreign import ccall "ellint.h gsl_sf_ellint_RD_e" gsl_sf_ellint_RD_e :: Double -> Double -> Double -> Gsl_mode_t -> Ptr () -> IO CInt

-- | wrapper for double gsl_sf_ellint_RD(double x,double y,double z,gsl_mode_t mode);
--
--   <http://www.google.com/search?q=gsl_sf_ellint_RD&as_sitesearch=www.gnu.org/software/gsl/manual&btnI=Lucky>
ellint_RD :: Double -> Double -> Double -> Precision -> Double
ellint_RD x y z mode = gsl_sf_ellint_RD x y z  (precCode mode)
foreign import ccall "ellint.h gsl_sf_ellint_RD" gsl_sf_ellint_RD :: Double -> Double -> Double -> Gsl_mode_t -> Double

-- | wrapper for int gsl_sf_ellint_RF_e(double x,double y,double z,gsl_mode_t mode,gsl_sf_result* result);
--
--   <http://www.google.com/search?q=gsl_sf_ellint_RF_e&as_sitesearch=www.gnu.org/software/gsl/manual&btnI=Lucky>
ellint_RF_e :: Double -> Double -> Double -> Precision -> (Double,Double)
ellint_RF_e x y z mode = createSFR "ellint_RF_e" $ gsl_sf_ellint_RF_e x y z  (precCode mode)
foreign import ccall "ellint.h gsl_sf_ellint_RF_e" gsl_sf_ellint_RF_e :: Double -> Double -> Double -> Gsl_mode_t -> Ptr () -> IO CInt

-- | wrapper for double gsl_sf_ellint_RF(double x,double y,double z,gsl_mode_t mode);
--
--   <http://www.google.com/search?q=gsl_sf_ellint_RF&as_sitesearch=www.gnu.org/software/gsl/manual&btnI=Lucky>
ellint_RF :: Double -> Double -> Double -> Precision -> Double
ellint_RF x y z mode = gsl_sf_ellint_RF x y z  (precCode mode)
foreign import ccall "ellint.h gsl_sf_ellint_RF" gsl_sf_ellint_RF :: Double -> Double -> Double -> Gsl_mode_t -> Double

-- | wrapper for int gsl_sf_ellint_RJ_e(double x,double y,double z,double p,gsl_mode_t mode,gsl_sf_result* result);
--
--   <http://www.google.com/search?q=gsl_sf_ellint_RJ_e&as_sitesearch=www.gnu.org/software/gsl/manual&btnI=Lucky>
ellint_RJ_e :: Double -> Double -> Double -> Double -> Precision -> (Double,Double)
ellint_RJ_e x y z p mode = createSFR "ellint_RJ_e" $ gsl_sf_ellint_RJ_e x y z p  (precCode mode)
foreign import ccall "ellint.h gsl_sf_ellint_RJ_e" gsl_sf_ellint_RJ_e :: Double -> Double -> Double -> Double -> Gsl_mode_t -> Ptr () -> IO CInt

-- | wrapper for double gsl_sf_ellint_RJ(double x,double y,double z,double p,gsl_mode_t mode);
--
--   <http://www.google.com/search?q=gsl_sf_ellint_RJ&as_sitesearch=www.gnu.org/software/gsl/manual&btnI=Lucky>
ellint_RJ :: Double -> Double -> Double -> Double -> Precision -> Double
ellint_RJ x y z p mode = gsl_sf_ellint_RJ x y z p  (precCode mode)
foreign import ccall "ellint.h gsl_sf_ellint_RJ" gsl_sf_ellint_RJ :: Double -> Double -> Double -> Double -> Gsl_mode_t -> Double
