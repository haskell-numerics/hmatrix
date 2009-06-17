------------------------------------------------------------
-- |
-- Module      :  Numeric.GSL.Special.Elljac
-- Copyright   :  (c) Alberto Ruiz 2006
-- License     :  GPL
-- Maintainer  :  Alberto Ruiz (aruiz at um dot es)
-- Stability   :  provisional
-- Portability :  uses ffi
--
-- Wrappers for selected functions described at:
--
-- <http://www.google.com/search?q=gsl_sf_elljac.h&as_sitesearch=www.gnu.org/software/gsl/manual&btnI=Lucky>
------------------------------------------------------------

module Numeric.GSL.Special.Elljac(
elljac_e
) where

import Foreign
import Foreign.C.Types(CInt)

elljac_e :: Double -> Double -> (Double,Double,Double)
elljac_e u m = unsafePerformIO $ do
    psn <- malloc
    pcn <- malloc
    pdn <- malloc
    res <- gsl_sf_elljac_e u m psn pcn pdn
    sn <- peek psn
    cn <- peek pcn
    dn <- peek pdn
    free psn
    free pcn
    free pdn
    if res == 0 then return (sn,cn,dn)
                else error $ "error code "++show res++
                             " in elljac_e "++show u++" "++show m

foreign import ccall "gsl_sf_elljac_e" gsl_sf_elljac_e :: Double -> Double -> Ptr Double -> Ptr Double -> Ptr Double -> IO CInt
