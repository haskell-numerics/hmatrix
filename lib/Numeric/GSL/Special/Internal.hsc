 {-# LANGUAGE ForeignFunctionInterface #-}
-----------------------------------------------------------------------------
{- |
Module      :  Numeric.GSL.Special.Internal
Copyright   :  (c) Alberto Ruiz 2007
License     :  GPL-style

Maintainer  :  Alberto Ruiz (aruiz at um dot es)
Stability   :  provisional
Portability :  uses ffi

Support for Special functions.

<http://www.gnu.org/software/gsl/manual/html_node/Special-Functions.html#Special-Functions>
-}
-----------------------------------------------------------------------------

#include <gsl/gsl_sf_result.h>
#let alignment t = "%lu", (unsigned long)offsetof(struct {char x__; t (y__); }, y__)

module Numeric.GSL.Special.Internal (
    createSFR,
    createSFR_E10,
    Precision(..),
    Gsl_mode_t,
    Size_t,
    precCode
)
where

import Foreign
import Data.Packed.Internal(check,(//))
import Foreign.C.Types(CSize,CInt)


data Precision = PrecDouble | PrecSingle | PrecApprox

precCode :: Precision -> Int
precCode PrecDouble = 0
precCode PrecSingle = 1
precCode PrecApprox = 2

type Gsl_mode_t = Int

type Size_t = CSize

---------------------------------------------------

data Gsl_sf_result = SF Double Double
  deriving (Show)

instance Storable Gsl_sf_result where
  sizeOf _ = #size gsl_sf_result
  alignment _ = #alignment gsl_sf_result
  peek ptr = do
    val <- (#peek gsl_sf_result, val) ptr
    err <- (#peek gsl_sf_result, err) ptr
    return (SF val err)
  poke ptr (SF val err) = do
    (#poke gsl_sf_result, val) ptr val
    (#poke gsl_sf_result, err) ptr err


data Gsl_sf_result_e10 = SFE Double Double CInt
  deriving (Show)

instance Storable Gsl_sf_result_e10 where
  sizeOf _ = #size gsl_sf_result_e10
  alignment _ = #alignment gsl_sf_result_e10
  peek ptr = do
    val <- (#peek gsl_sf_result_e10, val) ptr
    err <- (#peek gsl_sf_result_e10, err) ptr
    e10 <- (#peek gsl_sf_result_e10, e10) ptr
    return (SFE val err e10)
  poke ptr (SFE val err e10) = do
    (#poke gsl_sf_result_e10, val) ptr val
    (#poke gsl_sf_result_e10, err) ptr err
    (#poke gsl_sf_result_e10, e10) ptr e10


----------------------------------------------------------------
-- | access to a sf_result
createSFR :: String -> (Ptr a -> IO CInt) -> (Double, Double)
createSFR s f = unsafePerformIO $ do
    p <- malloc :: IO (Ptr Gsl_sf_result)
    f (castPtr p) // check s
    SF val err <- peek p
    free p
    return (val,err)


---------------------------------------------------------------------
-- the sf_result_e10 contains two doubles and the exponent

-- | access to sf_result_e10
createSFR_E10 :: String -> (Ptr a -> IO CInt) -> (Double, Int, Double)
createSFR_E10 s f = unsafePerformIO $ do
    p <- malloc :: IO (Ptr Gsl_sf_result_e10)
    f (castPtr p) // check s
    SFE val err expo  <- peek p
    free p
    return (val, fromIntegral expo,err)
