-- Module      :  Numeric.GSL.Internal
-- Copyright   :  (c) Alberto Ruiz 2009
-- License     :  GPL
--
-- Maintainer  :  Alberto Ruiz (aruiz at um dot es)
-- Stability   :  provisional
-- Portability :  uses ffi
--
-- Auxiliary functions.
--
-- #hide

module Numeric.GSL.Internal where

import Data.Packed.Internal
import Foreign
import Foreign.C.Types(CInt)

iv :: (Vector Double -> Double) -> (CInt -> Ptr Double -> Double)
iv f n p = f (createV (fromIntegral n) copy "iv") where
    copy n' q = do
        copyArray q p (fromIntegral n')
        return 0

-- | conversion of Haskell functions into function pointers that can be used in the C side
foreign import ccall "wrapper"
    mkVecfun :: (CInt -> Ptr Double -> Double)
             -> IO( FunPtr (CInt -> Ptr Double -> Double))

foreign import ccall "wrapper"
    mkVecVecfun :: TVV -> IO (FunPtr TVV)

foreign import ccall "wrapper"
    mkDoubleVecVecfun :: (Double -> TVV) -> IO (FunPtr (Double -> TVV))

aux_vTov :: (Vector Double -> Vector Double) -> TVV
aux_vTov f n p nr r = g where
    V {fptr = pr} = f x
    x = createV (fromIntegral n) copy "aux_vTov"
    copy n' q = do
        copyArray q p (fromIntegral n')
        return 0
    g = do withForeignPtr pr $ \p' -> copyArray r p' (fromIntegral nr)
           return 0

foreign import ccall "wrapper"
    mkVecMatfun :: TVM -> IO (FunPtr TVM)

foreign import ccall "wrapper"
    mkDoubleVecMatfun :: (Double -> TVM) -> IO (FunPtr (Double -> TVM))

aux_vTom :: (Vector Double -> Matrix Double) -> TVM
aux_vTom f n p rr cr r = g where
    V {fptr = pr} = flatten $ f x
    x = createV (fromIntegral n) copy "aux_vTov"
    copy n' q = do
        copyArray q p (fromIntegral n')
        return 0
    g = do withForeignPtr pr $ \p' -> copyArray r p' (fromIntegral $ rr*cr)
           return 0

createV n fun msg = unsafePerformIO $ do
    r <- createVector n
    app1 fun vec r msg
    return r

createMIO r c fun msg = do
    res <- createMatrix RowMajor r c
    app1 fun mat res msg
    return res
