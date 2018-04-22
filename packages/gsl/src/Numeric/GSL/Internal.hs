{-# LANGUAGE FlexibleContexts #-}

{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# OPTIONS_GHC -fno-warn-unused-top-binds #-}

-- |
-- Module      :  Numeric.GSL.Internal
-- Copyright   :  (c) Alberto Ruiz 2009
-- License     :  GPL
-- Maintainer  :  Alberto Ruiz
-- Stability   :  provisional
--
--
-- Auxiliary functions.
--


module Numeric.GSL.Internal(
    iv,
    mkVecfun,
    mkVecVecfun,
    mkDoubleVecVecfun,
    mkDoublefun,
    aux_vTov,
    mkVecMatfun,
    mkDoubleVecMatfun,
    aux_vTom,
    createV,
    createMIO,
    module Numeric.LinearAlgebra.Devel,
    check,(#),(#!),vec, ww2,
    Res,TV,TM,TCV,TCM
) where

import Numeric.LinearAlgebra.HMatrix
import Numeric.LinearAlgebra.Devel hiding (check)

import Foreign.Marshal.Array(copyArray)
import Foreign.Ptr(Ptr, FunPtr)
import Foreign.C.Types
import Foreign.C.String(peekCString)
import System.IO.Unsafe(unsafePerformIO)
import Data.Vector.Storable as V (unsafeWith,length)
import Control.Monad(when)

iv :: (Vector Double -> Double) -> (CInt -> Ptr Double -> Double)
iv f n p = f (createV (fromIntegral n) copy "iv") where
    copy n' q = do
        copyArray q p (fromIntegral n')
        return 0

-- | conversion of Haskell functions into function pointers that can be used in the C side
foreign import ccall safe "wrapper"
    mkVecfun :: (CInt -> Ptr Double -> Double)
             -> IO( FunPtr (CInt -> Ptr Double -> Double))

foreign import ccall safe "wrapper"
    mkVecVecfun :: TVV -> IO (FunPtr TVV)

foreign import ccall safe "wrapper"
    mkDoubleVecVecfun :: (Double -> TVV) -> IO (FunPtr (Double -> TVV))

foreign import ccall safe "wrapper"
    mkDoublefun :: (Double -> Double) -> IO (FunPtr (Double -> Double))

aux_vTov :: (Vector Double -> Vector Double) -> TVV
aux_vTov f n p nr r = g where
    v = f x
    x = createV (fromIntegral n) copy "aux_vTov"
    copy n' q = do
        copyArray q p (fromIntegral n')
        return 0
    g = do unsafeWith v $ \p' -> copyArray r p' (fromIntegral nr)
           return 0

foreign import ccall safe "wrapper"
    mkVecMatfun :: TVM -> IO (FunPtr TVM)

foreign import ccall safe "wrapper"
    mkDoubleVecMatfun :: (Double -> TVM) -> IO (FunPtr (Double -> TVM))

aux_vTom :: (Vector Double -> Matrix Double) -> TVM
aux_vTom f n p rr cr r = g where
    v = flatten $ f x
    x = createV (fromIntegral n) copy "aux_vTov"
    copy n' q = do
        copyArray q p (fromIntegral n')
        return 0
    g = do unsafeWith v $ \p' -> copyArray r p' (fromIntegral $ rr*cr)
           return 0

createV n fun msg = unsafePerformIO $ do
    r <- createVector n
    (r # id) fun #| msg
    return r

createMIO r c fun msg = do
    res <- createMatrix RowMajor r c
    (res # id) fun #| msg
    return res

--------------------------------------------------------------------------------

-- | check the error code
check :: String -> IO CInt -> IO ()
check msg f = do
    err <- f
    when (err/=0) $ do
        ps <- gsl_strerror err
        s <- peekCString ps
        error (msg++": "++s)
    return ()

-- | description of GSL error codes
foreign import ccall unsafe "gsl_strerror" gsl_strerror :: CInt -> IO (Ptr CChar)

type PF = Ptr Float
type PD = Ptr Double
type PQ = Ptr (Complex Float)
type PC = Ptr (Complex Double)

type Res = IO CInt
type TV x  = CInt -> PD -> x
type TM x  = CInt -> CInt -> PD -> x
type TCV x = CInt -> PC -> x
type TCM x = CInt -> CInt -> PC -> x

type TVV = TV (TV Res)
type TVM = TV (TM Res)

ww2 w1 o1 w2 o2 f = w1 o1 $ \a1 -> w2 o2 $ \a2 -> f a1 a2

vec x f = unsafeWith x $ \p -> do
    let v g = g (fi $ V.length x) p
    f v
{-# INLINE vec #-}

infixl 1 #
a # b = applyRaw a b
{-# INLINE (#) #-}

--infixr 1 #
--a # b = apply a b
--{-# INLINE (#) #-}

a #! b = a # b # id
{-# INLINE (#!) #-}

