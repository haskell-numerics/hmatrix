{-# OPTIONS_GHC -fglasgow-exts #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Numeric.GSL.Vector
-- Copyright   :  (c) Alberto Ruiz 2007
-- License     :  GPL-style
--
-- Maintainer  :  Alberto Ruiz <aruiz@um.es>
-- Stability   :  provisional
-- Portability :  portable (uses FFI)
--
-- Low level interface to vector operations.
--
-----------------------------------------------------------------------------

module Numeric.GSL.Vector (
    FunCodeS(..), toScalarR,
    FunCodeV(..), vectorMapR, vectorMapC,
    FunCodeSV(..), vectorMapValR, vectorMapValC,
    FunCodeVV(..), vectorZipR, vectorZipC,
    RandDist(..), randomVector
) where

import Data.Packed.Internal.Common
import Data.Packed.Internal.Signatures
import Data.Packed.Internal.Vector

import Complex
import Foreign
import Foreign.C.Types(CInt)

fromei x = fromIntegral (fromEnum x) :: CInt

data FunCodeV = Sin
              | Cos
              | Tan
              | Abs
              | ASin
              | ACos
              | ATan
              | Sinh
              | Cosh
              | Tanh
              | ASinh
              | ACosh
              | ATanh
              | Exp
              | Log
              | Sign
              | Sqrt
              deriving Enum

data FunCodeSV = Scale
               | Recip
               | AddConstant
               | Negate
               | PowSV
               | PowVS
               deriving Enum

data FunCodeVV = Add
               | Sub
               | Mul
               | Div
               | Pow
               | ATan2
               deriving Enum

data FunCodeS = Norm2
              | AbsSum
              | MaxIdx
              | Max
              | MinIdx
              | Min
              deriving Enum

------------------------------------------------------------------

toScalarAux fun code v = unsafePerformIO $ do
    r <- createVector 1
    app2 (fun (fromei code)) vec v vec r "toScalarAux"
    return (r `at` 0)

vectorMapAux fun code v = unsafePerformIO $ do
    r <- createVector (dim v)
    app2 (fun (fromei code)) vec v vec r "vectorMapAux"
    return r

vectorMapValAux fun code val v = unsafePerformIO $ do
    r <- createVector (dim v)
    pval <- newArray [val]
    app2 (fun (fromei code) pval) vec v vec r "vectorMapValAux"
    free pval
    return r

vectorZipAux fun code u v = unsafePerformIO $ do
    r <- createVector (dim u)
    app3 (fun (fromei code)) vec u vec v vec r "vectorZipAux"
    return r

---------------------------------------------------------------------

-- | obtains different functions of a vector: norm1, norm2, max, min, posmax, posmin, etc.
toScalarR :: FunCodeS -> Vector Double -> Double
toScalarR oper =  toScalarAux c_toScalarR (fromei oper)

foreign import ccall safe "gsl-aux.h toScalarR" c_toScalarR :: CInt -> TVV

------------------------------------------------------------------

-- | map of real vectors with given function
vectorMapR :: FunCodeV -> Vector Double -> Vector Double
vectorMapR = vectorMapAux c_vectorMapR

foreign import ccall safe "gsl-aux.h mapR" c_vectorMapR :: CInt -> TVV

-- | map of complex vectors with given function
vectorMapC :: FunCodeV -> Vector (Complex Double) -> Vector (Complex Double)
vectorMapC oper = vectorMapAux c_vectorMapC (fromei oper)

foreign import ccall safe "gsl-aux.h mapC" c_vectorMapC :: CInt -> TCVCV

-------------------------------------------------------------------

-- | map of real vectors with given function
vectorMapValR :: FunCodeSV -> Double -> Vector Double -> Vector Double
vectorMapValR oper = vectorMapValAux c_vectorMapValR (fromei oper)

foreign import ccall safe "gsl-aux.h mapValR" c_vectorMapValR :: CInt -> Ptr Double -> TVV

-- | map of complex vectors with given function
vectorMapValC :: FunCodeSV -> Complex Double -> Vector (Complex Double) -> Vector (Complex Double)
vectorMapValC = vectorMapValAux c_vectorMapValC

foreign import ccall safe "gsl-aux.h mapValC" c_vectorMapValC :: CInt -> Ptr (Complex Double) -> TCVCV

-------------------------------------------------------------------

-- | elementwise operation on real vectors
vectorZipR :: FunCodeVV -> Vector Double -> Vector Double -> Vector Double
vectorZipR = vectorZipAux c_vectorZipR

foreign import ccall safe "gsl-aux.h zipR" c_vectorZipR :: CInt -> TVVV

-- | elementwise operation on complex vectors
vectorZipC :: FunCodeVV -> Vector (Complex Double) -> Vector (Complex Double) -> Vector (Complex Double)
vectorZipC = vectorZipAux c_vectorZipC

foreign import ccall safe "gsl-aux.h zipC" c_vectorZipC :: CInt -> TCVCVCV

-----------------------------------------------------------------------

data RandDist = Uniform  -- ^ uniform distribution in [0,1)
              | Gaussian -- ^ normal distribution with mean zero and standard deviation one
              deriving Enum

-- | Obtains a vector of pseudorandom elements from the the mt19937 generator in GSL, with a given seed. Use randomIO to get a random seed.
randomVector :: Int      -- ^ seed
             -> RandDist -- ^ distribution
             -> Int      -- ^ vector size
             -> Vector Double
randomVector seed dist n = unsafePerformIO $ do
    r <- createVector n
    app1 (c_random_vector (fi seed) ((fi.fromEnum) dist)) vec r "randomVector"
    return r

foreign import ccall safe "random_vector" c_random_vector :: CInt -> CInt -> TV
