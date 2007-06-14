{-# OPTIONS_GHC -fglasgow-exts #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  GSL.Vector
-- Copyright   :  (c) Alberto Ruiz 2007
-- License     :  GPL-style
--
-- Maintainer  :  Alberto Ruiz <aruiz@um.es>
-- Stability   :  provisional
-- Portability :  portable (uses FFI)
--
-- Vector operations
--
-----------------------------------------------------------------------------

module GSL.Vector (
    FunCodeS(..), toScalarR,
    FunCodeV(..), vectorMapR, vectorMapC,
    FunCodeSV(..), vectorMapValR, vectorMapValC,
    FunCodeVV(..), vectorZipR, vectorZipC,
    scale, addConstant
) where

import Data.Packed.Internal
import Complex
import Foreign

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


scale :: (Num a, Field a) => a -> Vector a -> Vector a
scale x v | isReal baseOf v = scast $ vectorMapValR Scale (scast x) (scast v)
          | isComp baseOf v = scast $ vectorMapValC Scale (scast x) (scast v)
          | otherwise   = fromList $ map (*x) $ toList v

addConstant :: (Num a, Field a) => a -> Vector a -> Vector a
addConstant x v | isReal baseOf v = scast $ vectorMapValR AddConstant (scast x) (scast v)
                | isComp baseOf v = scast $ vectorMapValC AddConstant (scast x) (scast v)
                | otherwise   = fromList $ map (*x) $ toList v

------------------------------------------------------------------

toScalarAux fun code v = unsafePerformIO $ do
    r <- createVector 1
    fun (fromEnum code) // vec v // vec r // check "toScalarAux" [v]
    return (r `at` 0)

vectorMapAux fun code v = unsafePerformIO $ do
    r <- createVector (dim v)
    fun (fromEnum code) // vec v // vec r // check "vectorMapAux" [v]
    return r

vectorMapValAux fun code val v = unsafePerformIO $ do
    r <- createVector (dim v)
    pval <- newArray [val]
    fun (fromEnum code) pval // vec v // vec r // check "vectorMapValAux" [v]
    free pval
    return r

vectorZipAux fun code u v = unsafePerformIO $ do
    r <- createVector (dim u)
    fun (fromEnum code) // vec u // vec v // vec r // check "vectorZipAux" [u,v]
    return r

---------------------------------------------------------------------

-- | obtains different functions of a vector: norm1, norm2, max, min, posmax, posmin, etc.
toScalarR :: FunCodeS -> Vector Double -> Double
toScalarR oper =  toScalarAux c_toScalarR (fromEnum oper)

foreign import ccall safe "gsl-aux.h toScalarR"
    c_toScalarR :: Int -> Double :> Double :> IO Int

------------------------------------------------------------------

-- | map of real vectors with given function
vectorMapR :: FunCodeV -> Vector Double -> Vector Double
vectorMapR = vectorMapAux c_vectorMapR

foreign import ccall safe "gsl-aux.h mapR"
    c_vectorMapR :: Int -> Double :> Double :> IO Int

-- | map of complex vectors with given function
vectorMapC :: FunCodeV -> Vector (Complex Double) -> Vector (Complex Double)
vectorMapC oper = vectorMapAux c_vectorMapC (fromEnum oper)

foreign import ccall safe "gsl-aux.h mapC"
    c_vectorMapC :: Int -> Complex Double :> Complex Double :> IO Int

-------------------------------------------------------------------

-- | map of real vectors with given function
vectorMapValR :: FunCodeSV -> Double -> Vector Double -> Vector Double
vectorMapValR oper = vectorMapValAux c_vectorMapValR (fromEnum oper)

foreign import ccall safe "gsl-aux.h mapValR"
    c_vectorMapValR :: Int -> Ptr Double -> Double :> Double :> IO Int

-- | map of complex vectors with given function
vectorMapValC :: FunCodeSV -> Complex Double -> Vector (Complex Double) -> Vector (Complex Double)
vectorMapValC = vectorMapValAux c_vectorMapValC

foreign import ccall safe "gsl-aux.h mapValC"
    c_vectorMapValC :: Int -> Ptr (Complex Double) -> Complex Double :> Complex Double :> IO Int

-------------------------------------------------------------------

-- | elementwise operation on real vectors
vectorZipR :: FunCodeVV -> Vector Double -> Vector Double -> Vector Double
vectorZipR = vectorZipAux c_vectorZipR

foreign import ccall safe "gsl-aux.h zipR"
    c_vectorZipR :: Int -> Double :> Double :> Double :> IO Int

-- | elementwise operation on complex vectors
vectorZipC :: FunCodeVV -> Vector (Complex Double) -> Vector (Complex Double) -> Vector (Complex Double)
vectorZipC = vectorZipAux c_vectorZipC

foreign import ccall safe "gsl-aux.h zipC"
    c_vectorZipC :: Int -> Complex Double :> Complex Double :> Complex Double :> IO Int


