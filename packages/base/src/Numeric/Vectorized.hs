-----------------------------------------------------------------------------
-- |
-- Module      :  Numeric.Vectorized
-- Copyright   :  (c) Alberto Ruiz 2007-14
-- License     :  BSD3
-- Maintainer  :  Alberto Ruiz
-- Stability   :  provisional
--
-- Low level interface to vector operations.
--
-----------------------------------------------------------------------------

module Numeric.Vectorized (
    sumF, sumR, sumQ, sumC,
    prodF, prodR, prodQ, prodC,
    FunCodeS(..), toScalarR, toScalarF, toScalarC, toScalarQ,
    FunCodeV(..), vectorMapR, vectorMapC, vectorMapF, vectorMapQ,
    FunCodeSV(..), vectorMapValR, vectorMapValC, vectorMapValF, vectorMapValQ,
    FunCodeVV(..), vectorZipR, vectorZipC, vectorZipF, vectorZipQ,
    vectorScan, saveMatrix
) where

import Data.Packed.Internal.Common
import Data.Packed.Internal.Signatures
import Data.Packed.Internal.Vector
import Data.Packed.Internal.Matrix

import Data.Complex
import Foreign.Marshal.Alloc(free,malloc)
import Foreign.Marshal.Array(newArray,copyArray)
import Foreign.Ptr(Ptr)
import Foreign.Storable(peek)
import Foreign.C.Types
import Foreign.C.String
import System.IO.Unsafe(unsafePerformIO)

import Control.Monad(when)
import Control.Applicative((<$>))



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

-- | sum of elements
sumF :: Vector Float -> Float
sumF x = unsafePerformIO $ do
           r <- createVector 1
           app2 c_sumF vec x vec r "sumF"
           return $ r @> 0

-- | sum of elements
sumR :: Vector Double -> Double
sumR x = unsafePerformIO $ do
           r <- createVector 1
           app2 c_sumR vec x vec r "sumR"
           return $ r @> 0

-- | sum of elements
sumQ :: Vector (Complex Float) -> Complex Float
sumQ x = unsafePerformIO $ do
           r <- createVector 1
           app2 c_sumQ vec x vec r "sumQ"
           return $ r @> 0

-- | sum of elements
sumC :: Vector (Complex Double) -> Complex Double
sumC x = unsafePerformIO $ do
           r <- createVector 1
           app2 c_sumC vec x vec r "sumC"
           return $ r @> 0

foreign import ccall unsafe "sumF" c_sumF :: TFF
foreign import ccall unsafe "sumR" c_sumR :: TVV
foreign import ccall unsafe "sumQ" c_sumQ :: TQVQV
foreign import ccall unsafe "sumC" c_sumC :: TCVCV

-- | product of elements
prodF :: Vector Float -> Float
prodF x = unsafePerformIO $ do
           r <- createVector 1
           app2 c_prodF vec x vec r "prodF"
           return $ r @> 0

-- | product of elements
prodR :: Vector Double -> Double
prodR x = unsafePerformIO $ do
           r <- createVector 1
           app2 c_prodR vec x vec r "prodR"
           return $ r @> 0

-- | product of elements
prodQ :: Vector (Complex Float) -> Complex Float
prodQ x = unsafePerformIO $ do
           r <- createVector 1
           app2 c_prodQ vec x vec r "prodQ"
           return $ r @> 0

-- | product of elements
prodC :: Vector (Complex Double) -> Complex Double
prodC x = unsafePerformIO $ do
           r <- createVector 1
           app2 c_prodC vec x vec r "prodC"
           return $ r @> 0

foreign import ccall unsafe "prodF" c_prodF :: TFF
foreign import ccall unsafe "prodR" c_prodR :: TVV
foreign import ccall unsafe "prodQ" c_prodQ :: TQVQV
foreign import ccall unsafe "prodC" c_prodC :: TCVCV

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

foreign import ccall unsafe "toScalarR" c_toScalarR :: CInt -> TVV

-- | obtains different functions of a vector: norm1, norm2, max, min, posmax, posmin, etc.
toScalarF :: FunCodeS -> Vector Float -> Float
toScalarF oper =  toScalarAux c_toScalarF (fromei oper)

foreign import ccall unsafe "toScalarF" c_toScalarF :: CInt -> TFF

-- | obtains different functions of a vector: only norm1, norm2
toScalarC :: FunCodeS -> Vector (Complex Double) -> Double
toScalarC oper =  toScalarAux c_toScalarC (fromei oper)

foreign import ccall unsafe "toScalarC" c_toScalarC :: CInt -> TCVV

-- | obtains different functions of a vector: only norm1, norm2
toScalarQ :: FunCodeS -> Vector (Complex Float) -> Float
toScalarQ oper =  toScalarAux c_toScalarQ (fromei oper)

foreign import ccall unsafe "toScalarQ" c_toScalarQ :: CInt -> TQVF

------------------------------------------------------------------

-- | map of real vectors with given function
vectorMapR :: FunCodeV -> Vector Double -> Vector Double
vectorMapR = vectorMapAux c_vectorMapR

foreign import ccall unsafe "mapR" c_vectorMapR :: CInt -> TVV

-- | map of complex vectors with given function
vectorMapC :: FunCodeV -> Vector (Complex Double) -> Vector (Complex Double)
vectorMapC oper = vectorMapAux c_vectorMapC (fromei oper)

foreign import ccall unsafe "mapC" c_vectorMapC :: CInt -> TCVCV

-- | map of real vectors with given function
vectorMapF :: FunCodeV -> Vector Float -> Vector Float
vectorMapF = vectorMapAux c_vectorMapF

foreign import ccall unsafe "mapF" c_vectorMapF :: CInt -> TFF

-- | map of real vectors with given function
vectorMapQ :: FunCodeV -> Vector (Complex Float) -> Vector (Complex Float)
vectorMapQ = vectorMapAux c_vectorMapQ

foreign import ccall unsafe "mapQ" c_vectorMapQ :: CInt -> TQVQV

-------------------------------------------------------------------

-- | map of real vectors with given function
vectorMapValR :: FunCodeSV -> Double -> Vector Double -> Vector Double
vectorMapValR oper = vectorMapValAux c_vectorMapValR (fromei oper)

foreign import ccall unsafe "mapValR" c_vectorMapValR :: CInt -> Ptr Double -> TVV

-- | map of complex vectors with given function
vectorMapValC :: FunCodeSV -> Complex Double -> Vector (Complex Double) -> Vector (Complex Double)
vectorMapValC = vectorMapValAux c_vectorMapValC

foreign import ccall unsafe "mapValC" c_vectorMapValC :: CInt -> Ptr (Complex Double) -> TCVCV

-- | map of real vectors with given function
vectorMapValF :: FunCodeSV -> Float -> Vector Float -> Vector Float
vectorMapValF oper = vectorMapValAux c_vectorMapValF (fromei oper)

foreign import ccall unsafe "mapValF" c_vectorMapValF :: CInt -> Ptr Float -> TFF

-- | map of complex vectors with given function
vectorMapValQ :: FunCodeSV -> Complex Float -> Vector (Complex Float) -> Vector (Complex Float)
vectorMapValQ oper = vectorMapValAux c_vectorMapValQ (fromei oper)

foreign import ccall unsafe "mapValQ" c_vectorMapValQ :: CInt -> Ptr (Complex Float) -> TQVQV

-------------------------------------------------------------------

-- | elementwise operation on real vectors
vectorZipR :: FunCodeVV -> Vector Double -> Vector Double -> Vector Double
vectorZipR = vectorZipAux c_vectorZipR

foreign import ccall unsafe "zipR" c_vectorZipR :: CInt -> TVVV

-- | elementwise operation on complex vectors
vectorZipC :: FunCodeVV -> Vector (Complex Double) -> Vector (Complex Double) -> Vector (Complex Double)
vectorZipC = vectorZipAux c_vectorZipC

foreign import ccall unsafe "zipC" c_vectorZipC :: CInt -> TCVCVCV

-- | elementwise operation on real vectors
vectorZipF :: FunCodeVV -> Vector Float -> Vector Float -> Vector Float
vectorZipF = vectorZipAux c_vectorZipF

foreign import ccall unsafe "zipF" c_vectorZipF :: CInt -> TFFF

-- | elementwise operation on complex vectors
vectorZipQ :: FunCodeVV -> Vector (Complex Float) -> Vector (Complex Float) -> Vector (Complex Float)
vectorZipQ = vectorZipAux c_vectorZipQ

foreign import ccall unsafe "zipQ" c_vectorZipQ :: CInt -> TQVQVQV

--------------------------------------------------------------------------------

foreign import ccall unsafe "vectorScan" c_vectorScan
    :: CString -> Ptr CInt -> Ptr (Ptr Double) -> IO CInt

vectorScan :: FilePath -> IO (Vector Double)
vectorScan s = do
    pp <- malloc
    pn <- malloc
    cs <- newCString s
    ok <- c_vectorScan cs pn pp
    when (not (ok == 0)) $
        error ("vectorScan \"" ++ s ++"\"")
    n <- fromIntegral <$> peek pn
    p <- peek pp
    v <- createVector n
    free pn
    free cs
    unsafeWith v $ \pv -> copyArray pv p n
    free p
    free pp
    return v

--------------------------------------------------------------------------------

foreign import ccall unsafe "saveMatrix" c_saveMatrix
    :: CString -> CString -> TM

saveMatrix :: FilePath -> String -> Matrix Double -> IO ()
saveMatrix name format m = do
    cname   <- newCString name
    cformat <- newCString format
    app1 (c_saveMatrix cname cformat) mat m "saveMatrix"
    free cname
    free cformat
    return ()


