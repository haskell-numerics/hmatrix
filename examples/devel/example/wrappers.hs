{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE GADTs #-}

{-
    $ ghc -O2 wrappers.hs functions.c
    $ ./wrappers
-}

import Numeric.LinearAlgebra
import Numeric.LinearAlgebra.Devel
import System.IO.Unsafe(unsafePerformIO)
import Foreign.C.Types(CInt(..))
import Foreign.Ptr(Ptr)


infixl 1 #
a # b = apply a b
{-# INLINE (#) #-}

infixr 5 :>, ::>
type (:>)  t r = CInt -> Ptr t -> r
type (::>) t r =  CInt -> CInt -> CInt -> CInt -> Ptr t -> r
type Ok = IO CInt

-----------------------------------------------------

x = (3><5) [1..]

main = do
    print   x
    print $ myDiag x
    print $ myDiag (tr x)

-----------------------------------------------------
foreign import ccall unsafe "c_diag" cDiag :: Double ::> Double :> Double ::> Ok

myDiag m = unsafePerformIO $ do
    y <- createVector (min r c)
    z <- createMatrix RowMajor r c
    cDiag # m # y # z #| "cDiag"
    return (y,z)
  where
    (r,c) = size m

