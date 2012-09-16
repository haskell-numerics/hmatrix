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

import Data.Packed
import Data.Packed.Development(createVector,createMatrix,vec,mat,MatrixOrder(..),(//))
import Data.Vector.Storable(unsafeWith)

import Foreign.Marshal.Array(copyArray)
import Foreign.Ptr(Ptr, FunPtr)
import Foreign.C.Types
import System.IO.Unsafe(unsafePerformIO)
import Foreign.Storable(Storable)
import Data.Complex
import Control.Monad(when)
import Foreign.C.String

type PD = Ptr Double
type PC = Ptr (Complex Double) 
type TV = CInt -> PD -> IO CInt
type TVV = CInt -> PD -> TV
type TM = CInt -> CInt -> PD -> IO CInt
type TVM = CInt -> PD -> TM
type TVVM = CInt -> PD -> TVM
type TVCV = CInt -> PD -> TCV
type TCV = CInt -> PC -> IO CInt
type TCVCV = CInt -> PC -> TCV


type Adapt f t r = t -> ((f -> r) -> IO()) -> IO()

type Adapt1 f t1 = Adapt f t1 (IO CInt) -> t1 -> String -> IO()
type Adapt2 f t1 r1 t2 = Adapt f t1 r1 -> t1 -> Adapt1 r1 t2

app1 :: f -> Adapt1 f t1
app2 :: f -> Adapt2 f t1 r1 t2

app1 f w1 o1 s = w1 o1 $ \a1 -> f // a1 // check s
app2 f w1 o1 w2 o2 s = ww2 w1 o1 w2 o2 $ \a1 a2 -> f // a1 // a2 // check s
  
ww2 w1 o1 w2 o2 f = w1 o1 $ w2 o2 . f

-- GSL error codes are <= 1024
-- | error codes for the auxiliary functions required by the wrappers
errorCode :: CInt -> String
errorCode 2000 = "bad size"
errorCode 2001 = "bad function code"
errorCode 2002 = "memory problem"
errorCode 2003 = "bad file"
errorCode 2004 = "singular"
errorCode 2005 = "didn't converge"
errorCode 2006 = "the input matrix is not positive definite"
errorCode 2007 = "not yet supported in this OS"
errorCode n    = "code "++show n



-- | check the error code
check :: String -> IO CInt -> IO ()
check msg f = do
#if FINIT
    finit
#endif
    err <- f
    when (err/=0) $ if err > 1024
                      then (error (msg++": "++errorCode err)) -- our errors
                      else do                                 -- GSL errors
                        ps <- gsl_strerror err
                        s <- peekCString ps
                        error (msg++": "++s)
    return ()

-- | description of GSL error codes
foreign import ccall unsafe "gsl_strerror" gsl_strerror :: CInt -> IO (Ptr CChar)



fi :: Int -> CInt
fi = fromIntegral

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

createV :: Storable t
        => Int -> (CInt -> Ptr t -> IO CInt) -> String -> Vector t
createV n fun msg = unsafePerformIO $ do
    r <- createVector n
    app1 fun vec r msg
    return r

createMIO :: Storable t
          => Int -> Int
          -> (CInt -> CInt -> Ptr t -> IO CInt)
          -> String
          -> IO (Matrix t)
createMIO r c fun msg = do
    res <- createMatrix RowMajor r c
    app1 fun mat res msg
    return res

