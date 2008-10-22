{-# OPTIONS_GHC -fglasgow-exts #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Packed.Internal.Common
-- Copyright   :  (c) Alberto Ruiz 2007
-- License     :  GPL-style
--
-- Maintainer  :  Alberto Ruiz <aruiz@um.es>
-- Stability   :  provisional
-- Portability :  portable (uses FFI)
--
-- Development utilities.
--
-----------------------------------------------------------------------------
-- #hide

module Data.Packed.Internal.Common where

import Foreign
import Complex
import Control.Monad(when)
import Debug.Trace
import Foreign.C.String(peekCString)
import Foreign.C.Types
import Foreign.Storable.Complex()


-- | @debug x = trace (show x) x@
debug :: (Show a) => a -> a
debug x = trace (show x) x

-- | useful for expressions like @sortBy (compare \`on\` length)@
on :: (a -> a -> b) -> (t -> a) -> t -> t -> b
on f g = \x y -> f (g x) (g y)

-- | @partit 3 [1..9] == [[1,2,3],[4,5,6],[7,8,9]]@
partit :: Int -> [a] -> [[a]]
partit _ [] = []
partit n l  = take n l : partit n (drop n l)

-- | obtains the common value of a property of a list
common :: (Eq a) => (b->a) -> [b] -> Maybe a
common f = commonval . map f where
    commonval :: (Eq a) => [a] -> Maybe a
    commonval [] = Nothing
    commonval [a] = Just a
    commonval (a:b:xs) = if a==b then commonval (b:xs) else Nothing

-- | postfix function application (@flip ($)@)
(//) :: x -> (x -> y) -> y
infixl 0 //
(//) = flip ($)

-- | specialized fromIntegral
fi :: Int -> CInt
fi = fromIntegral

-- hmm..
ww2 w1 o1 w2 o2 f = w1 o1 $ \a1 -> w2 o2 $ \a2 -> f a1 a2
ww3 w1 o1 w2 o2 w3 o3 f = w1 o1 $ \a1 -> ww2 w2 o2 w3 o3 (f a1)
ww4 w1 o1 w2 o2 w3 o3 w4 o4 f = w1 o1 $ \a1 -> ww3 w2 o2 w3 o3 w4 o4 (f a1)

app1 f w1 o1 s = w1 o1 $ \a1 -> f // a1 // check s
app2 f w1 o1 w2 o2 s = ww2 w1 o1 w2 o2 $ \a1 a2 -> f // a1 // a2 // check s
app3 f w1 o1 w2 o2 w3 o3 s = ww3 w1 o1 w2 o2 w3 o3 $
     \a1 a2 a3 -> f // a1 // a2 // a3 // check s
app4 f w1 o1 w2 o2 w3 o3 w4 o4 s = ww4 w1 o1 w2 o2 w3 o3 w4 o4 $ 
     \a1 a2 a3 a4 -> f // a1 // a2 // a3 // a4 // check s

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
    err <- f
    when (err/=0) $ if err > 1024
                      then (error (msg++": "++errorCode err)) -- our errors
                      else do                                 -- GSL errors
                        ps <- gsl_strerror err
                        s <- peekCString ps
                        error (msg++": "++s)
    return ()

-- | description of GSL error codes
foreign import ccall "auxi.h gsl_strerror" gsl_strerror :: CInt -> IO (Ptr CChar)

---------------------------------------------------
-- ugly, but my haddock version doesn't understand
-- yet infix type constructors
---------------------------------------------------
---------- signatures of the C functions ---------
--------------------------------------------------
type PD = Ptr Double                            --
type PC = Ptr (Complex Double)                  --
type TV = CInt -> PD -> IO CInt                 --
type TVV = CInt -> PD -> TV                     --
type TVVV = CInt -> PD -> TVV                   --
type TM = CInt -> CInt -> PD -> IO CInt         --
type TMM =  CInt -> CInt -> PD -> TM            --
type TVMM = CInt -> PD -> TMM                   --
type TMVMM = CInt -> CInt -> PD -> TVMM         --
type TMMM =  CInt -> CInt -> PD -> TMM          --
type TVM = CInt -> PD -> TM                     --
type TVVM = CInt -> PD -> TVM                   --
type TMV = CInt -> CInt -> PD -> TV             --
type TMMV = CInt -> CInt -> PD -> TMV           --
type TMVM = CInt -> CInt -> PD -> TVM           --
type TMMVM = CInt -> CInt -> PD -> TMVM         --
type TCM = CInt -> CInt -> PC -> IO CInt        --
type TCVCM = CInt -> PC -> TCM                  --
type TCMCVCM = CInt -> CInt -> PC -> TCVCM      --
type TMCMCVCM = CInt -> CInt -> PD -> TCMCVCM   --
type TCMCMCVCM = CInt -> CInt -> PC -> TCMCVCM  --
type TCMCM = CInt -> CInt -> PC -> TCM          --
type TVCM = CInt -> PD -> TCM                   --
type TCMVCM = CInt -> CInt -> PC -> TVCM        --
type TCMCMVCM = CInt -> CInt -> PC -> TCMVCM    --
type TCMCMCM = CInt -> CInt -> PC -> TCMCM      --
type TCV = CInt -> PC -> IO CInt                --
type TCVCV = CInt -> PC -> TCV                  --
type TCVCVCV = CInt -> PC -> TCVCV              --
type TCMCV = CInt -> CInt -> PC -> TCV          --
type TVCV = CInt -> PD -> TCV                   --
type TCVM = CInt -> PC -> TM                    --
type TMCVM = CInt -> CInt -> PD -> TCVM         --
type TMMCVM = CInt -> CInt -> PD -> TMCVM       --
--------------------------------------------------

type TauxMul a = CInt -> CInt -> CInt -> Ptr a
               -> CInt -> CInt -> CInt -> Ptr a
               -> CInt -> CInt -> Ptr a
               -> IO CInt
