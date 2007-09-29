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
import Data.List(transpose,intersperse)
import Data.Typeable
import Data.Maybe(fromJust)

----------------------------------------------------------------------
instance (Storable a, RealFloat a) => Storable (Complex a) where    --
    alignment x = alignment (realPart x)                            --
    sizeOf x    = 2 * sizeOf (realPart x)                           --
    peek p = do                                                     --
        [re,im] <- peekArray 2 (castPtr p)                          --
        return (re :+ im)                                           --
    poke p (a :+ b) = pokeArray (castPtr p) [a,b]                   --
----------------------------------------------------------------------

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

-- GSL error codes are <= 1024
-- | error codes for the auxiliary functions required by the wrappers
errorCode :: Int -> String
errorCode 2000 = "bad size"
errorCode 2001 = "bad function code"
errorCode 2002 = "memory problem"
errorCode 2003 = "bad file"
errorCode 2004 = "singular"
errorCode 2005 = "didn't converge"
errorCode 2006 = "the input matrix is not positive definite"
errorCode n    = "code "++show n

{- | conversion of Haskell functions into function pointers that can be used in the C side
-}
foreign import ccall "wrapper" mkfun:: (Double -> Ptr() -> Double) -> IO( FunPtr (Double -> Ptr() -> Double)) 

---------------------------------------------------
-- ugly, but my haddock version doesn't understand
-- yet infix type constructors
---------------------------------------------------
---------- signatures of the C functions -------
------------------------------------------------
type PD = Ptr Double                          --
type PC = Ptr (Complex Double)                --
type TV = Int -> PD -> IO Int                 --
type TVV = Int -> PD -> TV                    --
type TVVV = Int -> PD -> TVV                  --
type TM = Int -> Int -> PD -> IO Int          --
type TMM =  Int -> Int -> PD -> TM            --
type TMMM =  Int -> Int -> PD -> TMM          --
type TVM = Int -> PD -> TM                    --
type TVVM = Int -> PD -> TVM                  --
type TMV = Int -> Int -> PD -> TV             --
type TMVM = Int -> Int -> PD -> TVM           --
type TMMVM = Int -> Int -> PD -> TMVM         --
type TCM = Int -> Int -> PC -> IO Int         --
type TCVCM = Int -> PC -> TCM                 --
type TCMCVCM = Int -> Int -> PC -> TCVCM      --
type TMCMCVCM = Int -> Int -> PD -> TCMCVCM   --
type TCMCMCVCM = Int -> Int -> PC -> TCMCVCM  --
type TCMCM = Int -> Int -> PC -> TCM          --
type TVCM = Int -> PD -> TCM                  --
type TCMVCM = Int -> Int -> PC -> TVCM        --
type TCMCMVCM = Int -> Int -> PC -> TCMVCM    --
type TCMCMCM = Int -> Int -> PC -> TCMCM      --
type TCV = Int -> PC -> IO Int                --
type TCVCV = Int -> PC -> TCV                 --
type TCVCVCV = Int -> PC -> TCVCV             --
type TCMCV = Int -> Int -> PC -> TCV          --
type TVCV = Int -> PD -> TCV                  --
type TCVM = Int -> PC -> TM                   --
type TMCVM = Int -> Int -> PD -> TCVM         --
type TMMCVM = Int -> Int -> PD -> TMCVM       --
------------------------------------------------
