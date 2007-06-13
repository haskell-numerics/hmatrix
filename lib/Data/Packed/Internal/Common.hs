{-# OPTIONS_GHC -fglasgow-exts -fallow-undecidable-instances #-}
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
-- Common tools
--
-----------------------------------------------------------------------------

module Data.Packed.Internal.Common where

import Foreign
import Complex
import Control.Monad(when)
import Debug.Trace
import Data.List(transpose,intersperse)
import Data.Typeable
import Data.Maybe(fromJust)

debug x = trace (show x) x

data Vector t = V { dim  :: Int
                  , fptr :: ForeignPtr t
                  , ptr  :: Ptr t
                  } deriving Typeable

----------------------------------------------------------------------
instance (Storable a, RealFloat a) => Storable (Complex a) where    --
    alignment x = alignment (realPart x)                            --
    sizeOf x    = 2 * sizeOf (realPart x)                           --
    peek p = do                                                     --
        [re,im] <- peekArray 2 (castPtr p)                          --
        return (re :+ im)                                           --
    poke p (a :+ b) = pokeArray (castPtr p) [a,b]                   --
----------------------------------------------------------------------

on f g = \x y -> f (g x) (g y)

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

xor a b = a && not b || b && not a

(//) :: x -> (x -> y) -> y
infixl 0 //
(//) = flip ($)

errorCode 1000 = "bad size"
errorCode 1001 = "bad function code"
errorCode 1002 = "memory problem"
errorCode 1003 = "bad file"
errorCode 1004 = "singular"
errorCode 1005 = "didn't converge"
errorCode n    = "code "++show n

check msg ls f = do
    err <- f
    when (err/=0) (error (msg++": "++errorCode err))
    mapM_ (touchForeignPtr . fptr) ls
    return ()

class (Storable a, Typeable a) => Field a where
instance (Storable a, Typeable a) => Field a where

isReal w x   = typeOf (undefined :: Double) == typeOf (w x)
isComp w x = typeOf (undefined :: Complex Double) == typeOf (w x)

scast :: forall a . forall b . (Typeable a, Typeable b) => a -> b
scast = fromJust . cast
