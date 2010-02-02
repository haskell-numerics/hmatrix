{-# LANGUAGE CPP #-}
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

module Data.Packed.Internal.Common(
  Adapt,
  app1, app2, app3, app4,
  (//), check,
  splitEvery, common, compatdim,
  fi,
  table
) where

import Foreign
import Control.Monad(when)
import Foreign.C.String(peekCString)
import Foreign.C.Types
import Foreign.Storable.Complex()
import Data.List(transpose,intersperse)

-- | @splitEvery 3 [1..9] == [[1,2,3],[4,5,6],[7,8,9]]@
splitEvery :: Int -> [a] -> [[a]]
splitEvery _ [] = []
splitEvery k l = take k l : splitEvery k (drop k l)

-- | obtains the common value of a property of a list
common :: (Eq a) => (b->a) -> [b] -> Maybe a
common f = commonval . map f where
    commonval :: (Eq a) => [a] -> Maybe a
    commonval [] = Nothing
    commonval [a] = Just a
    commonval (a:b:xs) = if a==b then commonval (b:xs) else Nothing

-- | common value with \"adaptable\" 1
compatdim :: [Int] -> Maybe Int
compatdim [] = Nothing
compatdim [a] = Just a
compatdim (a:b:xs) = if a==b || a==1 || b==1 then compatdim (max a b:xs) else Nothing

-- | Formatting tool
table :: String -> [[String]] -> String
table sep as = unlines . map unwords' $ transpose mtp where 
    mt = transpose as
    longs = map (maximum . map length) mt
    mtp = zipWith (\a b -> map (pad a) b) longs mt
    pad n str = replicate (n - length str) ' ' ++ str
    unwords' = concat . intersperse sep

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

type Adapt f t r = t -> ((f -> r) -> IO()) -> IO()

app1 :: f
     -> Adapt f t (IO CInt)
     -> t
     -> String
     -> IO()

app2 :: f
     -> Adapt f t1 r
     -> t1
     -> Adapt r t2 (IO CInt)
     -> t2
     -> String
     -> IO()

app3 :: f
     -> Adapt f t1 r1
     -> t1
     -> Adapt r1 t2 r2
     -> t2
     -> Adapt r2 t3 (IO CInt)
     -> t3
     -> String
     -> IO()

app4 :: f
     -> Adapt f t1 r1
     -> t1
     -> Adapt r1 t2 r2
     -> t2
     -> Adapt r2 t3 r3
     -> t3
     -> Adapt r3 t4 (IO CInt)
     -> t4
     -> String
     -> IO()

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


-- | clear the fpu
foreign import ccall "asm_finit" finit :: IO ()

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
foreign import ccall "auxi.h gsl_strerror" gsl_strerror :: CInt -> IO (Ptr CChar)
