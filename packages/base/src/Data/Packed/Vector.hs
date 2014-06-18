{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE CPP #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Packed.Vector
-- Copyright   :  (c) Alberto Ruiz 2007-10
-- License     :  BSD3
-- Maintainer  :  Alberto Ruiz
-- Stability   :  provisional
--
-- 1D arrays suitable for numeric computations using external libraries.
--
-- This module provides basic functions for manipulation of structure.
--
-----------------------------------------------------------------------------
{-# OPTIONS_HADDOCK hide #-}

module Data.Packed.Vector (
    Vector,
    fromList, (|>), toList, buildVector,
    dim, (@>),
    subVector, takesV, vjoin, join,
    mapVector, mapVectorWithIndex, zipVector, zipVectorWith, unzipVector, unzipVectorWith,
    mapVectorM, mapVectorM_, mapVectorWithIndexM, mapVectorWithIndexM_,
    foldLoop, foldVector, foldVectorG, foldVectorWithIndex,
    toByteString, fromByteString
) where

import Data.Packed.Internal.Vector
import Foreign.Storable

-------------------------------------------------------------------

#ifdef BINARY

import Data.Binary
import Control.Monad(replicateM)

import Data.ByteString.Internal as BS
import Foreign.ForeignPtr(castForeignPtr)
import Data.Vector.Storable.Internal(updPtr)
import Foreign.Ptr(plusPtr)


-- a 64K cache, with a Double taking 13 bytes in Bytestring,
-- implies a chunk size of 5041
chunk :: Int
chunk = 5000

chunks :: Int -> [Int]
chunks d = let c = d `div` chunk
               m = d `mod` chunk
           in if m /= 0 then reverse (m:(replicate c chunk)) else (replicate c chunk)

putVector v = mapM_ put $! toList v

getVector d = do
              xs <- replicateM d get
              return $! fromList xs

--------------------------------------------------------------------------------

toByteString :: Storable t => Vector t -> ByteString
toByteString v = BS.PS (castForeignPtr fp) (sz*o) (sz * dim v)
  where
    (fp,o,_n) = unsafeToForeignPtr v
    sz = sizeOf (v@>0)


fromByteString :: Storable t => ByteString -> Vector t
fromByteString (BS.PS fp o n) = r
  where
    r = unsafeFromForeignPtr (castForeignPtr (updPtr (`plusPtr` o) fp)) 0 n'
    n' = n `div` sz
    sz = sizeOf (r@>0)

--------------------------------------------------------------------------------

instance (Binary a, Storable a) => Binary (Vector a) where

    put v = do
            let d = dim v
            put d
            mapM_ putVector $! takesV (chunks d) v

    -- put = put . v2bs

    get = do
          d <- get
          vs <- mapM getVector $ chunks d
          return $! vjoin vs

    -- get = fmap bs2v get

#endif


-------------------------------------------------------------------

{- | creates a Vector of the specified length using the supplied function to
     to map the index to the value at that index.

@> buildVector 4 fromIntegral
4 |> [0.0,1.0,2.0,3.0]@

-}
buildVector :: Storable a => Int -> (Int -> a) -> Vector a
buildVector len f =
    fromList $ map f [0 .. (len - 1)]


-- | zip for Vectors
zipVector :: (Storable a, Storable b, Storable (a,b)) => Vector a -> Vector b -> Vector (a,b)
zipVector = zipVectorWith (,)

-- | unzip for Vectors
unzipVector :: (Storable a, Storable b, Storable (a,b)) => Vector (a,b) -> (Vector a,Vector b)
unzipVector = unzipVectorWith id

-------------------------------------------------------------------

{-# DEPRECATED join "use vjoin or Data.Vector.concat" #-}
join ::  Storable t => [Vector t] -> Vector t
join = vjoin

