{-# OPTIONS -XTypeOperators -XRank2Types  -XFlexibleContexts #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Packed.Convert
-- Copyright   :  (c) Alberto Ruiz 2008
-- License     :  GPL-style
--
-- Maintainer  :  Alberto Ruiz <aruiz@um.es>
-- Stability   :  provisional
-- Portability :  portable
--
-- Conversion of Vectors and Matrices to and from the standard Haskell arrays.
-- (provisional)
--
-----------------------------------------------------------------------------

module Data.Packed.Convert (
    arrayFromVector, vectorFromArray,
    mArrayFromVector, vectorFromMArray,
    vectorToStorableArray, storableArrayToVector,
    arrayFromMatrix, matrixFromArray,
    mArrayFromMatrix, matrixFromMArray,
--    matrixToStorableArray, storableArrayToMatrix
) where

import Data.Packed.Internal
import Data.Array.Storable
import Foreign
import Control.Monad.ST
import Data.Array.ST
import Data.Array.Unboxed

-- | Creates a StorableArray indexed from 0 to dim -1.
--   (Memory is efficiently copied, so you can then freely modify the obtained array)
vectorToStorableArray :: Storable t => Vector t -> IO (StorableArray Int t)
vectorToStorableArray v = do
    r <- cloneVector v
    unsafeForeignPtrToStorableArray (fptr r) (0,dim r -1)

-- | Creates a Vector from a StorableArray.
--   (Memory is efficiently copied, so posterior changes in the array will not affect the result)
storableArrayToVector :: Storable t => StorableArray Int t -> IO (Vector t)
storableArrayToVector s = do
    (a,b) <- getBounds s
    let n = (b-a+1)
    r <- createVector n
    withStorableArray s $ \p -> do
        let f _ d =  copyArray d p n >> return 0
        app1 f vec r "storableArrayToVector"
    return r


unsafeVectorToStorableArray :: Storable t => Vector t -> IO (StorableArray Int t)
unsafeVectorToStorableArray v = unsafeForeignPtrToStorableArray (fptr v) (0,dim v -1)

--unsafeStorableArrayToVector :: Storable t => StorableArray Int t -> IO (Vector t)
--unsafeStorableArrayToVector s = undefined -- the foreign ptr of Storable Array is not available?

-----------------------------------------------------------------
-- provisional, we need Unboxed arrays for Complex Double


unsafeFreeze' :: (MArray a e m, Ix i) => a i e -> m (Array i e)
unsafeFreeze' = unsafeFreeze

-- | creates an immutable Array from an hmatrix Vector (to do: unboxed)
arrayFromVector :: (Storable t) => Vector t -> Array Int t
arrayFromVector x = runSTArray (mArrayFromVector x)

-- | creates a mutable array from an hmatrix Vector (to do: unboxed)
mArrayFromVector :: (MArray b t (ST s), Storable t) => Vector t -> ST s (b Int t)
mArrayFromVector v = unsafeThaw =<< unsafeIOToST ( unsafeFreeze' =<< (vectorToStorableArray $ v))


-- (creates an hmatrix Vector from an immutable array (to do: unboxed))
vectorFromArray :: (Storable t) => Array Int t -> Vector t
vectorFromArray a = unsafePerformIO $ storableArrayToVector =<< unsafeThaw a

-- | creates a mutable Array from an hmatrix Vector for manipulation with runSTUArray (to do: unboxed)
vectorFromMArray :: (Storable t, MArray a t (ST s)) => a Int t -> ST s (Vector t)
vectorFromMArray x = fmap vectorFromArray (unsafeFreeze' x)

--------------------------------------------------------------------
-- provisional

matrixFromArray :: UArray (Int, Int) Double -> Matrix Double
matrixFromArray m = reshape c . fromList . elems $ m
    where ((r1,c1),(r2,c2)) = bounds m
          _r = r2-r1+1
          c = c2-c1+1

arrayFromMatrix :: Matrix Double -> UArray (Int, Int) Double
arrayFromMatrix m = listArray ((0,0),(rows m -1, cols m -1)) (toList $ flatten m)


mArrayFromMatrix :: (MArray b Double m) => Matrix Double -> m (b (Int, Int) Double)
mArrayFromMatrix = unsafeThaw . arrayFromMatrix

matrixFromMArray :: (MArray a Double (ST s)) => a (Int,Int) Double -> ST s (Matrix Double)
matrixFromMArray x = fmap matrixFromArray (unsafeFreeze x)
