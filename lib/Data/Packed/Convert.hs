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
--
-----------------------------------------------------------------------------

module Data.Packed.Convert (
    vectorToStorableArray,
    storableArrayToVector,
--    unsafeUpdateVector,
) where

import Data.Packed.Internal
import Data.Array.Storable
import Foreign

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


unsafeUpdateVector :: Storable t => Int -- ^ position
                           -> (t->t)    -- ^ modification function
                           -> Vector t  -- ^ source
                           -> IO ()     -- ^ result
unsafeUpdateVector k h v = do
    withForeignPtr (fptr v) $ \s -> pokeElemOff s k (h (v`at'`k))
