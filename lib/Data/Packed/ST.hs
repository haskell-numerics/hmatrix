{-# OPTIONS -XTypeOperators -XRank2Types  -XFlexibleContexts #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Packed.ST
-- Copyright   :  (c) Alberto Ruiz 2008
-- License     :  GPL-style
--
-- Maintainer  :  Alberto Ruiz <aruiz@um.es>
-- Stability   :  provisional
-- Portability :  portable
--
-- In-place manipulation inside the ST monad.
-- See examples/inplace.hs in the distribution.
--
-----------------------------------------------------------------------------

module Data.Packed.ST (
    STVector, thawVector, freezeVector, runSTVector,
    readVector, writeVector, modifyVector, liftSTVector,
    STMatrix, thawMatrix, freezeMatrix, runSTMatrix,
    readMatrix, writeMatrix, modifyMatrix, liftSTMatrix
) where

import Data.Packed.Internal
import Data.Array.Storable
import Control.Monad.ST
import Data.Array.ST
import Foreign


ioReadV :: Storable t => Vector t -> Int -> IO t
ioReadV v k = withForeignPtr (fptr v) $ \s -> peekElemOff s k

ioWriteV :: Storable t => Vector t -> Int -> t -> IO ()
ioWriteV v k x = withForeignPtr (fptr v) $ \s -> pokeElemOff s k x

newtype STVector s t = Mut (Vector t)

thawVector :: Storable t => Vector t -> ST s (STVector s t)
thawVector = unsafeIOToST . fmap Mut . cloneVector

unsafeFreezeVector (Mut x) = unsafeIOToST . return $ x

runSTVector :: Storable t => (forall s . ST s (STVector s t)) -> Vector t
runSTVector st = runST (st >>= unsafeFreezeVector)

readVector :: Storable t => STVector s t -> Int -> ST s t
readVector   (Mut x) = unsafeIOToST . ioReadV x

writeVector :: Storable t => STVector s t -> Int -> t -> ST s ()
writeVector  (Mut x) k = unsafeIOToST . ioWriteV x k

modifyVector :: (Storable t) => STVector s t -> Int -> (t -> t) -> ST s ()
modifyVector x k f = readVector x k >>= return . f >>= writeVector x k

liftSTVector :: (Storable t) => (Vector t -> a) -> STVector s1 t -> ST s2 a
liftSTVector f (Mut x) = unsafeIOToST . fmap f . cloneVector $ x

freezeVector :: (Storable t) => STVector s1 t -> ST s2 (Vector t)
freezeVector v = liftSTVector id v

-------------------------------------------------------------------------

ioReadM :: Storable t => Matrix t -> Int -> Int -> IO t
ioReadM (MC nr nc cv) r c = ioReadV cv (r*nc+c)
ioReadM (MF nr nc fv) r c = ioReadV fv (c*nr+r)

ioWriteM :: Storable t => Matrix t -> Int -> Int -> t -> IO ()
ioWriteM (MC nr nc cv) r c val = ioWriteV cv (r*nc+c) val
ioWriteM (MF nr nc fv) r c val = ioWriteV fv (c*nr+r) val

newtype STMatrix s t = STMatrix (Matrix t)

thawMatrix :: Storable t => Matrix t -> ST s (STMatrix s t)
thawMatrix = unsafeIOToST . fmap STMatrix . cloneMatrix

unsafeFreezeMatrix (STMatrix x) = unsafeIOToST . return $ x

runSTMatrix :: Storable t => (forall s . ST s (STMatrix s t)) -> Matrix t
runSTMatrix st = runST (st >>= unsafeFreezeMatrix)

readMatrix :: Storable t => STMatrix s t -> Int -> Int -> ST s t
readMatrix   (STMatrix x) r = unsafeIOToST . ioReadM x r

writeMatrix :: Storable t => STMatrix s t -> Int -> Int -> t -> ST s ()
writeMatrix  (STMatrix x) r c = unsafeIOToST . ioWriteM x r c

modifyMatrix :: (Storable t) => STMatrix s t -> Int -> Int -> (t -> t) -> ST s ()
modifyMatrix x r c f = readMatrix x r c >>= return . f >>= writeMatrix x r c

liftSTMatrix :: (Storable t) => (Matrix t -> a) -> STMatrix s1 t -> ST s2 a
liftSTMatrix f (STMatrix x) = unsafeIOToST . fmap f . cloneMatrix $ x

freezeMatrix :: (Storable t) => STMatrix s1 t -> ST s2 (Matrix t)
freezeMatrix m = liftSTMatrix id m

cloneMatrix (MC r c d) = cloneVector d >>= return . MC r c
cloneMatrix (MF r c d) = cloneVector d >>= return . MF r c
