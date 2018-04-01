{-# LANGUAGE Rank2Types    #-}
{-# LANGUAGE BangPatterns  #-}
{-# LANGUAGE ViewPatterns #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Internal.ST
-- Copyright   :  (c) Alberto Ruiz 2008
-- License     :  BSD3
-- Maintainer  :  Alberto Ruiz
-- Stability   :  provisional
--
-- In-place manipulation inside the ST monad.
-- See @examples/inplace.hs@ in the repository.
--
-----------------------------------------------------------------------------

module Internal.ST (
    ST, runST,
    -- * Mutable Vectors
    STVector, newVector, thawVector, freezeVector, runSTVector,
    readVector, writeVector, modifyVector, liftSTVector,
    -- * Mutable Matrices
    STMatrix, newMatrix, thawMatrix, freezeMatrix, runSTMatrix,
    readMatrix, writeMatrix, modifyMatrix, liftSTMatrix,
    mutable, extractMatrix, setMatrix, rowOper, RowOper(..), RowRange(..), ColRange(..), gemmm, Slice(..),
    -- * Unsafe functions
    newUndefinedVector,
    unsafeReadVector, unsafeWriteVector,
    unsafeThawVector, unsafeFreezeVector,
    newUndefinedMatrix,
    unsafeReadMatrix, unsafeWriteMatrix,
    unsafeThawMatrix, unsafeFreezeMatrix
) where

import Internal.Vector
import Internal.Matrix
import Internal.Vectorized
import Control.Monad.ST(ST, runST)
import Foreign.Storable(Storable, peekElemOff, pokeElemOff)
import Control.Monad.ST.Unsafe(unsafeIOToST)

{-# INLINE ioReadV #-}
ioReadV :: Storable t => Vector t -> Int -> IO t
ioReadV v k = unsafeWith v $ \s -> peekElemOff s k

{-# INLINE ioWriteV #-}
ioWriteV :: Storable t => Vector t -> Int -> t -> IO ()
ioWriteV v k x = unsafeWith v $ \s -> pokeElemOff s k x

newtype STVector s t = STVector (Vector t)

thawVector :: Storable t => Vector t -> ST s (STVector s t)
thawVector = unsafeIOToST . fmap STVector . cloneVector

unsafeThawVector :: Storable t => Vector t -> ST s (STVector s t)
unsafeThawVector = unsafeIOToST . return . STVector

runSTVector :: Storable t => (forall s . ST s (STVector s t)) -> Vector t
runSTVector st = runST (st >>= unsafeFreezeVector)

{-# INLINE unsafeReadVector #-}
unsafeReadVector :: Storable t => STVector s t -> Int -> ST s t
unsafeReadVector   (STVector x) = unsafeIOToST . ioReadV x

{-# INLINE unsafeWriteVector #-}
unsafeWriteVector :: Storable t => STVector s t -> Int -> t -> ST s ()
unsafeWriteVector  (STVector x) k = unsafeIOToST . ioWriteV x k

{-# INLINE modifyVector #-}
modifyVector :: (Storable t) => STVector s t -> Int -> (t -> t) -> ST s ()
modifyVector x k f = readVector x k >>= return . f >>= unsafeWriteVector x k

liftSTVector :: (Storable t) => (Vector t -> a) -> STVector s t -> ST s a
liftSTVector f (STVector x) = unsafeIOToST . fmap f . cloneVector $ x

freezeVector :: (Storable t) => STVector s t -> ST s (Vector t)
freezeVector v = liftSTVector id v

unsafeFreezeVector :: (Storable t) => STVector s t -> ST s (Vector t)
unsafeFreezeVector (STVector x) = unsafeIOToST . return $ x

{-# INLINE safeIndexV #-}
safeIndexV :: Storable t2
           => (STVector s t2 -> Int -> t) -> STVector t1 t2 -> Int -> t
safeIndexV f (STVector v) k
    | k < 0 || k>= dim v = error $ "out of range error in vector (dim="
                                   ++show (dim v)++", pos="++show k++")"
    | otherwise = f (STVector v) k

{-# INLINE readVector #-}
readVector :: Storable t => STVector s t -> Int -> ST s t
readVector = safeIndexV unsafeReadVector

{-# INLINE writeVector #-}
writeVector :: Storable t => STVector s t -> Int -> t -> ST s ()
writeVector = safeIndexV unsafeWriteVector

newUndefinedVector :: Storable t => Int -> ST s (STVector s t)
newUndefinedVector = unsafeIOToST . fmap STVector . createVector

{-# INLINE newVector #-}
newVector :: Storable t => t -> Int -> ST s (STVector s t)
newVector x n = do
    v <- newUndefinedVector n
    let go (-1) = return v
        go !k = unsafeWriteVector v k x >> go (k-1 :: Int)
    go (n-1)

-------------------------------------------------------------------------

{-# INLINE ioReadM #-}
ioReadM :: Storable t => Matrix t -> Int -> Int -> IO t
ioReadM m r c = ioReadV (xdat m) (r * xRow m + c * xCol m)


{-# INLINE ioWriteM #-}
ioWriteM :: Storable t => Matrix t -> Int -> Int -> t -> IO ()
ioWriteM m r c val = ioWriteV (xdat m)  (r * xRow m + c * xCol m) val


newtype STMatrix s t = STMatrix (Matrix t)

thawMatrix :: Element t => Matrix t -> ST s (STMatrix s t)
thawMatrix = unsafeIOToST . fmap STMatrix . cloneMatrix

unsafeThawMatrix :: Storable t => Matrix t -> ST s (STMatrix s t)
unsafeThawMatrix = unsafeIOToST . return . STMatrix

runSTMatrix :: Storable t => (forall s . ST s (STMatrix s t)) -> Matrix t
runSTMatrix st = runST (st >>= unsafeFreezeMatrix)

{-# INLINE unsafeReadMatrix #-}
unsafeReadMatrix :: Storable t => STMatrix s t -> Int -> Int -> ST s t
unsafeReadMatrix   (STMatrix x) r = unsafeIOToST . ioReadM x r

{-# INLINE unsafeWriteMatrix #-}
unsafeWriteMatrix :: Storable t => STMatrix s t -> Int -> Int -> t -> ST s ()
unsafeWriteMatrix  (STMatrix x) r c = unsafeIOToST . ioWriteM x r c

{-# INLINE modifyMatrix #-}
modifyMatrix :: (Storable t) => STMatrix s t -> Int -> Int -> (t -> t) -> ST s ()
modifyMatrix x r c f = readMatrix x r c >>= return . f >>= unsafeWriteMatrix x r c

liftSTMatrix :: (Element t) => (Matrix t -> a) -> STMatrix s t -> ST s a
liftSTMatrix f (STMatrix x) = unsafeIOToST . fmap f . cloneMatrix $ x

unsafeFreezeMatrix :: (Storable t) => STMatrix s t -> ST s (Matrix t)
unsafeFreezeMatrix (STMatrix x) = unsafeIOToST . return $ x


freezeMatrix :: (Element t) => STMatrix s t -> ST s (Matrix t)
freezeMatrix m = liftSTMatrix id m

cloneMatrix :: Element t => Matrix t -> IO (Matrix t)
cloneMatrix m = copy (orderOf m) m

{-# INLINE safeIndexM #-}
safeIndexM :: (STMatrix s t2 -> Int -> Int -> t)
           -> STMatrix t1 t2 -> Int -> Int -> t
safeIndexM f (STMatrix m) r c
    | r<0 || r>=rows m ||
      c<0 || c>=cols m = error $ "out of range error in matrix (size="
                                 ++show (rows m,cols m)++", pos="++show (r,c)++")"
    | otherwise = f (STMatrix m) r c

{-# INLINE readMatrix #-}
readMatrix :: Storable t => STMatrix s t -> Int -> Int -> ST s t
readMatrix = safeIndexM unsafeReadMatrix

{-# INLINE writeMatrix #-}
writeMatrix :: Storable t => STMatrix s t -> Int -> Int -> t -> ST s ()
writeMatrix = safeIndexM unsafeWriteMatrix

setMatrix :: Element t => STMatrix s t -> Int -> Int -> Matrix t -> ST s ()
setMatrix (STMatrix x) i j m = unsafeIOToST $ setRect i j m x

newUndefinedMatrix :: Storable t => MatrixOrder -> Int -> Int -> ST s (STMatrix s t)
newUndefinedMatrix ord r c = unsafeIOToST $ fmap STMatrix $ createMatrix ord r c

{-# NOINLINE newMatrix #-}
newMatrix :: Storable t => t -> Int -> Int -> ST s (STMatrix s t)
newMatrix v r c = unsafeThawMatrix $ reshape c $ runSTVector $ newVector v (r*c)

--------------------------------------------------------------------------------

data ColRange = AllCols
              | ColRange Int Int
              | Col Int
              | FromCol Int

getColRange :: Int -> ColRange -> (Int, Int)
getColRange c AllCols = (0,c-1)
getColRange c (ColRange a b) = (a `mod` c, b `mod` c)
getColRange c (Col a) = (a `mod` c, a `mod` c)
getColRange c (FromCol a) = (a `mod` c, c-1)

data RowRange = AllRows
              | RowRange Int Int
              | Row Int
              | FromRow Int

getRowRange :: Int -> RowRange -> (Int, Int)
getRowRange r AllRows = (0,r-1)
getRowRange r (RowRange a b) = (a `mod` r, b `mod` r)
getRowRange r (Row a) = (a `mod` r, a `mod` r)
getRowRange r (FromRow a) = (a `mod` r, r-1)

data RowOper t = AXPY t Int Int  ColRange
               | SCAL t RowRange ColRange
               | SWAP Int Int    ColRange

rowOper :: (Num t, Element t) => RowOper t -> STMatrix s t -> ST s ()

rowOper (AXPY x i1 i2 r) (STMatrix m) = unsafeIOToST $ rowOp 0 x i1' i2' j1 j2 m
  where
    (j1,j2) = getColRange (cols m) r
    i1' = i1 `mod` (rows m)
    i2' = i2 `mod` (rows m)

rowOper (SCAL x rr rc) (STMatrix m) = unsafeIOToST $ rowOp 1 x i1 i2 j1 j2 m
  where
    (i1,i2) = getRowRange (rows m) rr
    (j1,j2) = getColRange (cols m) rc

rowOper (SWAP i1 i2 r) (STMatrix m) = unsafeIOToST $ rowOp 2 0 i1' i2' j1 j2 m
  where
    (j1,j2) = getColRange (cols m) r
    i1' = i1 `mod` (rows m)
    i2' = i2 `mod` (rows m)


extractMatrix :: Element a => STMatrix t a -> RowRange -> ColRange -> ST s (Matrix a)
extractMatrix (STMatrix m) rr rc = unsafeIOToST (extractR (orderOf m) m 0 (idxs[i1,i2]) 0 (idxs[j1,j2]))
  where
    (i1,i2) = getRowRange (rows m) rr
    (j1,j2) = getColRange (cols m) rc

-- | r0 c0 height width
data Slice s t = Slice (STMatrix s t) Int Int Int Int

slice :: Element a => Slice t a -> Matrix a
slice (Slice (STMatrix m) r0 c0 nr nc) = subMatrix (r0,c0) (nr,nc) m

gemmm :: Element t => t -> Slice s t -> t -> Slice s t -> Slice s t -> ST s ()
gemmm beta (slice->r) alpha (slice->a) (slice->b) = res
  where
    res = unsafeIOToST (gemm v a b r)
    v = fromList [alpha,beta]


mutable :: Element t => (forall s . (Int, Int) -> STMatrix s t -> ST s u) -> Matrix t -> (Matrix t,u)
mutable f a = runST $ do
   x <- thawMatrix a
   info <- f (rows a, cols a) x
   r <- unsafeFreezeMatrix x
   return (r,info)
