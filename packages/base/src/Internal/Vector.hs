{-# LANGUAGE MagicHash, UnboxedTuples, BangPatterns, FlexibleContexts #-}
{-# LANGUAGE TypeSynonymInstances #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

-- |
-- Module      :  Internal.Vector
-- Copyright   :  (c) Alberto Ruiz 2007-15
-- License     :  BSD3
-- Maintainer  :  Alberto Ruiz
-- Stability   :  provisional
--

module Internal.Vector(
    I,Z,R,C,
    fi,ti,
    Vector, fromList, unsafeToForeignPtr, unsafeFromForeignPtr, unsafeWith,
    createVector, avec, inlinePerformIO,
    toList, dim, (@>), at', (|>),
    vjoin, subVector, takesV, idxs,
    buildVector,
    asReal, asComplex,
    toByteString,fromByteString,
    zipVector, unzipVector, zipVectorWith, unzipVectorWith,
    foldVector, foldVectorG, foldVectorWithIndex, foldLoop,
    mapVector, mapVectorM, mapVectorM_,
    mapVectorWithIndex, mapVectorWithIndexM, mapVectorWithIndexM_
) where

import Foreign.Marshal.Array
import Foreign.ForeignPtr
import Foreign.Ptr
import Foreign.Storable
import Foreign.C.Types(CInt)
import Data.Int(Int64)
import Data.Complex
import System.IO.Unsafe(unsafePerformIO)
import GHC.ForeignPtr(mallocPlainForeignPtrBytes)
import GHC.Base(realWorld#, IO(IO), when)
import qualified Data.Vector.Storable as Vector
import Data.Vector.Storable(Vector, fromList, unsafeToForeignPtr, unsafeFromForeignPtr, unsafeWith)

import Data.Binary
import Data.Binary.Put
import Control.Monad(replicateM)
import qualified Data.ByteString.Internal as BS
import Data.Vector.Storable.Internal(updPtr)

type I = CInt
type Z = Int64
type R = Double
type C = Complex Double


-- | specialized fromIntegral
fi :: Int -> CInt
fi = fromIntegral

-- | specialized fromIntegral
ti :: CInt -> Int
ti = fromIntegral


-- | Number of elements
dim :: (Storable t) => Vector t -> Int
dim = Vector.length
{-# INLINE dim #-}


-- C-Haskell vector adapter
{-# INLINE avec #-}
avec :: Storable a => Vector a -> (f -> IO r) -> ((CInt -> Ptr a -> f) -> IO r)
avec v f g = unsafeWith v $ \ptr -> f (g (fromIntegral (Vector.length v)) ptr)

-- allocates memory for a new vector
createVector :: Storable a => Int -> IO (Vector a)
createVector n = do
    when (n < 0) $ error ("trying to createVector of negative dim: "++show n)
    fp <- doMalloc undefined
    return $ unsafeFromForeignPtr fp 0 n
  where
    --
    -- Use the much cheaper Haskell heap allocated storage
    -- for foreign pointer space we control
    --
    doMalloc :: Storable b => b -> IO (ForeignPtr b)
    doMalloc dummy = do
        mallocPlainForeignPtrBytes (n * sizeOf dummy)

{- | creates a Vector from a list:

@> fromList [2,3,5,7]
4 |> [2.0,3.0,5.0,7.0]@

-}

safeRead :: Storable a => Vector a -> (Ptr a -> IO c) -> c
safeRead v = inlinePerformIO . unsafeWith v
{-# INLINE safeRead #-}

inlinePerformIO :: IO a -> a
inlinePerformIO (IO m) = case m realWorld# of (# _, r #) -> r
{-# INLINE inlinePerformIO #-}

{- extracts the Vector elements to a list

>>> toList (linspace 5 (1,10))
[1.0,3.25,5.5,7.75,10.0]

-}
toList :: Storable a => Vector a -> [a]
toList v = safeRead v $ peekArray (dim v)

{- | Create a vector from a list of elements and explicit dimension. The input
     list is truncated if it is too long, so it may safely
     be used, for instance, with infinite lists.

>>> 5 |> [1..]
[1.0,2.0,3.0,4.0,5.0]
it :: (Enum a, Num a, Foreign.Storable.Storable a) => Vector a

-}
(|>) :: (Storable a) => Int -> [a] -> Vector a
infixl 9 |>
n |> l
    | length l' == n = fromList l'
    | otherwise      = error "list too short for |>"
  where
    l' = take n l


-- | Create a vector of indexes, useful for matrix extraction using '(??)'
idxs :: [Int] -> Vector I
idxs js = fromList (map fromIntegral js) :: Vector I

{- | takes a number of consecutive elements from a Vector

>>> subVector 2 3 (fromList [1..10])
[3.0,4.0,5.0]
it :: (Enum t, Num t, Foreign.Storable.Storable t) => Vector t

-}
subVector :: Storable t => Int       -- ^ index of the starting element
                        -> Int       -- ^ number of elements to extract
                        -> Vector t  -- ^ source
                        -> Vector t  -- ^ result
subVector = Vector.slice
{-# INLINE subVector #-}




{- | Reads a vector position:

>>> fromList [0..9] @> 7
7.0

-}
(@>) :: Storable t => Vector t -> Int -> t
infixl 9 @>
v @> n
    | n >= 0 && n < dim v = at' v n
    | otherwise = error "vector index out of range"
{-# INLINE (@>) #-}

-- | access to Vector elements without range checking
at' :: Storable a => Vector a -> Int -> a
at' v n = safeRead v $ flip peekElemOff n
{-# INLINE at' #-}

{- | concatenate a list of vectors

>>> vjoin [fromList [1..5::Double], konst 1 3]
[1.0,2.0,3.0,4.0,5.0,1.0,1.0,1.0]
it :: Vector Double

-}
vjoin :: Storable t => [Vector t] -> Vector t
vjoin [] = fromList []
vjoin [v] = v
vjoin as = unsafePerformIO $ do
    let tot = sum (map dim as)
    r <- createVector tot
    unsafeWith r $ \ptr ->
        joiner as tot ptr
    return r
  where joiner [] _ _ = return ()
        joiner (v:cs) _ p = do
            let n = dim v
            unsafeWith v $ \pb -> copyArray p pb n
            joiner cs 0 (advancePtr p n)


{- | Extract consecutive subvectors of the given sizes.

>>> takesV [3,4] (linspace 10 (1,10::Double))
[[1.0,2.0,3.0],[4.0,5.0,6.0,7.0]]
it :: [Vector Double]

-}
takesV :: Storable t => [Int] -> Vector t -> [Vector t]
takesV ms w | sum ms > dim w = error $ "takesV " ++ show ms ++ " on dim = " ++ (show $ dim w)
            | otherwise = go ms w
    where go [] _ = []
          go (n:ns) v = subVector 0 n v
                      : go ns (subVector n (dim v - n) v)

---------------------------------------------------------------

-- | transforms a complex vector into a real vector with alternating real and imaginary parts
asReal :: (RealFloat a, Storable a) => Vector (Complex a) -> Vector a
asReal v = unsafeFromForeignPtr (castForeignPtr fp) (2*i) (2*n)
    where (fp,i,n) = unsafeToForeignPtr v

-- | transforms a real vector into a complex vector with alternating real and imaginary parts
asComplex :: (RealFloat a, Storable a) => Vector a -> Vector (Complex a)
asComplex v = unsafeFromForeignPtr (castForeignPtr fp) (i `div` 2) (n `div` 2)
    where (fp,i,n) = unsafeToForeignPtr v

--------------------------------------------------------------------------------


-- | map on Vectors
mapVector :: (Storable a, Storable b) => (a-> b) -> Vector a -> Vector b
mapVector f v = unsafePerformIO $ do
    w <- createVector (dim v)
    unsafeWith v $ \p ->
        unsafeWith w $ \q -> do
            let go (-1) = return ()
                go !k = do x <- peekElemOff p k
                           pokeElemOff      q k (f x)
                           go (k-1)
            go (dim v -1)
    return w
{-# INLINE mapVector #-}

-- | zipWith for Vectors
zipVectorWith :: (Storable a, Storable b, Storable c) => (a-> b -> c) -> Vector a -> Vector b -> Vector c
zipVectorWith f u v = unsafePerformIO $ do
    let n = min (dim u) (dim v)
    w <- createVector n
    unsafeWith u $ \pu ->
        unsafeWith v $ \pv ->
            unsafeWith w $ \pw -> do
                let go (-1) = return ()
                    go !k = do x <- peekElemOff pu k
                               y <- peekElemOff pv k
                               pokeElemOff      pw k (f x y)
                               go (k-1)
                go (n -1)
    return w
{-# INLINE zipVectorWith #-}

-- | unzipWith for Vectors
unzipVectorWith :: (Storable (a,b), Storable c, Storable d)
                   => ((a,b) -> (c,d)) -> Vector (a,b) -> (Vector c,Vector d)
unzipVectorWith f u = unsafePerformIO $ do
      let n = dim u
      v <- createVector n
      w <- createVector n
      unsafeWith u $ \pu ->
          unsafeWith v $ \pv ->
              unsafeWith w $ \pw -> do
                  let go (-1) = return ()
                      go !k   = do z <- peekElemOff pu k
                                   let (x,y) = f z
                                   pokeElemOff      pv k x
                                   pokeElemOff      pw k y
                                   go (k-1)
                  go (n-1)
      return (v,w)
{-# INLINE unzipVectorWith #-}

foldVector :: Storable a => (a -> b -> b) -> b -> Vector a -> b
foldVector f x v = unsafePerformIO $
    unsafeWith v $ \p -> do
        let go (-1) s = return s
            go !k !s = do y <- peekElemOff p k
                          go (k-1::Int) (f y s)
        go (dim v -1) x
{-# INLINE foldVector #-}

-- the zero-indexed index is passed to the folding function
foldVectorWithIndex :: Storable a => (Int -> a -> b -> b) -> b -> Vector a -> b
foldVectorWithIndex f x v = unsafePerformIO $
    unsafeWith v $ \p -> do
        let go (-1) s = return s
            go !k !s = do y <- peekElemOff p k
                          go (k-1::Int) (f k y s)
        go (dim v -1) x
{-# INLINE foldVectorWithIndex #-}

foldLoop :: (Int -> t -> t) -> t -> Int -> t
foldLoop f s0 d = go (d - 1) s0
     where
       go 0 s = f (0::Int) s
       go !j !s = go (j - 1) (f j s)

foldVectorG :: Storable t1 => (Int -> (Int -> t1) -> t -> t) -> t -> Vector t1 -> t
foldVectorG f s0 v = foldLoop g s0 (dim v)
    where g !k !s = f k (safeRead v . flip peekElemOff) s
          {-# INLINE g #-} -- Thanks to Ryan Ingram (http://permalink.gmane.org/gmane.comp.lang.haskell.cafe/46479)
{-# INLINE foldVectorG #-}

-------------------------------------------------------------------

-- | monadic map over Vectors
--    the monad @m@ must be strict
mapVectorM :: (Storable a, Storable b, Monad m) => (a -> m b) -> Vector a -> m (Vector b)
mapVectorM f v = do
    w <- return $! unsafePerformIO $! createVector (dim v)
    mapVectorM' w 0 (dim v -1)
    return w
    where mapVectorM' w' !k !t
              | k == t               = do
                                       x <- return $! inlinePerformIO $! unsafeWith v $! \p -> peekElemOff p k
                                       y <- f x
                                       return $! inlinePerformIO $! unsafeWith w' $! \q -> pokeElemOff q k y
              | otherwise            = do
                                       x <- return $! inlinePerformIO $! unsafeWith v $! \p -> peekElemOff p k
                                       y <- f x
                                       _ <- return $! inlinePerformIO $! unsafeWith w' $! \q -> pokeElemOff q k y
                                       mapVectorM' w' (k+1) t
{-# INLINE mapVectorM #-}

-- | monadic map over Vectors
mapVectorM_ :: (Storable a, Monad m) => (a -> m ()) -> Vector a -> m ()
mapVectorM_ f v = do
    mapVectorM' 0 (dim v -1)
    where mapVectorM' !k !t
              | k == t            = do
                                    x <- return $! inlinePerformIO $! unsafeWith v $! \p -> peekElemOff p k
                                    f x
              | otherwise         = do
                                    x <- return $! inlinePerformIO $! unsafeWith v $! \p -> peekElemOff p k
                                    _ <- f x
                                    mapVectorM' (k+1) t
{-# INLINE mapVectorM_ #-}

-- | monadic map over Vectors with the zero-indexed index passed to the mapping function
--    the monad @m@ must be strict
mapVectorWithIndexM :: (Storable a, Storable b, Monad m) => (Int -> a -> m b) -> Vector a -> m (Vector b)
mapVectorWithIndexM f v = do
    w <- return $! unsafePerformIO $! createVector (dim v)
    mapVectorM' w 0 (dim v -1)
    return w
    where mapVectorM' w' !k !t
              | k == t               = do
                                       x <- return $! inlinePerformIO $! unsafeWith v $! \p -> peekElemOff p k
                                       y <- f k x
                                       return $! inlinePerformIO $! unsafeWith w' $! \q -> pokeElemOff q k y
              | otherwise            = do
                                       x <- return $! inlinePerformIO $! unsafeWith v $! \p -> peekElemOff p k
                                       y <- f k x
                                       _ <- return $! inlinePerformIO $! unsafeWith w' $! \q -> pokeElemOff q k y
                                       mapVectorM' w' (k+1) t
{-# INLINE mapVectorWithIndexM #-}

-- | monadic map over Vectors with the zero-indexed index passed to the mapping function
mapVectorWithIndexM_ :: (Storable a, Monad m) => (Int -> a -> m ()) -> Vector a -> m ()
mapVectorWithIndexM_ f v = do
    mapVectorM' 0 (dim v -1)
    where mapVectorM' !k !t
              | k == t            = do
                                    x <- return $! inlinePerformIO $! unsafeWith v $! \p -> peekElemOff p k
                                    f k x
              | otherwise         = do
                                    x <- return $! inlinePerformIO $! unsafeWith v $! \p -> peekElemOff p k
                                    _ <- f k x
                                    mapVectorM' (k+1) t
{-# INLINE mapVectorWithIndexM_ #-}


mapVectorWithIndex :: (Storable a, Storable b) => (Int -> a -> b) -> Vector a -> Vector b
--mapVectorWithIndex g = head . mapVectorWithIndexM (\a b -> [g a b])
mapVectorWithIndex f v = unsafePerformIO $ do
    w <- createVector (dim v)
    unsafeWith v $ \p ->
        unsafeWith w $ \q -> do
            let go (-1) = return ()
                go !k = do x <- peekElemOff p k
                           pokeElemOff      q k (f k x)
                           go (k-1)
            go (dim v -1)
    return w
{-# INLINE mapVectorWithIndex #-}

--------------------------------------------------------------------------------



-- a 64K cache, with a Double taking 13 bytes in Bytestring,
-- implies a chunk size of 5041
chunk :: Int
chunk = 5000

chunks :: Int -> [Int]
chunks d = let c = d `div` chunk
               m = d `mod` chunk
           in if m /= 0 then reverse (m:(replicate c chunk)) else (replicate c chunk)

putVector :: (Storable t, Binary t) => Vector t -> Data.Binary.Put.PutM ()
putVector v = mapM_ put $! toList v

getVector :: (Storable a, Binary a) => Int -> Get (Vector a)
getVector d = do
              xs <- replicateM d get
              return $! fromList xs

--------------------------------------------------------------------------------

toByteString :: Storable t => Vector t -> BS.ByteString
toByteString v = BS.PS (castForeignPtr fp) (sz*o) (sz * dim v)
  where
    (fp,o,_n) = unsafeToForeignPtr v
    sz = sizeOf (v@>0)


fromByteString :: Storable t => BS.ByteString -> Vector t
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
