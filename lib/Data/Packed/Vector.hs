{-# LANGUAGE FlexibleContexts #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Packed.Vector
-- Copyright   :  (c) Alberto Ruiz 2007
-- License     :  GPL-style
--
-- Maintainer  :  Alberto Ruiz <aruiz@um.es>
-- Stability   :  provisional
-- Portability :  portable
--
-- 1D arrays suitable for numeric computations using external libraries.
--
-- This module provides basic functions for manipulation of structure.
--
-----------------------------------------------------------------------------

module Data.Packed.Vector (
    Vector,
    fromList, (|>), toList, buildVector,
    dim, (@>),
    subVector, takesV, join,
    mapVector, zipVector, zipVectorWith, unzipVector, unzipVectorWith,
    mapVectorM, mapVectorM_, mapVectorWithIndexM, mapVectorWithIndexM_,
    foldLoop, foldVector, foldVectorG, foldVectorWithIndex,
    successive_, successive
) where

import Data.Packed.Internal.Vector
import Data.Binary
import Foreign.Storable
import Control.Monad(replicateM)

-------------------------------------------------------------------

-- a 64K cache, with a Double taking 13 bytes in Bytestring,
-- implies a chunk size of 5041
chunk :: Int
chunk = 5000

chunks :: Int -> [Int]
chunks d = let c = d `div` chunk
               m = d `mod` chunk
           in if m /= 0 then reverse (m:(replicate c chunk)) else (replicate c chunk)  

putVector v = do
              let d = dim v
              mapM_ (\i -> put $ v @> i) [0..(d-1)]

getVector d = do
              xs <- replicateM d get
              return $! fromList xs

instance (Binary a, Storable a) => Binary (Vector a) where
    put v = do
            let d = dim v
            put d
            mapM_ putVector $! takesV (chunks d) v
    get = do
          d <- get
          vs <- mapM getVector $ chunks d
          return $! join vs

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

newtype State s a = State { runState :: s -> (a,s) }

instance Monad (State s) where
    return a = State $ \s -> (a,s)
    m >>= f = State $ \s -> let (a,s') = runState m s
                            in runState (f a) s'

state_get :: State s s
state_get = State $ \s -> (s,s)

state_put :: s -> State s ()
state_put s = State $ \_ -> ((),s)

evalState :: State s a -> s -> a
evalState m s = fst $ runState m s

newtype MaybeT m a = MaybeT { runMaybeT :: m (Maybe a) }

instance Monad m => Monad (MaybeT m) where
    return a = MaybeT $ return $ Just a
    m >>= f  = MaybeT $ do
                        res <- runMaybeT m
                        case res of
                                 Nothing -> return Nothing
                                 Just r  -> runMaybeT (f r)
    fail _   = MaybeT $ return Nothing

lift_maybe m = MaybeT $ do
                        res <- m
                        return $ Just res

-- | apply a test to successive elements of a vector, evaluates to true iff test passes for all pairs
successive_ :: Storable a => (a -> a -> Bool) -> Vector a -> Bool
successive_ t v = maybe False (\_ -> True) $ evalState (runMaybeT (mapVectorM_ step (subVector 1 (dim v - 1) v))) (v @> 0)
   where step e = do
                  ep <- lift_maybe $ state_get
                  if t e ep
                     then lift_maybe $ state_put e
                     else (fail "successive_ test failed")

-- | operate on successive elements of a vector and return the resulting vector, whose length 1 less than that of the input
successive :: (Storable a, Storable b) => (a -> a -> b) -> Vector a -> Vector b
successive f v = evalState (mapVectorM step (subVector 1 (dim v - 1) v)) (v @> 0)
   where step e = do
                  ep <- state_get
                  state_put e
                  return $ f ep e

-------------------------------------------------------------------
