{-# OPTIONS_GHC -fglasgow-exts #-}

import qualified Data.ListLike as LL
import LinearAlgebra
import Data.Monoid
import Data.Packed.Internal.Vector
import Foreign

instance (Storable a) => Monoid (Vector a) where
    mempty = V { dim = 0, fptr = undefined, ptr = undefined }
    mappend a b = mconcat [a,b]
    mconcat = j . filter ((>0).dim)
        where j [] = mempty
              j l  = join l

instance Storable a => LL.FoldableLL (Vector a) a where
    foldl f x v = foldl f x (toList v)
    foldr f x v = foldr f x (toList v)

instance Storable a => LL.ListLike (Vector a) a where
    singleton a = fromList [a]
    head a = a @> 0
    tail a | dim a == 1 = mempty
           | otherwise  = subVector 1 (dim a -1) a
    genericLength = fromIntegral.dim


v k = fromList [1..k] :: Vector Double

f k = k+(3::Double)

main = print $ (LL.map f [1..5] :: Vector Double)