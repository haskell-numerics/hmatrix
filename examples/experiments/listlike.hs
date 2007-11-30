{-# OPTIONS_GHC -fglasgow-exts #-}

import qualified Data.ListLike as LL
import Numeric.LinearAlgebra
import Data.Monoid
import Foreign

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