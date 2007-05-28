-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Packed.Internal
-- Copyright   :  (c) Alberto Ruiz 2007
-- License     :  GPL-style
--
-- Maintainer  :  Alberto Ruiz <aruiz@um.es>
-- Stability   :  provisional
-- Portability :  portable (uses FFI)
--
-- Fundamental types
--
-----------------------------------------------------------------------------

module Data.Packed.Internal where

import Foreign
import Complex
import Control.Monad(when)
import Debug.Trace

debug x = trace (show x) x

-- | 1D array
data Vector t = V { dim  :: Int
                  , fptr :: ForeignPtr t
                  , ptr  :: Ptr t
                  }

data TransMode = NoTrans | Trans | ConjTrans

-- | 2D array
data Matrix t = M { rows     :: Int
                  , cols     :: Int
                  , mat      :: Vector t
                  , trMode   :: TransMode
                  , isCOrder :: Bool
                  }

data IdxTp = Covariant | Contravariant

-- | multidimensional array
data Tensor t = T { rank   :: Int
                  , dims   :: [Int]
                  , idxNm  :: [String]
                  , idxTp  :: [IdxTp]
                  , ten    :: Vector t
                  }

----------------------------------------------------------------------
instance (Storable a, RealFloat a) => Storable (Complex a) where    --
    alignment x = alignment (realPart x)                            --
    sizeOf x    = 2 * sizeOf (realPart x)                           --
    peek p = do                                                     --
        [re,im] <- peekArray 2 (castPtr p)                          --
        return (re :+ im)                                           --
    poke p (a :+ b) = pokeArray (castPtr p) [a,b]                   --
----------------------------------------------------------------------


-- f // vec a // vec b // vec res // check "vector add" [a,b]

(//) :: x -> (x -> y) -> y
infixl 0 //
(//) = flip ($)

vec :: Vector a -> (Int -> Ptr b -> t) -> t
vec v f = f (dim v) (castPtr $ ptr v)

check msg ls f = do
    err <- f
    when (err/=0) (error msg)
    mapM_ (touchForeignPtr . fptr) ls
    return ()

createVector :: Storable a => Int -> IO (Vector a)
createVector n = do
    when (n <= 0) $ error ("trying to createVector of dim "++show n)
    fp <- mallocForeignPtrArray n
    let p = unsafeForeignPtrToPtr fp
    return $ V n fp p

fromList :: Storable a => [a] -> Vector a
fromList l = unsafePerformIO $ do
    v <- createVector (length l)
    let f _ p = pokeArray p l >> return 0
    f // vec v // check "fromList" []
    return v

toList :: Storable a => Vector a -> [a]
toList v = unsafePerformIO $ peekArray (dim v) (ptr v)

at' :: Storable a => Vector a -> Int -> a
at' v n = unsafePerformIO $ peekElemOff (ptr v) n

at :: Storable a => Vector a -> Int -> a
at v n | n >= 0 && n < dim v = at' v n
       | otherwise          = error "vector index out of range"

constant :: Storable a => Int -> a -> Vector a
constant n x = unsafePerformIO $ do
    v <- createVector n
    let f k p | k == n    = return 0
              | otherwise = pokeElemOff p k x >> f (k+1) p
    const (f 0) // vec v // check "constant" []
    return v

instance (Show a, Storable a) => (Show (Vector a)) where
    show v = "fromList " ++ show (toList v)

instance (Show a, Storable a) => (Show (Matrix a)) where
    show m = "reshape "++show (cols m) ++ " $ fromList " ++ show (toList (mat m))

reshape :: Storable a => Int -> Vector a -> Matrix a
reshape n v = M { rows = dim v `div` n
                , cols = n
                , mat  = v
                , trMode = NoTrans
                , isCOrder = True
                }
