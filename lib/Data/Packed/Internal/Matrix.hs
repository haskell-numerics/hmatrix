{-# OPTIONS_GHC -fglasgow-exts -fallow-overlapping-instances #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Packed.Internal.Matrix
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

module Data.Packed.Internal.Matrix where

import Data.Packed.Internal.Common
import Data.Packed.Internal.Vector

import Foreign hiding (xor)
import Complex
import Control.Monad(when)
import Data.List(transpose,intersperse)
--import Data.Typeable
import Data.Maybe(fromJust)

----------------------------------------------------------------

class Storable a => Field a where
    constant :: a -> Int -> Vector a
    transdata :: Int -> Vector a -> Int -> Vector a
    multiplyD :: MatrixOrder -> Matrix a -> Matrix a -> Matrix a
    subMatrix :: (Int,Int) -- ^ (r0,c0) starting position 
              -> (Int,Int) -- ^ (rt,ct) dimensions of submatrix
              -> Matrix a -> Matrix a
    diag :: Vector a -> Matrix a


instance Field Double where
    constant  = constantR
    transdata = transdataR
    multiplyD = multiplyR
    subMatrix = subMatrixR
    diag      = diagR

instance Field (Complex Double) where
    constant  = constantC
    transdata = transdataC
    multiplyD = multiplyC
    subMatrix = subMatrixC
    diag      = diagC

-----------------------------------------------------------------

transdataR :: Int -> Vector Double -> Int -> Vector Double
transdataR = transdataAux ctransR

transdataC :: Int -> Vector (Complex Double) -> Int -> Vector (Complex Double)
transdataC = transdataAux ctransC

transdataAux fun c1 d c2 =
    if noneed
        then d
        else unsafePerformIO $ do
            v <- createVector (dim d)
            fun r1 c1 (ptr d) r2 c2 (ptr v) // check "transdataAux" [d]
            --putStrLn "---> transdataAux"
            return v
  where r1 = dim d `div` c1
        r2 = dim d `div` c2
        noneed = r1 == 1 || c1 == 1

foreign import ccall safe "aux.h transR"
    ctransR :: TMM -- Double ::> Double ::> IO Int
foreign import ccall safe "aux.h transC"
    ctransC :: TCMCM -- Complex Double ::> Complex Double ::> IO Int

transdataG c1 d c2 = fromList . concat . transpose . partit c1 . toList $ d





data MatrixOrder = RowMajor | ColumnMajor deriving (Show,Eq)

data Matrix t = M { rows    :: Int
                  , cols    :: Int
                  , dat     :: Vector t
                  , tdat    :: Vector t
                  , isTrans :: Bool
                  , order   :: MatrixOrder
                  } -- deriving Typeable


data NMat t = MC { rws, cls :: Int, dtc :: Vector t}
            | MF { rws, cls :: Int, dtf :: Vector t}
            | Tr (NMat t)

ntrans (Tr m) = m
ntrans m = Tr m

viewC m@MC{} = m
viewF m@MF{} = m

fortran m = order m == ColumnMajor

cdat m = if fortran m `xor` isTrans m then tdat m else dat m
fdat m = if fortran m `xor` isTrans m then dat m else tdat m

trans :: Matrix t -> Matrix t
trans m = m { rows = cols m
            , cols = rows m
            , isTrans = not (isTrans m)
            }

type Mt t s = Int -> Int -> Ptr t -> s
-- not yet admitted by my haddock version
-- infixr 6 ::>
-- type t ::> s = Mt t s

mat d m f = f (rows m) (cols m) (ptr (d m))

toLists :: (Storable t) => Matrix t -> [[t]]
toLists m = partit (cols m) . toList . cdat $ m

instance (Show a, Storable a) => (Show (Matrix a)) where
    show m = (sizes++) . dsp . map (map show) . toLists $ m
        where sizes = "("++show (rows m)++"><"++show (cols m)++")\n"

dsp as = (++" ]") . (" ["++) . init . drop 2 . unlines . map (" , "++) . map unwords' $ transpose mtp
    where
        mt = transpose as
        longs = map (maximum . map length) mt
        mtp = zipWith (\a b -> map (pad a) b) longs mt
        pad n str = replicate (n - length str) ' ' ++ str
        unwords' = concat . intersperse ", "

matrixFromVector RowMajor c v =
    M { rows = r
      , cols = c
      , dat  = v
      , tdat = transdata c v r
      , order = RowMajor
      , isTrans = False
      } where (d,m) = dim v `divMod` c
              r | m==0 = d
                | otherwise = error "matrixFromVector"

-- r = dim v `div` c -- TODO check mod=0

matrixFromVector ColumnMajor c v =
    M { rows = r
      , cols = c
      , dat  = v
      , tdat = transdata r v c
      , order = ColumnMajor
      , isTrans = False
      } where (d,m) = dim v `divMod` c
              r | m==0 = d
                | otherwise = error "matrixFromVector"

createMatrix order r c = do
    p <- createVector (r*c)
    return (matrixFromVector order c p)

{- | Creates a matrix from a vector by grouping the elements in rows with the desired number of columns.

@\> reshape 4 ('fromList' [1..12])
(3><4)
 [ 1.0,  2.0,  3.0,  4.0
 , 5.0,  6.0,  7.0,  8.0
 , 9.0, 10.0, 11.0, 12.0 ]@

-}
reshape :: Field t => Int -> Vector t -> Matrix t
reshape c v = matrixFromVector RowMajor c v

singleton x = reshape 1 (fromList [x])

liftMatrix :: (Field a, Field b) => (Vector a -> Vector b) -> Matrix a -> Matrix b
liftMatrix f m = reshape (cols m) (f (cdat m))

liftMatrix2 :: (Field t) => (Vector a -> Vector b -> Vector t) -> Matrix a -> Matrix b -> Matrix t
liftMatrix2 f m1 m2 | compat m1 m2 = reshape (cols m1) (f (cdat m1) (cdat m2))
                    | otherwise    = error "nonconformant matrices in liftMatrix2"
------------------------------------------------------------------

compat :: Matrix a -> Matrix b -> Bool
compat m1 m2 = rows m1 == rows m2 && cols m1 == cols m2

dotL a b = sum (zipWith (*) a b)

multiplyL a b | ok = [[dotL x y | y <- transpose b] | x <- a]
              | otherwise = error "inconsistent dimensions in contraction "
    where ok = case common length a of
                   Nothing -> False
                   Just c  -> c == length b

transL m = matrixFromVector RowMajor (rows m) $ transdata (cols m) (cdat m) (rows m)

multiplyG a b = matrixFromVector RowMajor (cols b) $ fromList $ concat $ multiplyL (toLists a) (toLists b)

------------------------------------------------------------------

gmatC m f | fortran m =
                if (isTrans m)
                    then f 0 (rows m) (cols m) (ptr (dat m))
                    else f 1 (cols m) (rows m) (ptr (dat m))
         | otherwise =
                if isTrans m
                    then f 1 (cols m) (rows m) (ptr (dat m))
                    else f 0 (rows m) (cols m) (ptr (dat m))


multiplyAux fun order a b = unsafePerformIO $ do
    when (cols a /= rows b) $ error $ "inconsistent dimensions in contraction "++
                                      show (rows a,cols a) ++ " x " ++ show (rows b, cols b)
    r <- createMatrix order (rows a) (cols b)
    fun // gmatC a // gmatC b // mat dat r // check "multiplyAux" [dat a, dat b]
    return r

foreign import ccall safe "aux.h multiplyR"
    cmultiplyR :: Int -> Int -> Int -> Ptr Double
               -> Int -> Int -> Int -> Ptr Double
               -> Int -> Int -> Ptr Double
               -> IO Int

foreign import ccall safe "aux.h multiplyC"
    cmultiplyC :: Int -> Int -> Int -> Ptr (Complex Double)
               -> Int -> Int -> Int -> Ptr (Complex Double)
               -> Int -> Int -> Ptr (Complex Double)
               -> IO Int

multiply :: (Field a) => MatrixOrder -> Matrix a -> Matrix a -> Matrix a
multiply RowMajor a b    = multiplyD RowMajor a b
multiply ColumnMajor a b = m {rows = cols m, cols = rows m, order = ColumnMajor}
    where m = multiplyD RowMajor (trans b) (trans a)


multiplyR = multiplyAux cmultiplyR
multiplyC = multiplyAux cmultiplyC

----------------------------------------------------------------------

-- | extraction of a submatrix of a real matrix
subMatrixR :: (Int,Int) -- ^ (r0,c0) starting position 
           -> (Int,Int) -- ^ (rt,ct) dimensions of submatrix
           -> Matrix Double -> Matrix Double
subMatrixR (r0,c0) (rt,ct) x = unsafePerformIO $ do
    r <- createMatrix RowMajor rt ct
    c_submatrixR r0 (r0+rt-1) c0 (c0+ct-1) // mat cdat x // mat cdat r // check "subMatrixR" [dat r]
    return r
foreign import ccall "aux.h submatrixR" c_submatrixR :: Int -> Int -> Int -> Int -> TMM

-- | extraction of a submatrix of a complex matrix
subMatrixC :: (Int,Int) -- ^ (r0,c0) starting position
           -> (Int,Int) -- ^ (rt,ct) dimensions of submatrix
           -> Matrix (Complex Double) -> Matrix (Complex Double)
subMatrixC (r0,c0) (rt,ct) x =
    reshape ct . asComplex . cdat .
    subMatrixR (r0,2*c0) (rt,2*ct) .
    reshape (2*cols x) . asReal . cdat $ x

--subMatrix :: (Field a) 
--          => (Int,Int) -- ^ (r0,c0) starting position 
--          -> (Int,Int) -- ^ (rt,ct) dimensions of submatrix
--          -> Matrix a -> Matrix a
--subMatrix st sz m
--    | isReal (baseOf.dat) m = scast $ subMatrixR st sz (scast m)
--    | isComp (baseOf.dat) m = scast $ subMatrixC st sz (scast m)
--    | otherwise             = subMatrixG st sz m

subMatrixG (r0,c0) (rt,ct) x = reshape ct $ fromList $ concat $ map (subList c0 ct) (subList r0 rt (toLists x))
    where subList s n = take n . drop s

---------------------------------------------------------------------

diagAux fun msg (v@V {dim = n}) = unsafePerformIO $ do
    m <- createMatrix RowMajor n n
    fun // vec v // mat dat m // check msg [dat m]
    return m {tdat = dat m}

-- | diagonal matrix from a real vector
diagR :: Vector Double -> Matrix Double
diagR = diagAux c_diagR "diagR"
foreign import ccall "aux.h diagR" c_diagR :: TVM

-- | diagonal matrix from a real vector
diagC :: Vector (Complex Double) -> Matrix (Complex Double)
diagC = diagAux c_diagC "diagC"
foreign import ccall "aux.h diagC" c_diagC :: TCVCM

-- | diagonal matrix from a vector
--diag :: (Num a, Field a) => Vector a -> Matrix a
--diag v
--    | isReal (baseOf) v = scast $ diagR (scast v)
--    | isComp (baseOf) v = scast $ diagC (scast v)
--    | otherwise             = diagG v

diagG v = reshape c $ fromList $ [ l!!(i-1) * delta k i | k <- [1..c], i <- [1..c]]
    where c = dim v
          l = toList v
          delta i j | i==j      = 1
                    | otherwise = 0

-- | creates a Matrix from a list of vectors
fromRows :: Field t => [Vector t] -> Matrix t
fromRows vs = case common dim vs of
    Nothing -> error "fromRows applied to [] or to vectors with different sizes"
    Just c  -> reshape c (join vs)

-- | extracts the rows of a matrix as a list of vectors
toRows :: Storable t => Matrix t -> [Vector t]
toRows m = toRows' 0 where
    v = cdat m
    r = rows m
    c = cols m
    toRows' k | k == r*c  = []
              | otherwise = subVector k c v : toRows' (k+c)

-- | Creates a matrix from a list of vectors, as columns
fromColumns :: Field t => [Vector t] -> Matrix t
fromColumns m = trans . fromRows $ m

-- | Creates a list of vectors from the columns of a matrix
toColumns :: Storable t => Matrix t -> [Vector t]
toColumns m = toRows . trans $ m


-- | Reads a matrix position.
(@@>) :: Storable t => Matrix t -> (Int,Int) -> t
infixl 9 @@>
m@M {rows = r, cols = c} @@> (i,j)
    | i<0 || i>=r || j<0 || j>=c = error "matrix indexing out of range"
    | otherwise   = cdat m `at` (i*c+j)

------------------------------------------------------------------

constantR :: Double -> Int -> Vector Double
constantR = constantAux cconstantR

constantC :: Complex Double -> Int -> Vector (Complex Double)
constantC = constantAux cconstantC

constantAux fun x n = unsafePerformIO $ do
    v <- createVector n
    px <- newArray [x]
    fun px // vec v // check "constantAux" []
    free px
    return v

foreign import ccall safe "aux.h constantR"
    cconstantR :: Ptr Double -> TV -- Double :> IO Int

foreign import ccall safe "aux.h constantC"
    cconstantC :: Ptr (Complex Double) -> TCV -- Complex Double :> IO Int
