{-# OPTIONS_GHC -fglasgow-exts #-}
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
-- Internal matrix representation
--
-----------------------------------------------------------------------------
-- #hide

module Data.Packed.Internal.Matrix where

import Data.Packed.Internal.Common
import Data.Packed.Internal.Vector

import Foreign hiding (xor)
import Complex
import Control.Monad(when)
import Data.List(transpose,intersperse)
import Data.Maybe(fromJust)

-----------------------------------------------------------------

{- Design considerations for the Matrix Type
   -----------------------------------------

- we must easily handle both row major and column major order,
  for bindings to LAPACK and GSL/C

- we'd like to simplify redundant matrix transposes:
   - Some of them arise from the order requirements of some functions
   - some functions (matrix product) admit transposed arguments

- maybe we don't really need this kind of simplification:
   - more complex code
   - some computational overhead
   - only appreciable gain in code with a lot of redundant transpositions
     and cheap matrix computations

- we could carry both the matrix and its (lazily computed) transpose.
  This may save some transpositions, but it is necessary to keep track of the
  data which is actually computed to be used by functions like the matrix product
  which admit both orders. Therefore, maybe it is better to have something like
  viewC and viewF, which may actually perform a transpose if required.

- but if we need the transposed data and it is not in the structure, we must make
  sure that we touch the same foreignptr that is used in the computation. Access
  to such pointer cannot be made by creating a new vector.

-}

data MatrixOrder = RowMajor | ColumnMajor deriving (Show,Eq)

-- | Matrix representation suitable for GSL and LAPACK computations.
data Matrix t = MC { rows :: Int, cols :: Int, cdat :: Vector t, fdat :: Vector t }
              | MF { rows :: Int, cols :: Int, fdat :: Vector t, cdat :: Vector t }

-- MC: preferred by C, fdat may require a transposition
-- MF: preferred by LAPACK, cdat may require a transposition

-- | matrix transpose
trans :: Matrix t -> Matrix t
trans MC {rows = r, cols = c, cdat = d, fdat = dt } = MF {rows = c, cols = r, fdat = d, cdat = dt }
trans MF {rows = r, cols = c, fdat = d, cdat = dt } = MC {rows = c, cols = r, cdat = d, fdat = dt }

dat MC { cdat = d } = d
dat MF { fdat = d } = d

mat d m f = f (rows m) (cols m) (ptr (d m))

type Mt t s = Int -> Int -> Ptr t -> s
-- not yet admitted by my haddock version
-- infixr 6 ::>
-- type t ::> s = Mt t s

-- | the inverse of 'Data.Packed.Matrix.fromLists'
toLists :: (Field t) => Matrix t -> [[t]]
toLists m = partit (cols m) . toList . cdat $ m

-- | creates a Matrix from a list of vectors
fromRows :: Field t => [Vector t] -> Matrix t
fromRows vs = case common dim vs of
    Nothing -> error "fromRows applied to [] or to vectors with different sizes"
    Just c  -> reshape c (join vs)

-- | extracts the rows of a matrix as a list of vectors
toRows :: Field t => Matrix t -> [Vector t]
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
toColumns :: Field t => Matrix t -> [Vector t]
toColumns m = toRows . trans $ m


-- | Reads a matrix position.
(@@>) :: Storable t => Matrix t -> (Int,Int) -> t
infixl 9 @@>
--m@M {rows = r, cols = c} @@> (i,j)
--    | i<0 || i>=r || j<0 || j>=c = error "matrix indexing out of range"
--    | otherwise   = cdat m `at` (i*c+j)

MC {rows = r, cols = c, cdat = v} @@> (i,j)
    | i<0 || i>=r || j<0 || j>=c = error "matrix indexing out of range"
    | otherwise                  = v `at` (i*c+j)

MF {rows = r, cols = c, fdat = v} @@> (i,j)
    | i<0 || i>=r || j<0 || j>=c = error "matrix indexing out of range"
    | otherwise                  = v `at` (j*r+i)

------------------------------------------------------------------

matrixFromVector RowMajor c v = MC { rows = r, cols = c, cdat = v, fdat = transdata c v r }
    where (d,m) = dim v `divMod` c
          r | m==0 = d
            | otherwise = error "matrixFromVector"

matrixFromVector ColumnMajor c v = MF { rows = r, cols = c, fdat = v, cdat = transdata r v c }
    where (d,m) = dim v `divMod` c
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

-- | application of a vector function on the flattened matrix elements
liftMatrix :: (Field a, Field b) => (Vector a -> Vector b) -> Matrix a -> Matrix b
liftMatrix f MC { cols = c, cdat = d } = matrixFromVector RowMajor    c (f d)
liftMatrix f MF { cols = c, fdat = d } = matrixFromVector ColumnMajor c (f d)

-- | application of a vector function on the flattened matrices elements
liftMatrix2 :: (Field t, Field a, Field b) => (Vector a -> Vector b -> Vector t) -> Matrix a -> Matrix b -> Matrix t
liftMatrix2 f m1 m2
    | not (compat m1 m2) = error "nonconformant matrices in liftMatrix2"
    | otherwise = case m1 of
        MC {} -> matrixFromVector RowMajor    (cols m1) (f (cdat m1) (cdat m2))
        MF {} -> matrixFromVector ColumnMajor (cols m1) (f (fdat m1) (fdat m2))


compat :: Matrix a -> Matrix b -> Bool
compat m1 m2 = rows m1 == rows m2 && cols m1 == cols m2

----------------------------------------------------------------

-- | Optimized matrix computations are provided for elements in the Field class.
class (Storable a, Floating a) => Field a where
    constantD :: a -> Int -> Vector a
    transdata :: Int -> Vector a -> Int -> Vector a
    multiplyD :: Matrix a -> Matrix a -> Matrix a
    subMatrixD :: (Int,Int) -- ^ (r0,c0) starting position 
               -> (Int,Int) -- ^ (rt,ct) dimensions of submatrix
               -> Matrix a -> Matrix a
    diagD :: Vector a -> Matrix a

instance Field Double where
    constantD  = constantR
    transdata = transdataR
    multiplyD  = multiplyR
    subMatrixD = subMatrixR
    diagD      = diagR

instance Field (Complex Double) where
    constantD  = constantC
    transdata  = transdataC
    multiplyD  = multiplyC
    subMatrixD = subMatrixC
    diagD      = diagC

------------------------------------------------------------------

instance (Show a, Field a) => (Show (Matrix a)) where
    show m = (sizes++) . dsp . map (map show) . toLists $ m
        where sizes = "("++show (rows m)++"><"++show (cols m)++")\n"

dsp as = (++" ]") . (" ["++) . init . drop 2 . unlines . map (" , "++) . map unwords' $ transpose mtp
    where
        mt = transpose as
        longs = map (maximum . map length) mt
        mtp = zipWith (\a b -> map (pad a) b) longs mt
        pad n str = replicate (n - length str) ' ' ++ str
        unwords' = concat . intersperse ", "

------------------------------------------------------------------

(>|<) :: (Field a) => Int -> Int -> [a] -> Matrix a
r >|< c = f where
    f l | dim v == r*c = matrixFromVector ColumnMajor c v
        | otherwise    = error $ "inconsistent list size = "
                                 ++show (dim v) ++" in ("++show r++"><"++show c++")"
        where v = fromList l

-------------------------------------------------------------------

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

------------------------------------------------------------------

gmatC MF {rows = r, cols = c, fdat = d} f = f 1 c r (ptr d)
gmatC MC {rows = r, cols = c, cdat = d} f = f 0 r c (ptr d)

multiplyAux fun a b = unsafePerformIO $ do
    when (cols a /= rows b) $ error $ "inconsistent dimensions in contraction "++
                                      show (rows a,cols a) ++ " x " ++ show (rows b, cols b)
    r <- createMatrix RowMajor (rows a) (cols b)
    fun // gmatC a // gmatC b // mat dat r // check "multiplyAux" [dat a, dat b]
    return r

multiplyR = multiplyAux cmultiplyR
foreign import ccall safe "aux.h multiplyR"
    cmultiplyR :: Int -> Int -> Int -> Ptr Double
               -> Int -> Int -> Int -> Ptr Double
               -> Int -> Int -> Ptr Double
               -> IO Int

multiplyC = multiplyAux cmultiplyC
foreign import ccall safe "aux.h multiplyC"
    cmultiplyC :: Int -> Int -> Int -> Ptr (Complex Double)
               -> Int -> Int -> Int -> Ptr (Complex Double)
               -> Int -> Int -> Ptr (Complex Double)
               -> IO Int

multiply' :: (Field a) => MatrixOrder -> Matrix a -> Matrix a -> Matrix a
multiply' RowMajor a b    = multiplyD a b
multiply' ColumnMajor a b = trans $ multiplyD (trans b) (trans a)


-- | matrix product
multiply :: (Field a) => Matrix a -> Matrix a -> Matrix a
multiply = multiplyD

----------------------------------------------------------------------

-- | extraction of a submatrix from a real matrix
subMatrixR :: (Int,Int) -> (Int,Int) -> Matrix Double -> Matrix Double
subMatrixR (r0,c0) (rt,ct) x = unsafePerformIO $ do
    r <- createMatrix RowMajor rt ct
    c_submatrixR r0 (r0+rt-1) c0 (c0+ct-1) // mat cdat x // mat dat r // check "subMatrixR" [dat r]
    return r
foreign import ccall "aux.h submatrixR" c_submatrixR :: Int -> Int -> Int -> Int -> TMM

-- | extraction of a submatrix from a complex matrix
subMatrixC :: (Int,Int) -> (Int,Int) -> Matrix (Complex Double) -> Matrix (Complex Double)
subMatrixC (r0,c0) (rt,ct) x =
    reshape ct . asComplex . cdat .
    subMatrixR (r0,2*c0) (rt,2*ct) .
    reshape (2*cols x) . asReal . cdat $ x

-- | Extracts a submatrix from a matrix.
subMatrix :: Field a
          => (Int,Int) -- ^ (r0,c0) starting position 
          -> (Int,Int) -- ^ (rt,ct) dimensions of submatrix
          -> Matrix a -- ^ input matrix
          -> Matrix a -- ^ result
subMatrix = subMatrixD


---------------------------------------------------------------------

diagAux fun msg (v@V {dim = n}) = unsafePerformIO $ do
    m <- createMatrix RowMajor n n
    fun // vec v // mat cdat m // check msg [dat m]
    return m -- {tdat = dat m}

-- | diagonal matrix from a real vector
diagR :: Vector Double -> Matrix Double
diagR = diagAux c_diagR "diagR"
foreign import ccall "aux.h diagR" c_diagR :: TVM

-- | diagonal matrix from a real vector
diagC :: Vector (Complex Double) -> Matrix (Complex Double)
diagC = diagAux c_diagC "diagC"
foreign import ccall "aux.h diagC" c_diagC :: TCVCM

-- | creates a square matrix with the given diagonal
diag :: Field a => Vector a -> Matrix a
diag = diagD

------------------------------------------------------------------------

constantAux fun x n = unsafePerformIO $ do
    v <- createVector n
    px <- newArray [x]
    fun px // vec v // check "constantAux" []
    free px
    return v

constantR :: Double -> Int -> Vector Double
constantR = constantAux cconstantR
foreign import ccall safe "aux.h constantR"
    cconstantR :: Ptr Double -> TV -- Double :> IO Int

constantC :: Complex Double -> Int -> Vector (Complex Double)
constantC = constantAux cconstantC
foreign import ccall safe "aux.h constantC"
    cconstantC :: Ptr (Complex Double) -> TCV -- Complex Double :> IO Int

{- | creates a vector with a given number of equal components:

@> constant 2 7
7 |> [2.0,2.0,2.0,2.0,2.0,2.0,2.0]@
-}
constant :: Double -> Int -> Vector Double
constant = constantD

--------------------------------------------------------------------------

-- | obtains the complex conjugate of a complex vector
conj :: Vector (Complex Double) -> Vector (Complex Double)
conj v = asComplex $ cdat $ reshape 2 (asReal v) `multiply` diag (fromList [1,-1])

-- | creates a complex vector from vectors with real and imaginary parts
toComplex :: (Vector Double, Vector Double) ->  Vector (Complex Double)
toComplex (r,i) = asComplex $ cdat $ fromColumns [r,i]

-- | the inverse of 'toComplex'
fromComplex :: Vector (Complex Double) -> (Vector Double, Vector Double)
fromComplex z = (r,i) where
    [r,i] = toColumns $ reshape 2 $ asReal z

-- | converts a real vector into a complex representation (with zero imaginary parts)
comp :: Vector Double -> Vector (Complex Double)
comp v = toComplex (v,constant 0 (dim v))

-------------------------------------------------------------------------

-- Generic definitions

{-
transL m = matrixFromVector RowMajor (rows m) $ transdata (cols m) (cdat m) (rows m)

subMatrixG (r0,c0) (rt,ct) x = matrixFromVector RowMajor ct $ fromList $ concat $ map (subList c0 ct) (subList r0 rt (toLists x))
    where subList s n = take n . drop s

diagG v = matrixFromVector RowMajor c $ fromList $ [ l!!(i-1) * delta k i | k <- [1..c], i <- [1..c]]
    where c = dim v
          l = toList v
          delta i j | i==j      = 1
                    | otherwise = 0
-}

transdataG c1 d c2 = fromList . concat . transpose . partit c1 . toList $ d

dotL a b = sum (zipWith (*) a b)

multiplyG a b = matrixFromVector RowMajor (cols b) $ fromList $ concat $ multiplyL (toLists a) (toLists b)

multiplyL a b | ok = [[dotL x y | y <- transpose b] | x <- a]
              | otherwise = error "inconsistent dimensions in contraction "
    where ok = case common length a of
                   Nothing -> False
                   Just c  -> c == length b
