{-# OPTIONS_GHC -fglasgow-exts #-}
{-# LANGUAGE CPP, BangPatterns #-}
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
import Foreign.C.String
import Foreign.C.Types

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
  which admit both orders.

- but if we need the transposed data and it is not in the structure, we must make
  sure that we touch the same foreignptr that is used in the computation.

- a reasonable solution is using two constructors for a matrix. Transposition just
  "flips" the constructor. Actual data transposition is not done if followed by a
  matrix product or another transpose.

-}

data MatrixOrder = RowMajor | ColumnMajor deriving (Show,Eq)

-- | Matrix representation suitable for GSL and LAPACK computations.
data Matrix t = MC { rows :: {-# UNPACK #-} !Int
                   , cols :: {-# UNPACK #-} !Int
                   , cdat :: {-# UNPACK #-} !(Vector t) }

              | MF { rows :: {-# UNPACK #-} !Int
                   , cols :: {-# UNPACK #-} !Int
                   , fdat :: {-# UNPACK #-} !(Vector t) }

-- MC: preferred by C, fdat may require a transposition
-- MF: preferred by LAPACK, cdat may require a transposition

xdat MC {cdat = d } = d
xdat MF {fdat = d } = d

orderOf MF{} = ColumnMajor
orderOf MC{} = RowMajor

-- | Matrix transpose.
trans :: Matrix t -> Matrix t
trans MC {rows = r, cols = c, cdat = d } = MF {rows = c, cols = r, fdat = d }
trans MF {rows = r, cols = c, fdat = d } = MC {rows = c, cols = r, cdat = d }

cmat m@MC{} = m
cmat MF {rows = r, cols = c, fdat = d } = MC {rows = r, cols = c, cdat = transdata r d c}

fmat m@MF{} = m
fmat MC {rows = r, cols = c, cdat = d } = MF {rows = r, cols = c, fdat = transdata c d r}

mat = withMatrix

withMatrix a f =
    withForeignPtr (fptr (xdat a)) $ \p -> do
        let m g = do
            g (fi (rows a)) (fi (cols a)) p
        f m

{- | Creates a vector by concatenation of rows

@\> flatten ('ident' 3)
9 |> [1.0,0.0,0.0,0.0,1.0,0.0,0.0,0.0,1.0]@
-}
flatten :: Element t => Matrix t -> Vector t
flatten = cdat . cmat

type Mt t s = Int -> Int -> Ptr t -> s
-- not yet admitted by my haddock version
-- infixr 6 ::>
-- type t ::> s = Mt t s

-- | the inverse of 'Data.Packed.Matrix.fromLists'
toLists :: (Element t) => Matrix t -> [[t]]
toLists m = partit (cols m) . toList . flatten $ m

-- | creates a Matrix from a list of vectors
fromRows :: Element t => [Vector t] -> Matrix t
fromRows vs = case common dim vs of
    Nothing -> error "fromRows applied to [] or to vectors with different sizes"
    Just c  -> reshape c (join vs)

-- | extracts the rows of a matrix as a list of vectors
toRows :: Element t => Matrix t -> [Vector t]
toRows m = toRows' 0 where
    v = flatten $ m
    r = rows m
    c = cols m
    toRows' k | k == r*c  = []
              | otherwise = subVector k c v : toRows' (k+c)

-- | Creates a matrix from a list of vectors, as columns
fromColumns :: Element t => [Vector t] -> Matrix t
fromColumns m = trans . fromRows $ m

-- | Creates a list of vectors from the columns of a matrix
toColumns :: Element t => Matrix t -> [Vector t]
toColumns m = toRows . trans $ m

-- | Reads a matrix position.
(@@>) :: Storable t => Matrix t -> (Int,Int) -> t
infixl 9 @@>
--m@M {rows = r, cols = c} @@> (i,j)
--    | i<0 || i>=r || j<0 || j>=c = error "matrix indexing out of range"
--    | otherwise   = cdat m `at` (i*c+j)

MC {rows = r, cols = c, cdat = v} @@> (i,j)
    | safe      = if i<0 || i>=r || j<0 || j>=c
                    then error "matrix indexing out of range"
                    else v `at` (i*c+j)
    | otherwise = v `at` (i*c+j)

MF {rows = r, cols = c, fdat = v} @@> (i,j)
    | safe      = if i<0 || i>=r || j<0 || j>=c
                    then error "matrix indexing out of range"
                    else v `at` (j*r+i)
    | otherwise = v `at` (j*r+i)
{-# INLINE (@@>) #-}

-- | Unsafe matrix access without range checking
atM' MC {cols = c, cdat = v} i j = v `at'` (i*c+j)
atM' MF {rows = r, fdat = v} i j = v `at'` (j*r+i)
{-# INLINE atM' #-}

------------------------------------------------------------------

matrixFromVector RowMajor c v = MC { rows = r, cols = c, cdat = v }
    where (d,m) = dim v `divMod` c
          r | m==0 = d
            | otherwise = error "matrixFromVector"

matrixFromVector ColumnMajor c v = MF { rows = r, cols = c, fdat = v }
    where (d,m) = dim v `divMod` c
          r | m==0 = d
            | otherwise = error "matrixFromVector"

createMatrix order r c = do
    p <- createVector (r*c)
    return (matrixFromVector order c p)

{- | Creates a matrix from a vector by grouping the elements in rows with the desired number of columns. (GNU-Octave groups by columns. To do it you can define @reshapeF r = trans . reshape r@
where r is the desired number of rows.)

@\> reshape 4 ('fromList' [1..12])
(3><4)
 [ 1.0,  2.0,  3.0,  4.0
 , 5.0,  6.0,  7.0,  8.0
 , 9.0, 10.0, 11.0, 12.0 ]@

-}
reshape :: Element t => Int -> Vector t -> Matrix t
reshape c v = matrixFromVector RowMajor c v

singleton x = reshape 1 (fromList [x])

-- | application of a vector function on the flattened matrix elements
liftMatrix :: (Element a, Element b) => (Vector a -> Vector b) -> Matrix a -> Matrix b
liftMatrix f MC { cols = c, cdat = d } = matrixFromVector RowMajor    c (f d)
liftMatrix f MF { cols = c, fdat = d } = matrixFromVector ColumnMajor c (f d)

-- | application of a vector function on the flattened matrices elements
liftMatrix2 :: (Element t, Element a, Element b) => (Vector a -> Vector b -> Vector t) -> Matrix a -> Matrix b -> Matrix t
liftMatrix2 f m1 m2
    | not (compat m1 m2) = error "nonconformant matrices in liftMatrix2"
    | otherwise = case m1 of
        MC {} -> matrixFromVector RowMajor    (cols m1) (f (cdat m1) (flatten m2))
        MF {} -> matrixFromVector ColumnMajor (cols m1) (f (fdat m1) ((fdat.fmat) m2))


compat :: Matrix a -> Matrix b -> Bool
compat m1 m2 = rows m1 == rows m2 && cols m1 == cols m2

------------------------------------------------------------------

-- | Auxiliary class.
class (Storable a, Floating a) => Element a where
    subMatrixD :: (Int,Int) -- ^ (r0,c0) starting position 
               -> (Int,Int) -- ^ (rt,ct) dimensions of submatrix
               -> Matrix a -> Matrix a
    transdata :: Int -> Vector a -> Int -> Vector a
    constantD  :: a -> Int -> Vector a

instance Element Double where
    subMatrixD = subMatrixR
    transdata  = transdata'
    constantD   = constant'

instance Element (Complex Double) where
    subMatrixD = subMatrixC
    transdata  = transdata'
    constantD   = constant'

-------------------------------------------------------------------

transdata' :: Storable a => Int -> Vector a -> Int -> Vector a
transdata' c1 v c2 =
    if noneed
        then v
        else unsafePerformIO $ do
                w <- createVector (r2*c2)
                withForeignPtr (fptr v) $ \p ->
                    withForeignPtr (fptr w) $ \q -> do
                        let go (-1) _ = return ()
                            go !i (-1) = go (i-1) (c1-1)
                            go !i !j = do x <- peekElemOff p (i*c1+j)
                                          pokeElemOff      q (j*c2+i) x
                                          go i (j-1)
                        go (r1-1) (c1-1)
                return w
  where r1 = dim v `div` c1
        r2 = dim v `div` c2
        noneed = r1 == 1 || c1 == 1

-- {-# SPECIALIZE transdata' :: Int -> Vector Double -> Int ->  Vector Double #-}
-- {-# SPECIALIZE transdata' :: Int -> Vector (Complex Double) -> Int -> Vector (Complex Double) #-}

-- I don't know how to specialize...
-- The above pragmas only seem to work on top level defs
-- Fortunately everything seems to work using the above class

----------------------------------------------------------------------

constant' v n = unsafePerformIO $ do
    w <- createVector n
    withForeignPtr (fptr w) $ \p -> do
        let go (-1) = return ()
            go !k = pokeElemOff p k v >> go (k-1)
        go (n-1)
    return w

----------------------------------------------------------------------

-- | extraction of a submatrix from a real matrix
subMatrixR :: (Int,Int) -> (Int,Int) -> Matrix Double -> Matrix Double
subMatrixR (r0,c0) (rt,ct) x' = unsafePerformIO $ do
    r <- createMatrix RowMajor rt ct
    let x = cmat x'
    app2 (c_submatrixR (fi r0) (fi $ r0+rt-1) (fi c0) (fi $ c0+ct-1)) mat x mat r "subMatrixR"
    return r
foreign import ccall "auxi.h submatrixR" c_submatrixR :: CInt -> CInt -> CInt -> CInt -> TMM

-- | extraction of a submatrix from a complex matrix
subMatrixC :: (Int,Int) -> (Int,Int) -> Matrix (Complex Double) -> Matrix (Complex Double)
subMatrixC (r0,c0) (rt,ct) x =
    reshape ct . asComplex . flatten .
    subMatrixR (r0,2*c0) (rt,2*ct) .
    reshape (2*cols x) . asReal . flatten $ x

-- | Extracts a submatrix from a matrix.
subMatrix :: Element a
          => (Int,Int) -- ^ (r0,c0) starting position 
          -> (Int,Int) -- ^ (rt,ct) dimensions of submatrix
          -> Matrix a -- ^ input matrix
          -> Matrix a -- ^ result
subMatrix = subMatrixD

--------------------------------------------------------------------------

-- | obtains the complex conjugate of a complex vector
conj :: Vector (Complex Double) -> Vector (Complex Double)
conj = mapVector conjugate

-- | creates a complex vector from vectors with real and imaginary parts
toComplex :: (Vector Double, Vector Double) ->  Vector (Complex Double)
toComplex (r,i) = asComplex $ flatten $ fromColumns [r,i]

-- | the inverse of 'toComplex'
fromComplex :: Vector (Complex Double) -> (Vector Double, Vector Double)
fromComplex z = (r,i) where
    [r,i] = toColumns $ reshape 2 $ asReal z

-- | loads a matrix efficiently from formatted ASCII text file (the number of rows and columns must be known in advance).
fromFile :: FilePath -> (Int,Int) -> IO (Matrix Double)
fromFile filename (r,c) = do
    charname <- newCString filename
    res <- createMatrix RowMajor r c
    app1 (c_gslReadMatrix charname) mat res "gslReadMatrix"
    --free charname  -- TO DO: free the auxiliary CString
    return res
foreign import ccall "auxi.h matrix_fscanf" c_gslReadMatrix:: Ptr CChar -> TM
