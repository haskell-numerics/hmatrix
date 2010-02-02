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

module Data.Packed.Internal.Matrix(
    Matrix(..), rows, cols,
    MatrixOrder(..), orderOf,
    createMatrix, withMatrix, mat,
    cmat, fmat,
    toLists, flatten, reshape,
    Element(..),
    trans,
    fromRows, toRows, fromColumns, toColumns,
    matrixFromVector,
    subMatrix,
    liftMatrix, liftMatrix2,
    (@@>),
    saveMatrix,
    fromComplex, toComplex, conj,
    singleton
) where

import Data.Packed.Internal.Common
import Data.Packed.Internal.Signatures
import Data.Packed.Internal.Vector

import Foreign hiding (xor)
import Complex
import Foreign.C.Types
import Foreign.C.String

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
data Matrix t = MC { irows :: {-# UNPACK #-} !Int
                   , icols :: {-# UNPACK #-} !Int
                   , cdat :: {-# UNPACK #-} !(Vector t) }

              | MF { irows :: {-# UNPACK #-} !Int
                   , icols :: {-# UNPACK #-} !Int
                   , fdat :: {-# UNPACK #-} !(Vector t) }

-- MC: preferred by C, fdat may require a transposition
-- MF: preferred by LAPACK, cdat may require a transposition

rows :: Matrix t -> Int
rows = irows

cols :: Matrix t -> Int
cols = icols

xdat MC {cdat = d } = d
xdat MF {fdat = d } = d

orderOf :: Matrix t -> MatrixOrder
orderOf MF{} = ColumnMajor
orderOf MC{} = RowMajor

-- | Matrix transpose.
trans :: Matrix t -> Matrix t
trans MC {irows = r, icols = c, cdat = d } = MF {irows = c, icols = r, fdat = d }
trans MF {irows = r, icols = c, fdat = d } = MC {irows = c, icols = r, cdat = d }

cmat :: (Element t) => Matrix t -> Matrix t
cmat m@MC{} = m
cmat MF {irows = r, icols = c, fdat = d } = MC {irows = r, icols = c, cdat = transdata r d c}

fmat :: (Element t) => Matrix t -> Matrix t
fmat m@MF{} = m
fmat MC {irows = r, icols = c, cdat = d } = MF {irows = r, icols = c, fdat = transdata c d r}

-- C-Haskell matrix adapter
mat :: Adapt (CInt -> CInt -> Ptr t -> r) (Matrix t) r
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
toLists m = splitEvery (cols m) . toList . flatten $ m

-- | Create a matrix from a list of vectors.
-- All vectors must have the same dimension,
-- or dimension 1, which is are automatically expanded.
fromRows :: Element t => [Vector t] -> Matrix t
fromRows vs = case compatdim (map dim vs) of
    Nothing -> error "fromRows applied to [] or to vectors with different sizes"
    Just c  -> reshape c . join . map (adapt c) $ vs
  where
    adapt c v | dim v == c = v
              | otherwise = constantD (v@>0) c

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

MC {irows = r, icols = c, cdat = v} @@> (i,j)
    | safe      = if i<0 || i>=r || j<0 || j>=c
                    then error "matrix indexing out of range"
                    else v `at` (i*c+j)
    | otherwise = v `at` (i*c+j)

MF {irows = r, icols = c, fdat = v} @@> (i,j)
    | safe      = if i<0 || i>=r || j<0 || j>=c
                    then error "matrix indexing out of range"
                    else v `at` (j*r+i)
    | otherwise = v `at` (j*r+i)
{-# INLINE (@@>) #-}

--  Unsafe matrix access without range checking
atM' MC {icols = c, cdat = v} i j = v `at'` (i*c+j)
atM' MF {irows = r, fdat = v} i j = v `at'` (j*r+i)
{-# INLINE atM' #-}

------------------------------------------------------------------

matrixFromVector RowMajor c v = MC { irows = r, icols = c, cdat = v }
    where (d,m) = dim v `divMod` c
          r | m==0 = d
            | otherwise = error "matrixFromVector"

matrixFromVector ColumnMajor c v = MF { irows = r, icols = c, fdat = v }
    where (d,m) = dim v `divMod` c
          r | m==0 = d
            | otherwise = error "matrixFromVector"

-- allocates memory for a new matrix
createMatrix :: (Storable a) => MatrixOrder -> Int -> Int -> IO (Matrix a)
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
liftMatrix f MC { icols = c, cdat = d } = matrixFromVector RowMajor    c (f d)
liftMatrix f MF { icols = c, fdat = d } = matrixFromVector ColumnMajor c (f d)

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
    subMatrixD = subMatrix'
    transdata :: Int -> Vector a -> Int -> Vector a
    transdata = transdata'
    constantD  :: a -> Int -> Vector a
    constantD = constant'

instance Element Double where
    transdata  = transdataAux ctransR
    constantD  = constantAux cconstantR

instance Element (Complex Double) where
    transdata  = transdataAux ctransC
    constantD  = constantAux cconstantC

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

-- C versions, still a little faster:

transdataAux fun c1 d c2 =
    if noneed
        then d
        else unsafePerformIO $ do
            v <- createVector (dim d)
            withForeignPtr (fptr d) $ \pd ->
                withForeignPtr (fptr v) $ \pv ->
                    fun (fi r1) (fi c1) pd (fi r2) (fi c2) pv // check "transdataAux"
            return v
  where r1 = dim d `div` c1
        r2 = dim d `div` c2
        noneed = r1 == 1 || c1 == 1

foreign import ccall "transR" ctransR :: TMM
foreign import ccall "transC" ctransC :: TCMCM
----------------------------------------------------------------------

constant' v n = unsafePerformIO $ do
    w <- createVector n
    withForeignPtr (fptr w) $ \p -> do
        let go (-1) = return ()
            go !k = pokeElemOff p k v >> go (k-1)
        go (n-1)
    return w

-- C versions

constantAux fun x n = unsafePerformIO $ do
    v <- createVector n
    px <- newArray [x]
    app1 (fun px) vec v "constantAux"
    free px
    return v

constantR :: Double -> Int -> Vector Double
constantR = constantAux cconstantR
foreign import ccall "constantR" cconstantR :: Ptr Double -> TV

constantC :: Complex Double -> Int -> Vector (Complex Double)
constantC = constantAux cconstantC
foreign import ccall "constantC" cconstantC :: Ptr (Complex Double) -> TCV
----------------------------------------------------------------------

-- | Extracts a submatrix from a matrix.
subMatrix :: Element a
          => (Int,Int) -- ^ (r0,c0) starting position 
          -> (Int,Int) -- ^ (rt,ct) dimensions of submatrix
          -> Matrix a -- ^ input matrix
          -> Matrix a -- ^ result
subMatrix (r0,c0) (rt,ct) m
    | 0 <= r0 && 0 < rt && r0+rt <= (rows m) &&
      0 <= c0 && 0 < ct && c0+ct <= (cols m) = subMatrixD (r0,c0) (rt,ct) m
    | otherwise = error $ "wrong subMatrix "++
                          show ((r0,c0),(rt,ct))++" of "++show(rows m)++"x"++ show (cols m)

subMatrix'' (r0,c0) (rt,ct) c v = unsafePerformIO $ do
    w <- createVector (rt*ct)
    withForeignPtr (fptr v) $ \p ->
        withForeignPtr (fptr w) $ \q -> do
            let go (-1) _ = return ()
                go !i (-1) = go (i-1) (ct-1)
                go !i !j = do x <- peekElemOff p ((i+r0)*c+j+c0)
                              pokeElemOff      q (i*ct+j) x
                              go i (j-1)
            go (rt-1) (ct-1)
    return w

subMatrix' (r0,c0) (rt,ct) (MC _r c v) = MC rt ct $ subMatrix'' (r0,c0) (rt,ct) c v
subMatrix' (r0,c0) (rt,ct) m = trans $ subMatrix' (c0,r0) (ct,rt) (trans m)

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

--------------------------------------------------------------------------

-- | Saves a matrix as 2D ASCII table.
saveMatrix :: FilePath
           -> String     -- ^ format (%f, %g, %e)
           -> Matrix Double
           -> IO ()
saveMatrix filename fmt m = do
    charname <- newCString filename
    charfmt <- newCString fmt
    let o = if orderOf m == RowMajor then 1 else 0
    app1 (matrix_fprintf charname charfmt o) mat m "matrix_fprintf"
    free charname
    free charfmt

foreign import ccall "matrix_fprintf" matrix_fprintf :: Ptr CChar -> Ptr CChar -> CInt -> TM
