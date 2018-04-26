{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE FlexibleContexts         #-}
{-# LANGUAGE FlexibleInstances        #-}
{-# LANGUAGE BangPatterns             #-}
{-# LANGUAGE TypeOperators            #-}
{-# LANGUAGE TypeFamilies             #-}
{-# LANGUAGE ViewPatterns             #-}
{-# LANGUAGE DeriveGeneric            #-}
{-# LANGUAGE ConstrainedClassMethods  #-}

-- |
-- Module      :  Internal.Matrix
-- Copyright   :  (c) Alberto Ruiz 2007-15
-- License     :  BSD3
-- Maintainer  :  Alberto Ruiz
-- Stability   :  provisional
--
-- Internal matrix representation
--

module Internal.Matrix where

import Internal.Vector
import Internal.Devel
import Internal.Vectorized hiding ((#), (#!))
import Foreign.Marshal.Alloc ( free )
import Foreign.Marshal.Array(newArray)
import Foreign.Ptr ( Ptr )
import Foreign.Storable ( Storable )
import Data.Complex ( Complex )
import Foreign.C.Types ( CInt(..) )
import Foreign.C.String ( CString, newCString )
import System.IO.Unsafe ( unsafePerformIO )
import Control.DeepSeq ( NFData(..) )
import Text.Printf

-----------------------------------------------------------------

data MatrixOrder = RowMajor | ColumnMajor deriving (Show,Eq)

-- | Matrix representation suitable for BLAS\/LAPACK computations.

data Matrix t = Matrix
    { irows :: {-# UNPACK #-} !Int
    , icols :: {-# UNPACK #-} !Int
    , xRow  :: {-# UNPACK #-} !Int
    , xCol  :: {-# UNPACK #-} !Int
    , xdat  :: {-# UNPACK #-} !(Vector t)
    }


rows :: Matrix t -> Int
rows = irows
{-# INLINE rows #-}

cols :: Matrix t -> Int
cols = icols
{-# INLINE cols #-}

size :: Matrix t -> (Int, Int)
size m = (irows m, icols m)
{-# INLINE size #-}

rowOrder :: Matrix t -> Bool
rowOrder m = xCol m == 1 || cols m == 1
{-# INLINE rowOrder #-}

colOrder :: Matrix t -> Bool
colOrder m = xRow m == 1 || rows m == 1
{-# INLINE colOrder #-}

is1d :: Matrix t -> Bool
is1d (size->(r,c)) = r==1 || c==1
{-# INLINE is1d #-}

-- data is not contiguous
isSlice :: Storable t => Matrix t -> Bool
isSlice m@(size->(r,c)) = r*c < dim (xdat m)
{-# INLINE isSlice #-}

orderOf :: Matrix t -> MatrixOrder
orderOf m = if rowOrder m then RowMajor else ColumnMajor


showInternal :: Storable t => Matrix t -> IO ()
showInternal m = printf "%dx%d %s %s %d:%d (%d)\n" r c slc ord xr xc dv
  where
    r  = rows m
    c  = cols m
    xr = xRow m
    xc = xCol m
    slc = if isSlice m then "slice" else "full"
    ord = if is1d m then "1d" else if rowOrder m then "rows" else "cols"
    dv = dim (xdat m)

--------------------------------------------------------------------------------

-- | Matrix transpose.
trans :: Matrix t -> Matrix t
trans m@Matrix { irows = r, icols = c, xRow = xr, xCol = xc } =
             m { irows = c, icols = r, xRow = xc, xCol = xr }


cmat :: (Element t) => Matrix t -> Matrix t
cmat m
    | rowOrder m = m
    | otherwise  = extractAll RowMajor m


fmat :: (Element t) => Matrix t -> Matrix t
fmat m
    | colOrder m = m
    | otherwise  = extractAll ColumnMajor m


-- C-Haskell matrix adapters
{-# INLINE amatr #-}
amatr :: Storable a => Matrix a -> (f -> IO r) -> (CInt -> CInt -> Ptr a -> f) -> IO r
amatr x f g = unsafeWith (xdat x) (f . g r c)
  where
    r  = fi (rows x)
    c  = fi (cols x)

{-# INLINE amat #-}
amat :: Storable a => Matrix a -> (f -> IO r) -> (CInt -> CInt -> CInt -> CInt -> Ptr a -> f) -> IO r
amat x f g = unsafeWith (xdat x) (f . g r c sr sc)
  where
    r  = fi (rows x)
    c  = fi (cols x)
    sr = fi (xRow x)
    sc = fi (xCol x)


instance Storable t => TransArray (Matrix t)
  where
    type TransRaw (Matrix t) b = CInt -> CInt -> Ptr t -> b
    type Trans (Matrix t) b    = CInt -> CInt -> CInt -> CInt -> Ptr t -> b
    apply = amat
    {-# INLINE apply #-}
    applyRaw = amatr
    {-# INLINE applyRaw #-}

infixr 1 #
(#) :: TransArray c => c -> (b -> IO r) -> Trans c b -> IO r
a # b = apply a b
{-# INLINE (#) #-}

(#!) :: (TransArray c, TransArray c1) => c1 -> c -> Trans c1 (Trans c (IO r)) -> IO r
a #! b = a # b # id
{-# INLINE (#!) #-}

--------------------------------------------------------------------------------

copy :: Element t => MatrixOrder -> Matrix t -> IO (Matrix t)
copy ord m = extractR ord m 0 (idxs[0,rows m-1]) 0 (idxs[0,cols m-1])

extractAll :: Element t => MatrixOrder -> Matrix t -> Matrix t
extractAll ord m = unsafePerformIO (copy ord m)

{- | Creates a vector by concatenation of rows. If the matrix is ColumnMajor, this operation requires a transpose.

>>> flatten (ident 3)
[1.0,0.0,0.0,0.0,1.0,0.0,0.0,0.0,1.0]
it :: (Num t, Element t) => Vector t

-}
flatten :: Element t => Matrix t -> Vector t
flatten m
    | isSlice m || not (rowOrder m) = xdat (extractAll RowMajor m)
    | otherwise                     = xdat m


-- | the inverse of 'Data.Packed.Matrix.fromLists'
toLists :: (Element t) => Matrix t -> [[t]]
toLists = map toList . toRows



-- | common value with \"adaptable\" 1
compatdim :: [Int] -> Maybe Int
compatdim [] = Nothing
compatdim [a] = Just a
compatdim (a:b:xs)
    | a==b = compatdim (b:xs)
    | a==1 = compatdim (b:xs)
    | b==1 = compatdim (a:xs)
    | otherwise = Nothing




-- | Create a matrix from a list of vectors.
-- All vectors must have the same dimension,
-- or dimension 1, which is are automatically expanded.
fromRows :: Element t => [Vector t] -> Matrix t
fromRows [] = emptyM 0 0
fromRows vs = case compatdim (map dim vs) of
    Nothing -> error $ "fromRows expects vectors with equal sizes (or singletons), given: " ++ show (map dim vs)
    Just 0  -> emptyM r 0
    Just c  -> matrixFromVector RowMajor r c . vjoin . map (adapt c) $ vs
  where
    r = length vs
    adapt c v
        | c == 0 = fromList[]
        | dim v == c = v
        | otherwise = constantD (v@>0) c

-- | extracts the rows of a matrix as a list of vectors
toRows :: Element t => Matrix t -> [Vector t]
toRows m
    | rowOrder m = map sub rowRange
    | otherwise  = map ext rowRange
  where
    rowRange = [0..rows m-1]
    sub k = subVector (k*xRow m) (cols m) (xdat m)
    ext k = xdat $ unsafePerformIO $ extractR RowMajor m 1 (idxs[k]) 0 (idxs[0,cols m-1])


-- | Creates a matrix from a list of vectors, as columns
fromColumns :: Element t => [Vector t] -> Matrix t
fromColumns m = trans . fromRows $ m

-- | Creates a list of vectors from the columns of a matrix
toColumns :: Element t => Matrix t -> [Vector t]
toColumns m = toRows . trans $ m

-- | Reads a matrix position.
(@@>) :: Storable t => Matrix t -> (Int,Int) -> t
infixl 9 @@>
m@Matrix {irows = r, icols = c} @@> (i,j)
    | i<0 || i>=r || j<0 || j>=c = error "matrix indexing out of range"
    | otherwise = atM' m i j
{-# INLINE (@@>) #-}

--  Unsafe matrix access without range checking
atM' :: Storable t => Matrix t -> Int -> Int -> t
atM' m i j = xdat m `at'` (i * (xRow m) + j * (xCol m))
{-# INLINE atM' #-}

------------------------------------------------------------------

matrixFromVector :: Storable t => MatrixOrder -> Int -> Int -> Vector t -> Matrix t
matrixFromVector _ 1 _ v@(dim->d) = Matrix { irows = 1, icols = d, xdat = v, xRow = d, xCol = 1 }
matrixFromVector _ _ 1 v@(dim->d) = Matrix { irows = d, icols = 1, xdat = v, xRow = 1, xCol = d }
matrixFromVector o r c v
    | r * c == dim v = m
    | otherwise = error $ "can't reshape vector dim = "++ show (dim v)++" to matrix " ++ shSize m
  where
    m | o == RowMajor = Matrix { irows = r, icols = c, xdat = v, xRow = c, xCol = 1 }
      | otherwise     = Matrix { irows = r, icols = c, xdat = v, xRow = 1, xCol = r }

-- allocates memory for a new matrix
createMatrix :: (Storable a) => MatrixOrder -> Int -> Int -> IO (Matrix a)
createMatrix ord r c = do
    p <- createVector (r*c)
    return (matrixFromVector ord r c p)

{- | Creates a matrix from a vector by grouping the elements in rows with the desired number of columns. (GNU-Octave groups by columns. To do it you can define @reshapeF r = tr' . reshape r@
where r is the desired number of rows.)

>>> reshape 4 (fromList [1..12])
(3><4)
 [ 1.0,  2.0,  3.0,  4.0
 , 5.0,  6.0,  7.0,  8.0
 , 9.0, 10.0, 11.0, 12.0 ]

-}
reshape :: Storable t => Int -> Vector t -> Matrix t
reshape 0 v = matrixFromVector RowMajor 0 0 v
reshape c v = matrixFromVector RowMajor (dim v `div` c) c v


-- | application of a vector function on the flattened matrix elements
liftMatrix :: (Element a, Element b) => (Vector a -> Vector b) -> Matrix a -> Matrix b
liftMatrix f m@Matrix { irows = r, icols = c, xdat = d}
    | isSlice m = matrixFromVector RowMajor r c (f (flatten m))
    | otherwise = matrixFromVector (orderOf m) r c (f d)

-- | application of a vector function on the flattened matrices elements
liftMatrix2 :: (Element t, Element a, Element b) => (Vector a -> Vector b -> Vector t) -> Matrix a -> Matrix b -> Matrix t
liftMatrix2 f m1@(size->(r,c)) m2
    | (r,c)/=size m2 = error "nonconformant matrices in liftMatrix2"
    | rowOrder m1 = matrixFromVector RowMajor    r c (f (flatten m1) (flatten m2))
    | otherwise   = matrixFromVector ColumnMajor r c (f (flatten (trans m1)) (flatten (trans m2)))

------------------------------------------------------------------

-- | Supported matrix elements.
class (Storable a) => Element a where
    constantD  :: a -> Int -> Vector a
    extractR :: MatrixOrder -> Matrix a -> CInt -> Vector CInt -> CInt -> Vector CInt -> IO (Matrix a)
    setRect  :: Int -> Int -> Matrix a -> Matrix a -> IO ()
    sortI    :: Ord a => Vector a -> Vector CInt
    sortV    :: Ord a => Vector a -> Vector a
    compareV :: Ord a => Vector a -> Vector a -> Vector CInt
    selectV  :: Vector CInt -> Vector a -> Vector a -> Vector a -> Vector a
    remapM   :: Matrix CInt -> Matrix CInt -> Matrix a -> Matrix a
    rowOp    :: Int -> a -> Int -> Int -> Int -> Int -> Matrix a -> IO ()
    gemm     :: Vector a -> Matrix a -> Matrix a -> Matrix a -> IO ()
    reorderV :: Vector CInt-> Vector CInt-> Vector a -> Vector a -- see reorderVector for documentation


instance Element Float where
    constantD  = constantAux cconstantF
    extractR   = extractAux c_extractF
    setRect    = setRectAux c_setRectF
    sortI      = sortIdxF
    sortV      = sortValF
    compareV   = compareF
    selectV    = selectF
    remapM     = remapF
    rowOp      = rowOpAux c_rowOpF
    gemm       = gemmg c_gemmF
    reorderV   = reorderAux c_reorderF

instance Element Double where
    constantD  = constantAux cconstantR
    extractR   = extractAux c_extractD
    setRect    = setRectAux c_setRectD
    sortI      = sortIdxD
    sortV      = sortValD
    compareV   = compareD
    selectV    = selectD
    remapM     = remapD
    rowOp      = rowOpAux c_rowOpD
    gemm       = gemmg c_gemmD
    reorderV   = reorderAux c_reorderD

instance Element (Complex Float) where
    constantD  = constantAux cconstantQ
    extractR   = extractAux c_extractQ
    setRect    = setRectAux c_setRectQ
    sortI      = undefined
    sortV      = undefined
    compareV   = undefined
    selectV    = selectQ
    remapM     = remapQ
    rowOp      = rowOpAux c_rowOpQ
    gemm       = gemmg c_gemmQ
    reorderV   = reorderAux c_reorderQ

instance Element (Complex Double) where
    constantD  = constantAux cconstantC
    extractR   = extractAux c_extractC
    setRect    = setRectAux c_setRectC
    sortI      = undefined
    sortV      = undefined
    compareV   = undefined
    selectV    = selectC
    remapM     = remapC
    rowOp      = rowOpAux c_rowOpC
    gemm       = gemmg c_gemmC
    reorderV   = reorderAux c_reorderC

instance Element (CInt) where
    constantD  = constantAux cconstantI
    extractR   = extractAux c_extractI
    setRect    = setRectAux c_setRectI
    sortI      = sortIdxI
    sortV      = sortValI
    compareV   = compareI
    selectV    = selectI
    remapM     = remapI
    rowOp      = rowOpAux c_rowOpI
    gemm       = gemmg c_gemmI
    reorderV   = reorderAux c_reorderI

instance Element Z where
    constantD  = constantAux cconstantL
    extractR   = extractAux c_extractL
    setRect    = setRectAux c_setRectL
    sortI      = sortIdxL
    sortV      = sortValL
    compareV   = compareL
    selectV    = selectL
    remapM     = remapL
    rowOp      = rowOpAux c_rowOpL
    gemm       = gemmg c_gemmL
    reorderV   = reorderAux c_reorderL

-------------------------------------------------------------------

-- | reference to a rectangular slice of a matrix (no data copy)
subMatrix :: Element a
            => (Int,Int) -- ^ (r0,c0) starting position
            -> (Int,Int) -- ^ (rt,ct) dimensions of submatrix
            -> Matrix a -- ^ input matrix
            -> Matrix a -- ^ result
subMatrix (r0,c0) (rt,ct) m
    | rt <= 0 || ct <= 0 = matrixFromVector RowMajor (max 0 rt) (max 0 ct) (fromList [])
    | 0 <= r0 && 0 <= rt && r0+rt <= rows m &&
      0 <= c0 && 0 <= ct && c0+ct <= cols m = res
    | otherwise = error $ "wrong subMatrix "++show ((r0,c0),(rt,ct))++" of "++shSize m
  where
    p = r0 * xRow m + c0 * xCol m
    tot | rowOrder m = ct + (rt-1) * xRow m
        | otherwise  = rt + (ct-1) * xCol m
    res = m { irows = rt, icols = ct, xdat = subVector p tot (xdat m) }

--------------------------------------------------------------------------

maxZ :: (Num t1, Ord t1, Foldable t) => t t1 -> t1
maxZ xs = if minimum xs == 0 then 0 else maximum xs

conformMs :: Element t => [Matrix t] -> [Matrix t]
conformMs ms = map (conformMTo (r,c)) ms
  where
    r = maxZ (map rows ms)
    c = maxZ (map cols ms)

conformVs :: Element t => [Vector t] -> [Vector t]
conformVs vs = map (conformVTo n) vs
  where
    n = maxZ (map dim vs)

conformMTo :: Element t => (Int, Int) -> Matrix t -> Matrix t
conformMTo (r,c) m
    | size m == (r,c) = m
    | size m == (1,1) = matrixFromVector RowMajor r c (constantD (m@@>(0,0)) (r*c))
    | size m == (r,1) = repCols c m
    | size m == (1,c) = repRows r m
    | otherwise = error $ "matrix " ++ shSize m ++ " cannot be expanded to " ++ shDim (r,c)

conformVTo :: Element t => Int -> Vector t -> Vector t
conformVTo n v
    | dim v == n = v
    | dim v == 1 = constantD (v@>0) n
    | otherwise = error $ "vector of dim=" ++ show (dim v) ++ " cannot be expanded to dim=" ++ show n

repRows :: Element t => Int -> Matrix t -> Matrix t
repRows n x = fromRows (replicate n (flatten x))
repCols :: Element t => Int -> Matrix t -> Matrix t
repCols n x = fromColumns (replicate n (flatten x))

shSize :: Matrix t -> [Char]
shSize = shDim . size

shDim :: (Show a, Show a1) => (a1, a) -> [Char]
shDim (r,c) = "(" ++ show r ++"x"++ show c ++")"

emptyM :: Storable t => Int -> Int -> Matrix t
emptyM r c = matrixFromVector RowMajor r c (fromList[])

----------------------------------------------------------------------

instance (Storable t, NFData t) => NFData (Matrix t)
  where
    rnf m | d > 0     = rnf (v @> 0)
          | otherwise = ()
      where
        d = dim v
        v = xdat m

---------------------------------------------------------------

extractAux :: (Eq t3, Eq t2, TransArray c, Storable a, Storable t1,
                Storable t, Num t3, Num t2, Integral t1, Integral t)
           => (t3 -> t2 -> CInt -> Ptr t1 -> CInt -> Ptr t
                  -> Trans c (CInt -> CInt -> CInt -> CInt -> Ptr a -> IO CInt))
           -> MatrixOrder -> c -> t3 -> Vector t1 -> t2 -> Vector t -> IO (Matrix a)
extractAux f ord m moder vr modec vc = do
    let nr = if moder == 0 then fromIntegral $ vr@>1 - vr@>0 + 1 else dim vr
        nc = if modec == 0 then fromIntegral $ vc@>1 - vc@>0 + 1 else dim vc
    r <- createMatrix ord nr nc
    (vr # vc # m #! r) (f moder modec)  #|"extract"

    return r

type Extr x = CInt -> CInt -> CIdxs (CIdxs (OM x (OM x (IO CInt))))

foreign import ccall unsafe "extractD" c_extractD :: Extr Double
foreign import ccall unsafe "extractF" c_extractF :: Extr Float
foreign import ccall unsafe "extractC" c_extractC :: Extr (Complex Double)
foreign import ccall unsafe "extractQ" c_extractQ :: Extr (Complex Float)
foreign import ccall unsafe "extractI" c_extractI :: Extr CInt
foreign import ccall unsafe "extractL" c_extractL :: Extr Z

---------------------------------------------------------------

setRectAux :: (TransArray c1, TransArray c)
           => (CInt -> CInt -> Trans c1 (Trans c (IO CInt)))
           -> Int -> Int -> c1 -> c -> IO ()
setRectAux f i j m r = (m #! r) (f (fi i) (fi j)) #|"setRect"

type SetRect x = I -> I -> x ::> x::> Ok

foreign import ccall unsafe "setRectD" c_setRectD :: SetRect Double
foreign import ccall unsafe "setRectF" c_setRectF :: SetRect Float
foreign import ccall unsafe "setRectC" c_setRectC :: SetRect (Complex Double)
foreign import ccall unsafe "setRectQ" c_setRectQ :: SetRect (Complex Float)
foreign import ccall unsafe "setRectI" c_setRectI :: SetRect I
foreign import ccall unsafe "setRectL" c_setRectL :: SetRect Z

--------------------------------------------------------------------------------

sortG :: (Storable t, Storable a)
      => (CInt -> Ptr t -> CInt -> Ptr a -> IO CInt) -> Vector t -> Vector a
sortG f v = unsafePerformIO $ do
    r <- createVector (dim v)
    (v #! r) f #|"sortG"
    return r

sortIdxD :: Vector Double -> Vector CInt
sortIdxD = sortG c_sort_indexD
sortIdxF :: Vector Float -> Vector CInt
sortIdxF = sortG c_sort_indexF
sortIdxI :: Vector CInt -> Vector CInt
sortIdxI = sortG c_sort_indexI
sortIdxL :: Vector Z -> Vector I
sortIdxL = sortG c_sort_indexL

sortValD :: Vector Double -> Vector Double
sortValD = sortG c_sort_valD
sortValF :: Vector Float -> Vector Float
sortValF = sortG c_sort_valF
sortValI :: Vector CInt -> Vector CInt
sortValI = sortG c_sort_valI
sortValL :: Vector Z -> Vector Z
sortValL = sortG c_sort_valL

foreign import ccall unsafe "sort_indexD" c_sort_indexD :: CV Double (CV CInt (IO CInt))
foreign import ccall unsafe "sort_indexF" c_sort_indexF :: CV Float  (CV CInt (IO CInt))
foreign import ccall unsafe "sort_indexI" c_sort_indexI :: CV CInt   (CV CInt (IO CInt))
foreign import ccall unsafe "sort_indexL" c_sort_indexL :: Z :> I :> Ok

foreign import ccall unsafe "sort_valuesD" c_sort_valD :: CV Double (CV Double (IO CInt))
foreign import ccall unsafe "sort_valuesF" c_sort_valF :: CV Float  (CV Float (IO CInt))
foreign import ccall unsafe "sort_valuesI" c_sort_valI :: CV CInt   (CV CInt (IO CInt))
foreign import ccall unsafe "sort_valuesL" c_sort_valL :: Z :> Z :> Ok

--------------------------------------------------------------------------------

compareG :: (TransArray c, Storable t, Storable a)
         => Trans c (CInt -> Ptr t -> CInt -> Ptr a -> IO CInt)
         -> c -> Vector t -> Vector a
compareG f u v = unsafePerformIO $ do
    r <- createVector (dim v)
    (u # v #! r) f #|"compareG"
    return r

compareD :: Vector Double -> Vector Double -> Vector CInt
compareD = compareG c_compareD
compareF :: Vector Float -> Vector Float -> Vector CInt
compareF = compareG c_compareF
compareI :: Vector CInt -> Vector CInt -> Vector CInt
compareI = compareG c_compareI
compareL :: Vector Z -> Vector Z -> Vector CInt
compareL = compareG c_compareL

foreign import ccall unsafe "compareD" c_compareD :: CV Double (CV Double (CV CInt (IO CInt)))
foreign import ccall unsafe "compareF" c_compareF :: CV Float (CV Float  (CV CInt (IO CInt)))
foreign import ccall unsafe "compareI" c_compareI :: CV CInt (CV CInt   (CV CInt (IO CInt)))
foreign import ccall unsafe "compareL" c_compareL :: Z :> Z :> I :> Ok

--------------------------------------------------------------------------------

selectG :: (TransArray c, TransArray c1, TransArray c2, Storable t, Storable a)
        => Trans c2 (Trans c1 (CInt -> Ptr t -> Trans c (CInt -> Ptr a -> IO CInt)))
        -> c2 -> c1 -> Vector t -> c -> Vector a
selectG f c u v w = unsafePerformIO $ do
    r <- createVector (dim v)
    (c # u # v # w #! r) f #|"selectG"
    return r

selectD :: Vector CInt -> Vector Double -> Vector Double -> Vector Double -> Vector Double
selectD = selectG c_selectD
selectF :: Vector CInt -> Vector Float -> Vector Float -> Vector Float -> Vector Float
selectF = selectG c_selectF
selectI :: Vector CInt -> Vector CInt -> Vector CInt -> Vector CInt -> Vector CInt
selectI = selectG c_selectI
selectL :: Vector CInt -> Vector Z -> Vector Z -> Vector Z -> Vector Z
selectL = selectG c_selectL
selectC :: Vector CInt
        -> Vector (Complex Double)
        -> Vector (Complex Double)
        -> Vector (Complex Double)
        -> Vector (Complex Double)
selectC = selectG c_selectC
selectQ :: Vector CInt
        -> Vector (Complex Float)
        -> Vector (Complex Float)
        -> Vector (Complex Float)
        -> Vector (Complex Float)
selectQ = selectG c_selectQ

type Sel x = CV CInt (CV x (CV x (CV x (CV x (IO CInt)))))

foreign import ccall unsafe "chooseD" c_selectD :: Sel Double
foreign import ccall unsafe "chooseF" c_selectF :: Sel Float
foreign import ccall unsafe "chooseI" c_selectI :: Sel CInt
foreign import ccall unsafe "chooseC" c_selectC :: Sel (Complex Double)
foreign import ccall unsafe "chooseQ" c_selectQ :: Sel (Complex Float)
foreign import ccall unsafe "chooseL" c_selectL :: Sel Z

---------------------------------------------------------------------------

remapG :: (TransArray c, TransArray c1, Storable t, Storable a)
       => (CInt -> CInt -> CInt -> CInt -> Ptr t
                -> Trans c1 (Trans c (CInt -> CInt -> CInt -> CInt -> Ptr a -> IO CInt)))
       -> Matrix t -> c1 -> c -> Matrix a
remapG f i j m = unsafePerformIO $ do
    r <- createMatrix RowMajor (rows i) (cols i)
    (i # j # m #! r) f #|"remapG"
    return r

remapD :: Matrix CInt -> Matrix CInt -> Matrix Double -> Matrix Double
remapD = remapG c_remapD
remapF :: Matrix CInt -> Matrix CInt -> Matrix Float -> Matrix Float
remapF = remapG c_remapF
remapI :: Matrix CInt -> Matrix CInt -> Matrix CInt -> Matrix CInt
remapI = remapG c_remapI
remapL :: Matrix CInt -> Matrix CInt -> Matrix Z -> Matrix Z
remapL = remapG c_remapL
remapC :: Matrix CInt
       -> Matrix CInt
       -> Matrix (Complex Double)
       -> Matrix (Complex Double)
remapC = remapG c_remapC
remapQ :: Matrix CInt -> Matrix CInt -> Matrix (Complex Float) -> Matrix (Complex Float)
remapQ = remapG c_remapQ

type Rem x = OM CInt (OM CInt (OM x (OM x (IO CInt))))

foreign import ccall unsafe "remapD" c_remapD :: Rem Double
foreign import ccall unsafe "remapF" c_remapF :: Rem Float
foreign import ccall unsafe "remapI" c_remapI :: Rem CInt
foreign import ccall unsafe "remapC" c_remapC :: Rem (Complex Double)
foreign import ccall unsafe "remapQ" c_remapQ :: Rem (Complex Float)
foreign import ccall unsafe "remapL" c_remapL :: Rem Z

--------------------------------------------------------------------------------

rowOpAux :: (TransArray c, Storable a) =>
            (CInt -> Ptr a -> CInt -> CInt -> CInt -> CInt -> Trans c (IO CInt))
         -> Int -> a -> Int -> Int -> Int -> Int -> c -> IO ()
rowOpAux f c x i1 i2 j1 j2 m = do
    px <- newArray [x]
    (m # id) (f (fi c) px (fi i1) (fi i2) (fi j1) (fi j2)) #|"rowOp"
    free px

type RowOp x = CInt -> Ptr x -> CInt -> CInt -> CInt -> CInt -> x ::> Ok

foreign import ccall unsafe "rowop_double"  c_rowOpD :: RowOp R
foreign import ccall unsafe "rowop_float"   c_rowOpF :: RowOp Float
foreign import ccall unsafe "rowop_TCD"     c_rowOpC :: RowOp C
foreign import ccall unsafe "rowop_TCF"     c_rowOpQ :: RowOp (Complex Float)
foreign import ccall unsafe "rowop_int32_t" c_rowOpI :: RowOp I
foreign import ccall unsafe "rowop_int64_t" c_rowOpL :: RowOp Z
foreign import ccall unsafe "rowop_mod_int32_t" c_rowOpMI :: I -> RowOp I
foreign import ccall unsafe "rowop_mod_int64_t" c_rowOpML :: Z -> RowOp Z

--------------------------------------------------------------------------------

gemmg :: (TransArray c1, TransArray c, TransArray c2, TransArray c3)
      => Trans c3 (Trans c2 (Trans c1 (Trans c (IO CInt))))
      -> c3 -> c2 -> c1 -> c -> IO ()
gemmg f v m1 m2 m3 = (v # m1 # m2 #! m3) f #|"gemmg"

type Tgemm x = x :> x ::> x ::> x ::> Ok

foreign import ccall unsafe "gemm_double"  c_gemmD :: Tgemm R
foreign import ccall unsafe "gemm_float"   c_gemmF :: Tgemm Float
foreign import ccall unsafe "gemm_TCD"     c_gemmC :: Tgemm C
foreign import ccall unsafe "gemm_TCF"     c_gemmQ :: Tgemm (Complex Float)
foreign import ccall unsafe "gemm_int32_t" c_gemmI :: Tgemm I
foreign import ccall unsafe "gemm_int64_t" c_gemmL :: Tgemm Z
foreign import ccall unsafe "gemm_mod_int32_t" c_gemmMI :: I -> Tgemm I
foreign import ccall unsafe "gemm_mod_int64_t" c_gemmML :: Z -> Tgemm Z

--------------------------------------------------------------------------------

reorderAux :: (TransArray c, Storable t, Storable a1, Storable t1, Storable a) =>
              (CInt -> Ptr a -> CInt -> Ptr t1
                    -> Trans c (CInt -> Ptr t -> CInt -> Ptr a1 -> IO CInt))
           -> Vector t1 -> c -> Vector t -> Vector a1
reorderAux f s d v = unsafePerformIO $ do
    k <- createVector (dim s)
    r <- createVector (dim v)
    (k # s # d # v #! r) f #| "reorderV"
    return r

type Reorder x = CV CInt (CV CInt (CV CInt (CV x (CV x (IO CInt)))))

foreign import ccall unsafe "reorderD" c_reorderD :: Reorder Double
foreign import ccall unsafe "reorderF" c_reorderF :: Reorder Float
foreign import ccall unsafe "reorderI" c_reorderI :: Reorder CInt
foreign import ccall unsafe "reorderC" c_reorderC :: Reorder (Complex Double)
foreign import ccall unsafe "reorderQ" c_reorderQ :: Reorder (Complex Float)
foreign import ccall unsafe "reorderL" c_reorderL :: Reorder Z

-- | Transpose an array with dimensions @dims@ by making a copy using @strides@. For example, for an array with 3 indices,
--   @(reorderVector strides dims v) ! ((i * dims ! 1 + j) * dims ! 2 + k) == v ! (i * strides ! 0 + j * strides ! 1 + k * strides ! 2)@
--   This function is intended to be used internally by tensor libraries.
reorderVector :: Element a
                    => Vector CInt -- ^ @strides@: array strides
                    -> Vector CInt -- ^ @dims@: array dimensions of new array @v@
                    -> Vector a    -- ^ @v@: flattened input array
                    -> Vector a    -- ^ @v'@: flattened output array
reorderVector = reorderV

--------------------------------------------------------------------------------

foreign import ccall unsafe "saveMatrix" c_saveMatrix
    :: CString -> CString -> Double ::> Ok

{- | save a matrix as a 2D ASCII table
-}
saveMatrix
    :: FilePath
    -> String        -- ^ \"printf\" format (e.g. \"%.2f\", \"%g\", etc.)
    -> Matrix Double
    -> IO ()
saveMatrix name format m = do
    cname   <- newCString name
    cformat <- newCString format
    (m # id) (c_saveMatrix cname cformat) #|"saveMatrix"
    free cname
    free cformat
    return ()

--------------------------------------------------------------------------------
