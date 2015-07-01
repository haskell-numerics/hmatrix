{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE FlexibleContexts         #-}
{-# LANGUAGE FlexibleInstances        #-}
{-# LANGUAGE BangPatterns             #-}
{-# LANGUAGE TypeOperators            #-}
{-# LANGUAGE TypeFamilies             #-}
{-# LANGUAGE ViewPatterns             #-}



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
import Internal.Vectorized hiding ((#))
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

size m = (irows m, icols m)
{-# INLINE size #-}

rowOrder m = xCol m == 1 || cols m == 1
{-# INLINE rowOrder #-}

colOrder m = xRow m == 1 || rows m == 1
{-# INLINE colOrder #-}

is1d (size->(r,c)) = r==1 || c==1
{-# INLINE is1d #-}

-- data is not contiguous
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
amatr :: Storable a => (CInt -> CInt -> Ptr a -> b) -> Matrix a -> b
amatr f x = inlinePerformIO (unsafeWith (xdat x) (return . f r c))
  where
    r  = fi (rows x)
    c  = fi (cols x)

{-# INLINE amat #-}
amat :: Storable a => (CInt -> CInt -> CInt -> CInt -> Ptr a -> b) -> Matrix a -> b
amat f x = inlinePerformIO (unsafeWith (xdat x) (return . f r c sr sc))
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

infixl 1 #
a # b = apply a b
{-# INLINE (#) #-}

--------------------------------------------------------------------------------

copy ord m = extractR ord m 0 (idxs[0,rows m-1]) 0 (idxs[0,cols m-1])

extractAll ord m = unsafePerformIO (copy ord m)

{- | Creates a vector by concatenation of rows. If the matrix is ColumnMajor, this operation requires a transpose.

>>> flatten (ident 3)
fromList [1.0,0.0,0.0,0.0,1.0,0.0,0.0,0.0,1.0]

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
atM' m i j = xdat m `at'` (i * (xRow m) + j * (xCol m))
{-# INLINE atM' #-}

------------------------------------------------------------------

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

-------------------------------------------------------------------

-- | reference to a rectangular slice of a matrix (no data copy)
subMatrix :: Element a
            => (Int,Int) -- ^ (r0,c0) starting position
            -> (Int,Int) -- ^ (rt,ct) dimensions of submatrix
            -> Matrix a -- ^ input matrix
            -> Matrix a -- ^ result
subMatrix (r0,c0) (rt,ct) m
    | 0 <= r0 && 0 <= rt && r0+rt <= rows m &&
      0 <= c0 && 0 <= ct && c0+ct <= cols m = res
    | otherwise = error $ "wrong subMatrix "++show ((r0,c0),(rt,ct))++" of "++shSize m
  where
    p = r0 * xRow m + c0 * xCol m
    tot | rowOrder m = ct + (rt-1) * xRow m
        | otherwise  = rt + (ct-1) * xCol m
    res = m { irows = rt, icols = ct, xdat = subVector p tot (xdat m) }

--------------------------------------------------------------------------

maxZ xs = if minimum xs == 0 then 0 else maximum xs

conformMs ms = map (conformMTo (r,c)) ms
  where
    r = maxZ (map rows ms)
    c = maxZ (map cols ms)


conformVs vs = map (conformVTo n) vs
  where
    n = maxZ (map dim vs)

conformMTo (r,c) m
    | size m == (r,c) = m
    | size m == (1,1) = matrixFromVector RowMajor r c (constantD (m@@>(0,0)) (r*c))
    | size m == (r,1) = repCols c m
    | size m == (1,c) = repRows r m
    | otherwise = error $ "matrix " ++ shSize m ++ " cannot be expanded to " ++ shDim (r,c)

conformVTo n v
    | dim v == n = v
    | dim v == 1 = constantD (v@>0) n
    | otherwise = error $ "vector of dim=" ++ show (dim v) ++ " cannot be expanded to dim=" ++ show n

repRows n x = fromRows (replicate n (flatten x))
repCols n x = fromColumns (replicate n (flatten x))

shSize = shDim . size

shDim (r,c) = "(" ++ show r ++"x"++ show c ++")"

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

extractAux f ord m moder vr modec vc = do
    let nr = if moder == 0 then fromIntegral $ vr@>1 - vr@>0 + 1 else dim vr
        nc = if modec == 0 then fromIntegral $ vc@>1 - vc@>0 + 1 else dim vc
    r <- createMatrix ord nr nc
    f moder modec # vr # vc # m # r  #|"extract"
    return r

type Extr x = CInt -> CInt -> CIdxs (CIdxs (OM x (OM x (IO CInt))))

foreign import ccall unsafe "extractD" c_extractD :: Extr Double
foreign import ccall unsafe "extractF" c_extractF :: Extr Float
foreign import ccall unsafe "extractC" c_extractC :: Extr (Complex Double)
foreign import ccall unsafe "extractQ" c_extractQ :: Extr (Complex Float)
foreign import ccall unsafe "extractI" c_extractI :: Extr CInt
foreign import ccall unsafe "extractL" c_extractL :: Extr Z

---------------------------------------------------------------

setRectAux f i j m r = f (fi i) (fi j) # m # r #|"setRect"

type SetRect x = I -> I -> x ::> x::> Ok

foreign import ccall unsafe "setRectD" c_setRectD :: SetRect Double
foreign import ccall unsafe "setRectF" c_setRectF :: SetRect Float
foreign import ccall unsafe "setRectC" c_setRectC :: SetRect (Complex Double)
foreign import ccall unsafe "setRectQ" c_setRectQ :: SetRect (Complex Float)
foreign import ccall unsafe "setRectI" c_setRectI :: SetRect I
foreign import ccall unsafe "setRectL" c_setRectL :: SetRect Z

--------------------------------------------------------------------------------

sortG f v = unsafePerformIO $ do
    r <- createVector (dim v)
    f # v # r #|"sortG"
    return r

sortIdxD = sortG c_sort_indexD
sortIdxF = sortG c_sort_indexF
sortIdxI = sortG c_sort_indexI
sortIdxL = sortG c_sort_indexL

sortValD = sortG c_sort_valD
sortValF = sortG c_sort_valF
sortValI = sortG c_sort_valI
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

compareG f u v = unsafePerformIO $ do
    r <- createVector (dim v)
    f # u # v # r #|"compareG"
    return r

compareD = compareG c_compareD
compareF = compareG c_compareF
compareI = compareG c_compareI
compareL = compareG c_compareL

foreign import ccall unsafe "compareD" c_compareD :: CV Double (CV Double (CV CInt (IO CInt)))
foreign import ccall unsafe "compareF" c_compareF :: CV Float (CV Float  (CV CInt (IO CInt)))
foreign import ccall unsafe "compareI" c_compareI :: CV CInt (CV CInt   (CV CInt (IO CInt)))
foreign import ccall unsafe "compareL" c_compareL :: Z :> Z :> I :> Ok

--------------------------------------------------------------------------------

selectG f c u v w = unsafePerformIO $ do
    r <- createVector (dim v)
    f # c # u # v # w # r #|"selectG"
    return r

selectD = selectG c_selectD
selectF = selectG c_selectF
selectI = selectG c_selectI
selectL = selectG c_selectL
selectC = selectG c_selectC
selectQ = selectG c_selectQ

type Sel x = CV CInt (CV x (CV x (CV x (CV x (IO CInt)))))

foreign import ccall unsafe "chooseD" c_selectD :: Sel Double
foreign import ccall unsafe "chooseF" c_selectF :: Sel Float
foreign import ccall unsafe "chooseI" c_selectI :: Sel CInt
foreign import ccall unsafe "chooseC" c_selectC :: Sel (Complex Double)
foreign import ccall unsafe "chooseQ" c_selectQ :: Sel (Complex Float)
foreign import ccall unsafe "chooseL" c_selectL :: Sel Z

---------------------------------------------------------------------------

remapG f i j m = unsafePerformIO $ do
    r <- createMatrix RowMajor (rows i) (cols i)
    f # i # j # m # r #|"remapG"
    return r

remapD = remapG c_remapD
remapF = remapG c_remapF
remapI = remapG c_remapI
remapL = remapG c_remapL
remapC = remapG c_remapC
remapQ = remapG c_remapQ

type Rem x = OM CInt (OM CInt (OM x (OM x (IO CInt))))

foreign import ccall unsafe "remapD" c_remapD :: Rem Double
foreign import ccall unsafe "remapF" c_remapF :: Rem Float
foreign import ccall unsafe "remapI" c_remapI :: Rem CInt
foreign import ccall unsafe "remapC" c_remapC :: Rem (Complex Double)
foreign import ccall unsafe "remapQ" c_remapQ :: Rem (Complex Float)
foreign import ccall unsafe "remapL" c_remapL :: Rem Z

--------------------------------------------------------------------------------

rowOpAux f c x i1 i2 j1 j2 m = do
    px <- newArray [x]
    f (fi c) px (fi i1) (fi i2) (fi j1) (fi j2) # m #|"rowOp"
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

gemmg f v m1 m2 m3 = f # v # m1 # m2 # m3 #|"gemmg"

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

foreign import ccall unsafe "saveMatrix" c_saveMatrix
    :: CString -> CString -> Double ..> Ok

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
    c_saveMatrix cname cformat `applyRaw` m #|"saveMatrix"
    free cname
    free cformat
    return ()

--------------------------------------------------------------------------------

