{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE FlexibleContexts         #-}
{-# LANGUAGE FlexibleInstances        #-}
{-# LANGUAGE BangPatterns             #-}
{-# LANGUAGE TypeOperators            #-}
{-# LANGUAGE TypeFamilies #-}


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
import Data.List.Split(chunksOf)

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

transOrder RowMajor = ColumnMajor
transOrder ColumnMajor = RowMajor
{- | Matrix representation suitable for BLAS\/LAPACK computations.

The elements are stored in a continuous memory array.

-}

data Matrix t = Matrix { irows :: {-# UNPACK #-} !Int
                       , icols :: {-# UNPACK #-} !Int
                       , xdat :: {-# UNPACK #-} !(Vector t)
                       , order :: !MatrixOrder }
-- RowMajor: preferred by C, fdat may require a transposition
-- ColumnMajor: preferred by LAPACK, cdat may require a transposition


rows :: Matrix t -> Int
rows = irows

cols :: Matrix t -> Int
cols = icols

orderOf :: Matrix t -> MatrixOrder
orderOf = order

stepRow :: Matrix t -> CInt
stepRow Matrix {icols = c, order = RowMajor } = fromIntegral c
stepRow _                                     = 1

stepCol :: Matrix t -> CInt
stepCol Matrix {irows = r, order = ColumnMajor } = fromIntegral r
stepCol _                                        = 1


-- | Matrix transpose.
trans :: Matrix t -> Matrix t
trans Matrix {irows = r, icols = c, xdat = d, order = o } = Matrix { irows = c, icols = r, xdat = d, order = transOrder o}

cmat :: (Element t) => Matrix t -> Matrix t
cmat m@Matrix{order = RowMajor} = m
cmat Matrix {irows = r, icols = c, xdat = d, order = ColumnMajor } = Matrix { irows = r, icols = c, xdat = transdata r d c, order = RowMajor}

fmat :: (Element t) => Matrix t -> Matrix t
fmat m@Matrix{order = ColumnMajor} = m
fmat Matrix {irows = r, icols = c, xdat = d, order = RowMajor } = Matrix { irows = r, icols = c, xdat = transdata c d r, order = ColumnMajor}

-- C-Haskell matrix adapter
-- mat :: Adapt (CInt -> CInt -> Ptr t -> r) (Matrix t) r

mat :: (Storable t) => Matrix t -> (((CInt -> CInt -> Ptr t -> t1) -> t1) -> IO b) -> IO b
mat a f =
    unsafeWith (xdat a) $ \p -> do
        let m g = do
            g (fi (rows a)) (fi (cols a)) p
        f m

omat :: (Storable t) => Matrix t -> (((CInt -> CInt -> CInt -> CInt -> Ptr t -> t1) -> t1) -> IO b) -> IO b
omat a f =
    unsafeWith (xdat a) $ \p -> do
        let m g = do
            g (fi (rows a)) (fi (cols a)) (stepRow a) (stepCol a) p
        f m

--------------------------------------------------------------------------------

{-# INLINE amatr #-}
amatr :: Storable a => (CInt -> CInt -> Ptr a -> b) -> Matrix a -> b
amatr f x = inlinePerformIO (unsafeWith (xdat x) (return . f r c))
  where
    r  = fromIntegral (rows x)
    c  = fromIntegral (cols x)

{-# INLINE amat #-}
amat :: Storable a => (CInt -> CInt -> CInt -> CInt -> Ptr a -> b) -> Matrix a -> b
amat f x = inlinePerformIO (unsafeWith (xdat x) (return . f r c sr sc))
  where
    r  = fromIntegral (rows x)
    c  = fromIntegral (cols x)
    sr = stepRow x
    sc = stepCol x

{-# INLINE arrmat #-}
arrmat :: Storable a => (Ptr CInt -> Ptr a -> b) -> Matrix a -> b
arrmat f x = inlinePerformIO (unsafeWith s (\p -> unsafeWith (xdat x) (return . f p)))
  where
    s = fromList [fi (rows x), fi (cols x), stepRow x, stepCol x]


instance Storable t => TransArray (Matrix t)
  where
    type Elem (Matrix t)       = t
    type TransRaw (Matrix t) b = CInt -> CInt -> Ptr t -> b
    type Trans (Matrix t) b    = CInt -> CInt -> CInt -> CInt -> Ptr t -> b
    apply = amat
    {-# INLINE apply #-}
    applyRaw = amatr
    {-# INLINE applyRaw #-}
    applyArray = arrmat
    {-# INLINE applyArray #-}

infixl 1 #
a # b = apply a b
{-# INLINE (#) #-}

--------------------------------------------------------------------------------

{- | Creates a vector by concatenation of rows. If the matrix is ColumnMajor, this operation requires a transpose.

>>> flatten (ident 3)
fromList [1.0,0.0,0.0,0.0,1.0,0.0,0.0,0.0,1.0]

-}
flatten :: Element t => Matrix t -> Vector t
flatten = xdat . cmat


-- | the inverse of 'Data.Packed.Matrix.fromLists'
toLists :: (Element t) => Matrix t -> [[t]]
toLists m = chunksOf (cols m) . toList . flatten $ m



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
    | c == 0    = replicate r (fromList[])
    | otherwise = toRows' 0
  where
    v = flatten m
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
m@Matrix {irows = r, icols = c} @@> (i,j)
    | i<0 || i>=r || j<0 || j>=c = error "matrix indexing out of range"
    | otherwise = atM' m i j
{-# INLINE (@@>) #-}

--  Unsafe matrix access without range checking
atM' Matrix {icols = c, xdat = v, order = RowMajor} i j = v `at'` (i*c+j)
atM' Matrix {irows = r, xdat = v, order = ColumnMajor} i j = v `at'` (j*r+i)
{-# INLINE atM' #-}

------------------------------------------------------------------

matrixFromVector o r c v
    | r * c == dim v = m
    | otherwise = error $ "can't reshape vector dim = "++ show (dim v)++" to matrix " ++ shSize m
  where
    m = Matrix { irows = r, icols = c, xdat = v, order = o }

-- allocates memory for a new matrix
createMatrix :: (Storable a) => MatrixOrder -> Int -> Int -> IO (Matrix a)
createMatrix ord r c = do
    p <- createVector (r*c)
    return (matrixFromVector ord r c p)

{- | Creates a matrix from a vector by grouping the elements in rows with the desired number of columns. (GNU-Octave groups by columns. To do it you can define @reshapeF r = trans . reshape r@
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

--singleton x = reshape 1 (fromList [x])

-- | application of a vector function on the flattened matrix elements
liftMatrix :: (Storable a, Storable b) => (Vector a -> Vector b) -> Matrix a -> Matrix b
liftMatrix f Matrix { irows = r, icols = c, xdat = d, order = o } = matrixFromVector o r c (f d)

-- | application of a vector function on the flattened matrices elements
liftMatrix2 :: (Element t, Element a, Element b) => (Vector a -> Vector b -> Vector t) -> Matrix a -> Matrix b -> Matrix t
liftMatrix2 f m1 m2
    | not (compat m1 m2) = error "nonconformant matrices in liftMatrix2"
    | otherwise = case orderOf m1 of
        RowMajor    -> matrixFromVector RowMajor    (rows m1) (cols m1) (f (xdat m1) (flatten m2))
        ColumnMajor -> matrixFromVector ColumnMajor (rows m1) (cols m1) (f (xdat m1) ((xdat.fmat) m2))


compat :: Matrix a -> Matrix b -> Bool
compat m1 m2 = rows m1 == rows m2 && cols m1 == cols m2

------------------------------------------------------------------

-- | Supported matrix elements.
class (Storable a) => Element a where
    transdata :: Int -> Vector a -> Int -> Vector a
    constantD  :: a -> Int -> Vector a
    extractR :: Matrix a -> CInt -> Vector CInt -> CInt -> Vector CInt -> IO (Matrix a)
    setRect  :: Int -> Int -> Matrix a -> Matrix a -> IO ()
    sortI    :: Ord a => Vector a -> Vector CInt
    sortV    :: Ord a => Vector a -> Vector a
    compareV :: Ord a => Vector a -> Vector a -> Vector CInt
    selectV  :: Vector CInt -> Vector a -> Vector a -> Vector a -> Vector a
    remapM   :: Matrix CInt -> Matrix CInt -> Matrix a -> Matrix a
    rowOp    :: Int -> a -> Int -> Int -> Int -> Int -> Matrix a -> IO ()
    gemm     :: Vector a -> Vector I -> Matrix a -> Matrix a -> Matrix a -> IO ()


instance Element Float where
    transdata  = transdataAux ctransF
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
    transdata  = transdataAux ctransR
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
    transdata  = transdataAux ctransQ
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
    transdata  = transdataAux ctransC
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
    transdata  = transdataAux ctransI
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
    transdata  = transdataAux ctransL
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

transdataAux fun c1 d c2 =
    if noneed
        then d
        else unsafePerformIO $ do
            -- putStrLn "T"
            v <- createVector (dim d)
            unsafeWith d $ \pd ->
                unsafeWith v $ \pv ->
                    fun (fi r1) (fi c1) pd (fi r2) (fi c2) pv // check "transdataAux"
            return v
  where r1 = dim d `div` c1
        r2 = dim d `div` c2
        noneed = dim d == 0 || r1 == 1 || c1 == 1


type TMM t = t ..> t ..> Ok

foreign import ccall unsafe "transF" ctransF :: TMM Float
foreign import ccall unsafe "transR" ctransR :: TMM Double
foreign import ccall unsafe "transQ" ctransQ :: TMM (Complex Float)
foreign import ccall unsafe "transC" ctransC :: TMM (Complex Double)
foreign import ccall unsafe "transI" ctransI :: TMM CInt
foreign import ccall unsafe "transL" ctransL :: TMM Z

----------------------------------------------------------------------

subMatrix :: Element a
          => (Int,Int) -- ^ (r0,c0) starting position
          -> (Int,Int) -- ^ (rt,ct) dimensions of submatrix
          -> Matrix a -- ^ input matrix
          -> Matrix a -- ^ result
subMatrix (r0,c0) (rt,ct) m
    | 0 <= r0 && 0 <= rt && r0+rt <= rows m &&
      0 <= c0 && 0 <= ct && c0+ct <= cols m = unsafePerformIO $ extractR m 0 (idxs[r0,r0+rt-1]) 0 (idxs[c0,c0+ct-1])
    | otherwise = error $ "wrong subMatrix "++
                          show ((r0,c0),(rt,ct))++" of "++show(rows m)++"x"++ show (cols m)

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
    | otherwise = error $ "matrix " ++ shSize m ++ " cannot be expanded to (" ++ show r ++ "><"++ show c ++")"

conformVTo n v
    | dim v == n = v
    | dim v == 1 = constantD (v@>0) n
    | otherwise = error $ "vector of dim=" ++ show (dim v) ++ " cannot be expanded to dim=" ++ show n

repRows n x = fromRows (replicate n (flatten x))
repCols n x = fromColumns (replicate n (flatten x))

size m = (rows m, cols m)

shSize m = "(" ++ show (rows m) ++"><"++ show (cols m)++")"

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

extractAux f m moder vr modec vc = do
    let nr = if moder == 0 then fromIntegral $ vr@>1 - vr@>0 + 1 else dim vr
        nc = if modec == 0 then fromIntegral $ vc@>1 - vc@>0 + 1 else dim vc
    r <- createMatrix RowMajor nr nc
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

gemmg f u v m1 m2 m3 = f # u # v # m1 # m2 # m3 #|"gemmg"

type Tgemm x = x :> I :> x ::> x ::> x ::> Ok

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

