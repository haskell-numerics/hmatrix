{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Packed.Matrix
-- Copyright   :  (c) Alberto Ruiz 2007-10
-- License     :  BSD3
-- Maintainer  :  Alberto Ruiz
-- Stability   :  provisional
--
-- A Matrix representation suitable for numerical computations using LAPACK and GSL.
--
-- This module provides basic functions for manipulation of structure.

-----------------------------------------------------------------------------

module Internal.Element where

import Internal.Vector
import Internal.Matrix
import Internal.Vectorized
import qualified Internal.ST as ST
import Data.Array
import Text.Printf
import Data.List(transpose,intersperse)
import Data.List.Split(chunksOf)
import Foreign.Storable(Storable)
import System.IO.Unsafe(unsafePerformIO)
import Control.Monad(liftM)
import Foreign.C.Types(CInt)

-------------------------------------------------------------------


import Data.Binary

instance (Binary a, Element a) => Binary (Matrix a) where
    put m = do
            put (cols m)
            put (flatten m)
    get = do
          c <- get
          v <- get
          return (reshape c v)


-------------------------------------------------------------------

instance (Show a, Element a) => (Show (Matrix a)) where
    show m | rows m == 0 || cols m == 0 = sizes m ++" []"
    show m = (sizes m++) . dsp . map (map show) . toLists $ m

sizes :: Matrix t -> [Char]
sizes m = "("++show (rows m)++"><"++show (cols m)++")\n"

dsp :: [[[Char]]] -> [Char]
dsp as = (++" ]") . (" ["++) . init . drop 2 . unlines . map (" , "++) . map unwords' $ transpose mtp
    where
        mt = transpose as
        longs = map (maximum . map length) mt
        mtp = zipWith (\a b -> map (pad a) b) longs mt
        pad n str = replicate (n - length str) ' ' ++ str
        unwords' = concat . intersperse ", "

------------------------------------------------------------------

instance (Element a, Read a) => Read (Matrix a) where
    readsPrec _ s = [((rs><cs) . read $ listnums, rest)]
        where (thing,rest) = breakAt ']' s
              (dims,listnums) = breakAt ')' thing
              cs = read . init . fst. breakAt ')' . snd . breakAt '<' $ dims
              rs = read . snd . breakAt '(' .init . fst . breakAt '>' $ dims


breakAt :: Eq a => a -> [a] -> ([a], [a])
breakAt c l = (a++[c],tail b) where
    (a,b) = break (==c) l

--------------------------------------------------------------------------------
-- | Specification of indexes for the operator '??'.
data Extractor
    = All
    | Range Int Int Int
    | Pos (Vector I)
    | PosCyc (Vector I)
    | Take Int
    | TakeLast Int
    | Drop Int
    | DropLast Int
  deriving Show

ppext :: Extractor -> [Char]
ppext All = ":"
ppext (Range a 1 c) = printf "%d:%d" a c
ppext (Range a b c) = printf "%d:%d:%d" a b c
ppext (Pos v) = show (toList v)
ppext (PosCyc v) = "Cyclic"++show (toList v)
ppext (Take n) = printf "Take %d" n
ppext (Drop n) = printf "Drop %d" n
ppext (TakeLast n) = printf "TakeLast %d" n
ppext (DropLast n) = printf "DropLast %d" n

{- | General matrix slicing.

>>> m
(4><5)
 [  0,  1,  2,  3,  4
 ,  5,  6,  7,  8,  9
 , 10, 11, 12, 13, 14
 , 15, 16, 17, 18, 19 ]

>>> m ?? (Take 3, DropLast 2)
(3><3)
 [  0,  1,  2
 ,  5,  6,  7
 , 10, 11, 12 ]

>>> m ?? (Pos (idxs[2,1]), All)
(2><5)
 [ 10, 11, 12, 13, 14
 ,  5,  6,  7,  8,  9 ]

>>> m ?? (PosCyc (idxs[-7,80]), Range 4 (-2) 0)
(2><3)
 [ 9, 7, 5
 , 4, 2, 0 ]

-}
infixl 9 ??
(??)  :: Element t => Matrix t -> (Extractor,Extractor) -> Matrix t

minEl :: Vector CInt -> CInt
minEl = toScalarI Min
maxEl :: Vector CInt -> CInt
maxEl = toScalarI Max
cmodi :: Foreign.C.Types.CInt -> Vector Foreign.C.Types.CInt -> Vector Foreign.C.Types.CInt
cmodi = vectorMapValI ModVS

extractError :: Matrix t1 -> (Extractor, Extractor) -> t
extractError m (e1,e2)= error $ printf "can't extract (%s,%s) from matrix %dx%d" (ppext e1::String) (ppext e2::String) (rows m) (cols m)

m ?? (Range a s b,e) | s /= 1 = m ?? (Pos (idxs [a,a+s .. b]), e)
m ?? (e,Range a s b) | s /= 1 = m ?? (e, Pos (idxs [a,a+s .. b]))

m ?? e@(Range a _ b,_) | a < 0 || b >= rows m = extractError m e
m ?? e@(_,Range a _ b) | a < 0 || b >= cols m = extractError m e

m ?? e@(Pos vs,_) | dim vs>0 && (minEl vs < 0 || maxEl vs >= fi (rows m)) = extractError m e
m ?? e@(_,Pos vs) | dim vs>0 && (minEl vs < 0 || maxEl vs >= fi (cols m)) = extractError m e

m ?? (All,All) = m

m ?? (Range a _ b,e) | a > b = m ?? (Take 0,e)
m ?? (e,Range a _ b) | a > b = m ?? (e,Take 0)

m ?? (Take n,e)
    | n <= 0      = (0><cols m) [] ?? (All,e)
    | n >= rows m =              m ?? (All,e)

m ?? (e,Take n)
    | n <= 0      = (rows m><0) [] ?? (e,All)
    | n >= cols m =              m ?? (e,All)

m ?? (Drop n,e)
    | n <= 0      =              m ?? (All,e)
    | n >= rows m = (0><cols m) [] ?? (All,e)

m ?? (e,Drop n)
    | n <= 0      =              m ?? (e,All)
    | n >= cols m = (rows m><0) [] ?? (e,All)

m ?? (TakeLast n, e) = m ?? (Drop (rows m - n), e)
m ?? (e, TakeLast n) = m ?? (e, Drop (cols m - n))

m ?? (DropLast n, e) = m ?? (Take (rows m - n), e)
m ?? (e, DropLast n) = m ?? (e, Take (cols m - n))

m ?? (er,ec) = unsafePerformIO $ extractR (orderOf m) m moder rs modec cs
  where
    (moder,rs) = mkExt (rows m) er
    (modec,cs) = mkExt (cols m) ec
    ran a b = (0, idxs [a,b])
    pos ks  = (1, ks)
    mkExt _ (Pos  ks)     = pos ks
    mkExt n (PosCyc ks)
        | n == 0          = mkExt n (Take 0)
        | otherwise       = pos (cmodi (fi n) ks)
    mkExt _ (Range mn _ mx) = ran mn mx
    mkExt _ (Take k)      = ran 0 (k-1)
    mkExt n (Drop k)      = ran k (n-1)
    mkExt n _             = ran 0 (n-1) -- All

--------------------------------------------------------------------------------

-- | obtains the common value of a property of a list
common :: (Eq a) => (b->a) -> [b] -> Maybe a
common f = commonval . map f
  where
    commonval :: (Eq a) => [a] -> Maybe a
    commonval [] = Nothing
    commonval [a] = Just a
    commonval (a:b:xs) = if a==b then commonval (b:xs) else Nothing


-- | creates a matrix from a vertical list of matrices
joinVert :: Element t => [Matrix t] -> Matrix t
joinVert [] = emptyM 0 0
joinVert ms = case common cols ms of
    Nothing -> error "(impossible) joinVert on matrices with different number of columns"
    Just c  -> matrixFromVector RowMajor (sum (map rows ms)) c $ vjoin (map flatten ms)

-- | creates a matrix from a horizontal list of matrices
joinHoriz :: Element t => [Matrix t] -> Matrix t
joinHoriz ms = trans. joinVert . map trans $ ms

{- | Create a matrix from blocks given as a list of lists of matrices.

Single row-column components are automatically expanded to match the
corresponding common row and column:

@
disp = putStr . dispf 2
@

>>> disp $ fromBlocks [[ident 5, 7, row[10,20]], [3, diagl[1,2,3], 0]]
8x10
1  0  0  0  0  7  7  7  10  20
0  1  0  0  0  7  7  7  10  20
0  0  1  0  0  7  7  7  10  20
0  0  0  1  0  7  7  7  10  20
0  0  0  0  1  7  7  7  10  20
3  3  3  3  3  1  0  0   0   0
3  3  3  3  3  0  2  0   0   0
3  3  3  3  3  0  0  3   0   0

-}
fromBlocks :: Element t => [[Matrix t]] -> Matrix t
fromBlocks = fromBlocksRaw . adaptBlocks

fromBlocksRaw :: Element t => [[Matrix t]] -> Matrix t
fromBlocksRaw mms = joinVert . map joinHoriz $ mms

adaptBlocks :: Element t => [[Matrix t]] -> [[Matrix t]]
adaptBlocks ms = ms' where
    bc = case common length ms of
          Just c -> c
          Nothing -> error "fromBlocks requires rectangular [[Matrix]]"
    rs = map (compatdim . map rows) ms
    cs = map (compatdim . map cols) (transpose ms)
    szs = sequence [rs,cs]
    ms' = chunksOf bc $ zipWith g szs (concat ms)

    g [Just nr,Just nc] m
                | nr == r && nc == c = m
                | r == 1 && c == 1 = matrixFromVector RowMajor nr nc (constantD x (nr*nc))
                | r == 1 = fromRows (replicate nr (flatten m))
                | otherwise = fromColumns (replicate nc (flatten m))
      where
        r = rows m
        c = cols m
        x = m@@>(0,0)
    g _ _ = error "inconsistent dimensions in fromBlocks"


--------------------------------------------------------------------------------

{- | create a block diagonal matrix

>>>  disp 2 $ diagBlock [konst 1 (2,2), konst 2 (3,5), col [5,7]]
7x8
1  1  0  0  0  0  0  0
1  1  0  0  0  0  0  0
0  0  2  2  2  2  2  0
0  0  2  2  2  2  2  0
0  0  2  2  2  2  2  0
0  0  0  0  0  0  0  5
0  0  0  0  0  0  0  7

>>> diagBlock [(0><4)[], konst 2 (2,3)]  :: Matrix Double
(2><7)
 [ 0.0, 0.0, 0.0, 0.0, 2.0, 2.0, 2.0
 , 0.0, 0.0, 0.0, 0.0, 2.0, 2.0, 2.0 ]

-}
diagBlock :: (Element t, Num t) => [Matrix t] -> Matrix t
diagBlock ms = fromBlocks $ zipWith f ms [0..]
  where
    f m k = take n $ replicate k z ++ m : repeat z
    n = length ms
    z = (1><1) [0]

--------------------------------------------------------------------------------


-- | Reverse rows
flipud :: Element t => Matrix t -> Matrix t
flipud m = extractRows [r-1,r-2 .. 0] $ m
  where
    r = rows m

-- | Reverse columns
fliprl :: Element t => Matrix t -> Matrix t
fliprl m = extractColumns [c-1,c-2 .. 0] $ m
  where
    c = cols m

------------------------------------------------------------

{- | creates a rectangular diagonal matrix:

>>> diagRect 7 (fromList [10,20,30]) 4 5 :: Matrix Double
(4><5)
 [ 10.0,  7.0,  7.0, 7.0, 7.0
 ,  7.0, 20.0,  7.0, 7.0, 7.0
 ,  7.0,  7.0, 30.0, 7.0, 7.0
 ,  7.0,  7.0,  7.0, 7.0, 7.0 ]

-}
diagRect :: (Storable t) => t -> Vector t -> Int -> Int -> Matrix t
diagRect z v r c = ST.runSTMatrix $ do
        m <- ST.newMatrix z r c
        let d = min r c `min` (dim v)
        mapM_ (\k -> ST.writeMatrix m k k (v@>k)) [0..d-1]
        return m

-- | extracts the diagonal from a rectangular matrix
takeDiag :: (Element t) => Matrix t -> Vector t
takeDiag m = fromList [flatten m @> (k*cols m+k) | k <- [0 .. min (rows m) (cols m) -1]]

------------------------------------------------------------

{- | Create a matrix from a list of elements

>>> (2><3) [2, 4, 7+2*iC,   -3, 11, 0]
(2><3)
 [       2.0 :+ 0.0,  4.0 :+ 0.0, 7.0 :+ 2.0
 , (-3.0) :+ (-0.0), 11.0 :+ 0.0, 0.0 :+ 0.0 ]

The input list is explicitly truncated, so that it can
safely be used with lists that are too long (like infinite lists).

>>> (2><3)[1..]
(2><3)
 [ 1.0, 2.0, 3.0
 , 4.0, 5.0, 6.0 ]

This is the format produced by the instances of Show (Matrix a), which
can also be used for input.

-}
(><) :: (Storable a) => Int -> Int -> [a] -> Matrix a
r >< c = f where
    f l | dim v == r*c = matrixFromVector RowMajor r c v
        | otherwise    = error $ "inconsistent list size = "
                                 ++show (dim v) ++" in ("++show r++"><"++show c++")"
        where v = fromList $ take (r*c) l

----------------------------------------------------------------

takeRows :: Element t => Int -> Matrix t -> Matrix t
takeRows n mt = subMatrix (0,0) (n, cols mt) mt

-- | Creates a matrix with the last n rows of another matrix
takeLastRows :: Element t => Int -> Matrix t -> Matrix t
takeLastRows n mt = subMatrix (rows mt - n, 0) (n, cols mt) mt

dropRows :: Element t => Int -> Matrix t -> Matrix t
dropRows n mt = subMatrix (n,0) (rows mt - n, cols mt) mt

-- | Creates a copy of a matrix without the last n rows
dropLastRows :: Element t => Int -> Matrix t -> Matrix t
dropLastRows n mt = subMatrix (0,0) (rows mt - n, cols mt) mt

takeColumns :: Element t => Int -> Matrix t -> Matrix t
takeColumns n mt = subMatrix (0,0) (rows mt, n) mt

-- |Creates a matrix with the last n columns of another matrix
takeLastColumns :: Element t => Int -> Matrix t -> Matrix t
takeLastColumns n mt = subMatrix (0, cols mt - n) (rows mt, n) mt

dropColumns :: Element t => Int -> Matrix t -> Matrix t
dropColumns n mt = subMatrix (0,n) (rows mt, cols mt - n) mt

-- | Creates a copy of a matrix without the last n columns
dropLastColumns :: Element t => Int -> Matrix t -> Matrix t
dropLastColumns n mt = subMatrix (0,0) (rows mt, cols mt - n) mt

----------------------------------------------------------------

{- | Creates a 'Matrix' from a list of lists (considered as rows).

>>> fromLists [[1,2],[3,4],[5,6]]
(3><2)
 [ 1.0, 2.0
 , 3.0, 4.0
 , 5.0, 6.0 ]

-}
fromLists :: Element t => [[t]] -> Matrix t
fromLists = fromRows . map fromList

-- | creates a 1-row matrix from a vector
--
-- >>> asRow (fromList [1..5])
--  (1><5)
--   [ 1.0, 2.0, 3.0, 4.0, 5.0 ]
--
asRow :: Storable a => Vector a -> Matrix a
asRow = trans . asColumn

-- | creates a 1-column matrix from a vector
--
-- >>> asColumn (fromList [1..5])
-- (5><1)
--  [ 1.0
--  , 2.0
--  , 3.0
--  , 4.0
--  , 5.0 ]
--
asColumn :: Storable a => Vector a -> Matrix a
asColumn v = reshape 1 v



{- | creates a Matrix of the specified size using the supplied function to
     to map the row\/column position to the value at that row\/column position.

@> buildMatrix 3 4 (\\(r,c) -> fromIntegral r * fromIntegral c)
(3><4)
 [ 0.0, 0.0, 0.0, 0.0, 0.0
 , 0.0, 1.0, 2.0, 3.0, 4.0
 , 0.0, 2.0, 4.0, 6.0, 8.0]@

Hilbert matrix of order N:

@hilb n = buildMatrix n n (\\(i,j)->1/(fromIntegral i + fromIntegral j +1))@

-}
buildMatrix :: Element a => Int -> Int -> ((Int, Int) -> a) -> Matrix a
buildMatrix rc cc f =
    fromLists $ map (map f)
        $ map (\ ri -> map (\ ci -> (ri, ci)) [0 .. (cc - 1)]) [0 .. (rc - 1)]

-----------------------------------------------------

fromArray2D :: (Storable e) => Array (Int, Int) e -> Matrix e
fromArray2D m = (r><c) (elems m)
    where ((r0,c0),(r1,c1)) = bounds m
          r = r1-r0+1
          c = c1-c0+1


-- | rearranges the rows of a matrix according to the order given in a list of integers.
extractRows :: Element t => [Int] -> Matrix t -> Matrix t
extractRows l m = m ?? (Pos (idxs l), All)

-- | rearranges the rows of a matrix according to the order given in a list of integers.
extractColumns :: Element t => [Int] -> Matrix t -> Matrix t
extractColumns l m = m ?? (All, Pos (idxs l))


{- | creates matrix by repetition of a matrix a given number of rows and columns

>>> repmat (ident 2) 2 3
(4><6)
 [ 1.0, 0.0, 1.0, 0.0, 1.0, 0.0
 , 0.0, 1.0, 0.0, 1.0, 0.0, 1.0
 , 1.0, 0.0, 1.0, 0.0, 1.0, 0.0
 , 0.0, 1.0, 0.0, 1.0, 0.0, 1.0 ]

-}
repmat :: (Element t) => Matrix t -> Int -> Int -> Matrix t
repmat m r c
    | r == 0 || c == 0 = emptyM (r*rows m) (c*cols m)
    | otherwise = fromBlocks $ replicate r $ replicate c $ m

-- | A version of 'liftMatrix2' which automatically adapt matrices with a single row or column to match the dimensions of the other matrix.
liftMatrix2Auto :: (Element t, Element a, Element b) => (Vector a -> Vector b -> Vector t) -> Matrix a -> Matrix b -> Matrix t
liftMatrix2Auto f m1 m2
    | compat' m1 m2 = lM f m1  m2
    | ok            = lM f m1' m2'
    | otherwise = error $ "nonconformable matrices in liftMatrix2Auto: " ++ shSize m1 ++ ", " ++ shSize m2
  where
    (r1,c1) = size m1
    (r2,c2) = size m2
    r = max r1 r2
    c = max c1 c2
    r0 = min r1 r2
    c0 = min c1 c2
    ok = r0 == 1 || r1 == r2 && c0 == 1 || c1 == c2
    m1' = conformMTo (r,c) m1
    m2' = conformMTo (r,c) m2

-- FIXME do not flatten if equal order
lM :: (Storable t, Element t1, Element t2)
   => (Vector t1 -> Vector t2 -> Vector t)
   -> Matrix t1 -> Matrix t2 -> Matrix t
lM f m1 m2 = matrixFromVector
                RowMajor
                (max' (rows m1) (rows m2))
                (max' (cols m1) (cols m2))
                (f (flatten m1) (flatten m2))
  where
    max' 1 b = b
    max' a 1 = a
    max' a b = max a b

compat' :: Matrix a -> Matrix b -> Bool
compat' m1 m2 = s1 == (1,1) || s2 == (1,1) || s1 == s2
  where
    s1 = size m1
    s2 = size m2

------------------------------------------------------------

toBlockRows :: Element t => [Int] -> Matrix t -> [Matrix t]
toBlockRows [r] m
    | r == rows m = [m]
toBlockRows rs m
    | cols m > 0 = map (reshape (cols m)) (takesV szs (flatten m))
    | otherwise = map g rs
  where
    szs = map (* cols m) rs
    g k = (k><0)[]

toBlockCols :: Element t => [Int] -> Matrix t -> [Matrix t]
toBlockCols [c] m | c == cols m = [m]
toBlockCols cs m = map trans . toBlockRows cs . trans $ m

-- | Partition a matrix into blocks with the given numbers of rows and columns.
-- The remaining rows and columns are discarded.
toBlocks :: (Element t) => [Int] -> [Int] -> Matrix t -> [[Matrix t]]
toBlocks rs cs m
    | ok = map (toBlockCols cs) . toBlockRows rs $ m
    | otherwise = error $ "toBlocks: bad partition: "++show rs++" "++show cs
                          ++ " "++shSize m
  where
    ok = sum rs <= rows m && sum cs <= cols m && all (>=0) rs && all (>=0) cs

-- | Fully partition a matrix into blocks of the same size. If the dimensions are not
-- a multiple of the given size the last blocks will be smaller.
toBlocksEvery :: (Element t) => Int -> Int -> Matrix t -> [[Matrix t]]
toBlocksEvery r c m
    | r < 1 || c < 1 = error $ "toBlocksEvery expects block sizes > 0, given "++show r++" and "++ show c
    | otherwise = toBlocks rs cs m
  where
    (qr,rr) = rows m `divMod` r
    (qc,rc) = cols m `divMod` c
    rs = replicate qr r ++ if rr > 0 then [rr] else []
    cs = replicate qc c ++ if rc > 0 then [rc] else []

-------------------------------------------------------------------

-- Given a column number and a function taking matrix indexes, returns
-- a function which takes vector indexes (that can be used on the
-- flattened matrix).
mk :: Int -> ((Int, Int) -> t) -> (Int -> t)
mk c g = \k -> g (divMod k c)

{- |

>>> mapMatrixWithIndexM_ (\(i,j) v -> printf "m[%d,%d] = %.f\n" i j v :: IO()) ((2><3)[1 :: Double ..])
m[0,0] = 1
m[0,1] = 2
m[0,2] = 3
m[1,0] = 4
m[1,1] = 5
m[1,2] = 6

-}
mapMatrixWithIndexM_
  :: (Element a, Num a, Monad m) =>
      ((Int, Int) -> a -> m ()) -> Matrix a -> m ()
mapMatrixWithIndexM_ g m = mapVectorWithIndexM_ (mk c g) . flatten $ m
  where
    c = cols m

{- |

>>> mapMatrixWithIndexM (\(i,j) v -> Just $ 100*v + 10*fromIntegral i + fromIntegral j) (ident 3:: Matrix Double)
Just (3><3)
 [ 100.0,   1.0,   2.0
 ,  10.0, 111.0,  12.0
 ,  20.0,  21.0, 122.0 ]

-}
mapMatrixWithIndexM
  :: (Element a, Storable b, Monad m) =>
      ((Int, Int) -> a -> m b) -> Matrix a -> m (Matrix b)
mapMatrixWithIndexM g m = liftM (reshape c) . mapVectorWithIndexM (mk c g) . flatten $ m
    where
      c = cols m

{- |

>>> mapMatrixWithIndex (\(i,j) v -> 100*v + 10*fromIntegral i + fromIntegral j) (ident 3:: Matrix Double)
(3><3)
 [ 100.0,   1.0,   2.0
 ,  10.0, 111.0,  12.0
 ,  20.0,  21.0, 122.0 ]

 -}
mapMatrixWithIndex
  :: (Element a, Storable b) =>
      ((Int, Int) -> a -> b) -> Matrix a -> Matrix b
mapMatrixWithIndex g m = reshape c . mapVectorWithIndex (mk c g) . flatten $ m
    where
      c = cols m

mapMatrix :: (Element a, Element b) => (a -> b) -> Matrix a -> Matrix b
mapMatrix f = liftMatrix (mapVector f)
