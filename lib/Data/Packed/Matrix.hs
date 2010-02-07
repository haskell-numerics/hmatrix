-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Packed.Matrix
-- Copyright   :  (c) Alberto Ruiz 2007
-- License     :  GPL-style
--
-- Maintainer  :  Alberto Ruiz <aruiz@um.es>
-- Stability   :  provisional
-- Portability :  portable
--
-- A Matrix representation suitable for numerical computations using LAPACK and GSL.
--
-----------------------------------------------------------------------------

module Data.Packed.Matrix (
    Element,
    Matrix,rows,cols,
    (><),
    trans,
    reshape, flatten,
    fromLists, toLists, buildMatrix,
    (@@>),
    asRow, asColumn,
    fromRows, toRows, fromColumns, toColumns,
    fromBlocks, repmat,
    flipud, fliprl,
    subMatrix, takeRows, dropRows, takeColumns, dropColumns,
    extractRows,
    ident, diag, diagRect, takeDiag,
    liftMatrix, liftMatrix2, liftMatrix2Auto,
    dispf, disps, dispcf, latexFormat, format,
    loadMatrix, saveMatrix, fromFile, fileDimensions,
    readMatrix, fromArray2D
) where

import Data.Packed.Internal
import qualified Data.Packed.ST as ST
import Data.Packed.Vector
import Data.Array
import System.Process(readProcess)
import Text.Printf(printf)
import Data.List(transpose,intersperse)
import Data.Complex

-- | creates a matrix from a vertical list of matrices
joinVert :: Element t => [Matrix t] -> Matrix t
joinVert ms = case common cols ms of
    Nothing -> error "(impossible) joinVert on matrices with different number of columns"
    Just c  -> reshape c $ join (map flatten ms)

-- | creates a matrix from a horizontal list of matrices
joinHoriz :: Element t => [Matrix t] -> Matrix t
joinHoriz ms = trans. joinVert . map trans $ ms

{- | Creates a matrix from blocks given as a list of lists of matrices.

Single row/column components are automatically expanded to match the
corresponding common row and column:

@\> let disp = putStr . dispf 2
\> let vector xs = fromList xs :: Vector Double
\> let diagl = diag . vector
\> let rowm = asRow . vector

\> disp $ fromBlocks [[ident 5, 7, rowm[10,20]], [3, diagl[1,2,3], 0]]

8x10
1  0  0  0  0  7  7  7  10  20
0  1  0  0  0  7  7  7  10  20
0  0  1  0  0  7  7  7  10  20
0  0  0  1  0  7  7  7  10  20
0  0  0  0  1  7  7  7  10  20
3  3  3  3  3  1  0  0   0   0
3  3  3  3  3  0  2  0   0   0
3  3  3  3  3  0  0  3   0   0@
-}
fromBlocks :: Element t => [[Matrix t]] -> Matrix t
fromBlocks = fromBlocksRaw . adaptBlocks

fromBlocksRaw mms = joinVert . map joinHoriz $ mms

adaptBlocks ms = ms' where
    bc = case common length ms of
          Just c -> c
          Nothing -> error "fromBlocks requires rectangular [[Matrix]]"
    rs = map (compatdim . map rows) ms
    cs = map (compatdim . map cols) (transpose ms)
    szs = sequence [rs,cs]
    ms' = splitEvery bc $ zipWith g szs (concat ms)

    g [Just nr,Just nc] m
                | nr == r && nc == c = m
                | r == 1 && c == 1 = reshape nc (constant x (nr*nc))
                | r == 1 = fromRows (replicate nr (flatten m))
                | otherwise = fromColumns (replicate nc (flatten m))
      where
        r = rows m
        c = cols m
        x = m@@>(0,0)
    g _ _ = error "inconsistent dimensions in fromBlocks"

-----------------------------------------------------------

-- | Reverse rows 
flipud :: Element t => Matrix t -> Matrix t
flipud m = fromRows . reverse . toRows $ m

-- | Reverse columns
fliprl :: Element t => Matrix t -> Matrix t
fliprl m = fromColumns . reverse . toColumns $ m

------------------------------------------------------------

-- | Creates a square matrix with a given diagonal.
diag :: Element a => Vector a -> Matrix a
diag v = ST.runSTMatrix $ do
    let d = dim v
    m <- ST.newMatrix 0 d d
    mapM_ (\k -> ST.writeMatrix m k k (v@>k)) [0..d-1]
    return m

{- | creates a rectangular diagonal matrix

@> diagRect (constant 5 3) 3 4 :: Matrix Double
(3><4)
 [ 5.0, 0.0, 0.0, 0.0
 , 0.0, 5.0, 0.0, 0.0
 , 0.0, 0.0, 5.0, 0.0 ]@
-}
diagRect :: (Element t, Num t) => Vector t -> Int -> Int -> Matrix t
diagRect v r c
    | dim v < min r c = error "diagRect called with dim v < min r c"
    | otherwise = ST.runSTMatrix $ do
        m <- ST.newMatrix 0 r c
        let d = min r c
        mapM_ (\k -> ST.writeMatrix m k k (v@>k)) [0..d-1]
        return m

-- | extracts the diagonal from a rectangular matrix
takeDiag :: (Element t) => Matrix t -> Vector t
takeDiag m = fromList [flatten m `at` (k*cols m+k) | k <- [0 .. min (rows m) (cols m) -1]]

-- | creates the identity matrix of given dimension
ident :: Element a => Int -> Matrix a
ident n = diag (constant 1 n)

------------------------------------------------------------

{- | An easy way to create a matrix:

@\> (2><3)[1..6]
(2><3)
 [ 1.0, 2.0, 3.0
 , 4.0, 5.0, 6.0 ]@

This is the format produced by the instances of Show (Matrix a), which
can also be used for input.

The input list is explicitly truncated, so that it can
safely be used with lists that are too long (like infinite lists).

Example:

@\> (2>|<3)[1..]
(2><3)
 [ 1.0, 2.0, 3.0
 , 4.0, 5.0, 6.0 ]@

-}
(><) :: (Element a) => Int -> Int -> [a] -> Matrix a
r >< c = f where
    f l | dim v == r*c = matrixFromVector RowMajor c v
        | otherwise    = error $ "inconsistent list size = "
                                 ++show (dim v) ++" in ("++show r++"><"++show c++")"
        where v = fromList $ take (r*c) l

----------------------------------------------------------------

-- | Creates a matrix with the first n rows of another matrix
takeRows :: Element t => Int -> Matrix t -> Matrix t
takeRows n mt = subMatrix (0,0) (n, cols mt) mt
-- | Creates a copy of a matrix without the first n rows
dropRows :: Element t => Int -> Matrix t -> Matrix t
dropRows n mt = subMatrix (n,0) (rows mt - n, cols mt) mt
-- |Creates a matrix with the first n columns of another matrix
takeColumns :: Element t => Int -> Matrix t -> Matrix t
takeColumns n mt = subMatrix (0,0) (rows mt, n) mt
-- | Creates a copy of a matrix without the first n columns
dropColumns :: Element t => Int -> Matrix t -> Matrix t
dropColumns n mt = subMatrix (0,n) (rows mt, cols mt - n) mt

----------------------------------------------------------------

{- | Creates a 'Matrix' from a list of lists (considered as rows).

@\> fromLists [[1,2],[3,4],[5,6]]
(3><2)
 [ 1.0, 2.0
 , 3.0, 4.0
 , 5.0, 6.0 ]@
-}
fromLists :: Element t => [[t]] -> Matrix t
fromLists = fromRows . map fromList

-- | creates a 1-row matrix from a vector
asRow :: Element a => Vector a -> Matrix a
asRow v = reshape (dim v) v

-- | creates a 1-column matrix from a vector
asColumn :: Element a => Vector a -> Matrix a
asColumn v = reshape 1 v


{- | creates a Matrix of the specified size using the supplied function to
     to map the row/column position to the value at that row/column position.

@> buildMatrix 3 4 (\ (r,c) -> fromIntegral r * fromIntegral c)
(3><4)
 [ 0.0, 0.0, 0.0, 0.0, 0.0
 , 0.0, 1.0, 2.0, 3.0, 4.0
 , 0.0, 2.0, 4.0, 6.0, 8.0]@
-}
buildMatrix :: Element a => Int -> Int -> ((Int, Int) -> a) -> Matrix a
buildMatrix rc cc f =
    fromLists $ map (\x -> map f x)
    	$ map (\ ri -> map (\ ci -> (ri, ci)) [0 .. (cc - 1)]) [0 .. (rc - 1)]

-----------------------------------------------------

fromArray2D :: (Element e) => Array (Int, Int) e -> Matrix e
fromArray2D m = (r><c) (elems m)
    where ((r0,c0),(r1,c1)) = bounds m
          r = r1-r0+1
          c = c1-c0+1


-------------------------------------------------------------------
-- display utilities


{- | Creates a string from a matrix given a separator and a function to show each entry. Using
this function the user can easily define any desired display function:

@import Text.Printf(printf)@

@disp = putStr . format \"  \" (printf \"%.2f\")@

-}
format :: (Element t) => String -> (t -> String) -> Matrix t -> String
format sep f m = table sep . map (map f) . toLists $ m

{- | Show a matrix with \"autoscaling\" and a given number of decimal places.

@disp = putStr . disps 2

\> disp $ 120 * (3><4) [1..]
3x4  E3
 0.12  0.24  0.36  0.48
 0.60  0.72  0.84  0.96
 1.08  1.20  1.32  1.44
@
-}
disps :: Int -> Matrix Double -> String
disps d x = sdims x ++ "  " ++ formatScaled d x

{- | Show a matrix with a given number of decimal places.

@disp = putStr . dispf 3

\> disp (1/3 + ident 4)
4x4
1.333  0.333  0.333  0.333
0.333  1.333  0.333  0.333
0.333  0.333  1.333  0.333
0.333  0.333  0.333  1.333
@
-}
dispf :: Int -> Matrix Double -> String
dispf d x = sdims x ++ "\n" ++ formatFixed (if isInt x then 0 else d) x

sdims x = show (rows x) ++ "x" ++ show (cols x)

formatFixed d x = format "  " (printf ("%."++show d++"f")) $ x

isInt = all lookslikeInt . toList . flatten

formatScaled dec t = "E"++show o++"\n" ++ ss
    where ss = format " " (printf fmt. g) t
          g x | o >= 0    = x/10^(o::Int)
              | otherwise = x*10^(-o)
          o = floor $ maximum $ map (logBase 10 . abs) $ toList $ flatten t
          fmt = '%':show (dec+3) ++ '.':show dec ++"f"

{- | Show a vector using a function for showing matrices.

@disp = putStr . vecdisp (dispf 2)

\> disp (linspace 10 (0,1))
10 |> 0.00  0.11  0.22  0.33  0.44  0.56  0.67  0.78  0.89  1.00
@
-}
vecdisp :: (Element t) => (Matrix t -> String) -> Vector t -> String
vecdisp f v
    = ((show (dim v) ++ " |> ") ++) . (++"\n")
    . unwords . lines .  tail . dropWhile (not . (`elem` " \n"))
    . f . trans . reshape 1
    $ v

-- | Tool to display matrices with latex syntax.
latexFormat :: String -- ^ type of braces: \"matrix\", \"bmatrix\", \"pmatrix\", etc.
            -> String -- ^ Formatted matrix, with elements separated by spaces and newlines
            -> String
latexFormat del tab = "\\begin{"++del++"}\n" ++ f tab ++ "\\end{"++del++"}"
    where f = unlines . intersperse "\\\\" . map unwords . map (intersperse " & " . words) . tail . lines

-- | Pretty print a complex number with at most n decimal digits.
showComplex :: Int -> Complex Double -> String
showComplex d (a:+b)
    | isZero a && isZero b = "0"
    | isZero b = sa
    | isZero a && isOne b = s2++"i"
    | isZero a = sb++"i"
    | isOne b = sa++s3++"i"
    | otherwise = sa++s1++sb++"i"
  where
    sa = shcr d a
    sb = shcr d b
    s1 = if b<0 then "" else "+"
    s2 = if b<0 then "-" else ""
    s3 = if b<0 then "-" else "+"

shcr d a | lookslikeInt a = printf "%.0f" a
         | otherwise      = printf ("%."++show d++"f") a


lookslikeInt x = show (round x :: Int) ++".0" == shx || "-0.0" == shx
   where shx = show x

isZero x = show x `elem` ["0.0","-0.0"]
isOne  x = show x `elem` ["1.0","-1.0"]

-- | Pretty print a complex matrix with at most n decimal digits.
dispcf :: Int -> Matrix (Complex Double) -> String
dispcf d m = sdims m ++ "\n" ++ format "  " (showComplex d) m

--------------------------------------------------------------------

-- | reads a matrix from a string containing a table of numbers.
readMatrix :: String -> Matrix Double
readMatrix = fromLists . map (map read). map words . filter (not.null) . lines

{- |  obtains the number of rows and columns in an ASCII data file
      (provisionally using unix's wc).
-}
fileDimensions :: FilePath -> IO (Int,Int)
fileDimensions fname = do
    wcres <- readProcess "wc" ["-w",fname] ""
    contents <- readFile fname
    let tot = read . head . words $ wcres
        c   = length . head . dropWhile null . map words . lines $ contents
    if tot > 0
        then return (tot `div` c, c)
        else return (0,0)

-- | Loads a matrix from an ASCII file formatted as a 2D table.
loadMatrix :: FilePath -> IO (Matrix Double)
loadMatrix file = fromFile file =<< fileDimensions file

-- | Loads a matrix from an ASCII file (the number of rows and columns must be known in advance).
fromFile :: FilePath -> (Int,Int) -> IO (Matrix Double)
fromFile filename (r,c) = reshape c `fmap` fscanfVector filename (r*c)


-- | rearranges the rows of a matrix according to the order given in a list of integers. 
extractRows :: Element t => [Int] -> Matrix t -> Matrix t
extractRows l m = fromRows $ extract (toRows $ m) l
    where extract l' is = [l'!!i |i<-is]

{- | creates matrix by repetition of a matrix a given number of rows and columns

@> repmat (ident 2) 2 3 :: Matrix Double
(4><6)
 [ 1.0, 0.0, 1.0, 0.0, 1.0, 0.0
 , 0.0, 1.0, 0.0, 1.0, 0.0, 1.0
 , 1.0, 0.0, 1.0, 0.0, 1.0, 0.0
 , 0.0, 1.0, 0.0, 1.0, 0.0, 1.0 ]@

-}
repmat :: (Element t) => Matrix t -> Int -> Int -> Matrix t
repmat m r c = fromBlocks $ splitEvery c $ replicate (r*c) m

-- | A version of 'liftMatrix2' which automatically adapt matrices with a single row or column to match the dimensions of the other matrix.
liftMatrix2Auto :: (Element t, Element a, Element b)
             => (Vector a -> Vector b -> Vector t) -> Matrix a -> Matrix b -> Matrix t
liftMatrix2Auto f m1 m2 | compat' m1 m2 = lM f m1 m2
                     | rows m1 == rows m2 && cols m2 == 1 = lM f m1 (repCols (cols m1) m2)
                     | rows m1 == rows m2 && cols m1 == 1 = lM f (repCols (cols m2) m1) m2
                     | cols m1 == cols m2 && rows m2 == 1 = lM f m1 (repRows (rows m1) m2)
                     | cols m1 == cols m2 && cols m1 == 1 = lM f (repRows (rows m2) m1) m2
                     | rows m1 == 1 && cols m2 == 1 = lM f (repRows (rows m2) m1) (repCols (cols m1) m2)
                     | cols m1 == 1 && rows m2 == 1 = lM f (repCols (cols m2) m1) (repRows (rows m1) m2)
                     | otherwise    = error $ "nonconformable matrices in liftMatrix2Auto: " ++ show (size m1) ++ ", " ++ show (size m2)

size m = (rows m, cols m)

lM f m1 m2 = reshape (max (cols m1) (cols m2)) (f (flatten m1) (flatten m2))

repRows n x = fromRows (replicate n (flatten x))
repCols n x = fromColumns (replicate n (flatten x))

compat' :: Matrix a -> Matrix b -> Bool
compat' m1 m2 = rows m1 == 1 && cols m1 == 1
             || rows m2 == 1 && cols m2 == 1
             || rows m1 == rows m2 && cols m1 == cols m2

