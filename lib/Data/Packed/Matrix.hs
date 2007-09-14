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
-- Matrices
--
-----------------------------------------------------------------------------

module Data.Packed.Matrix (
    Field,
    Matrix,rows,cols,
    (><), reshape, flatten,
    fromLists, toLists,
    (@@>),
    trans, conjTrans,
    asRow, asColumn,
    fromRows, toRows, fromColumns, toColumns,
    fromBlocks, joinVert, joinHoriz,
    flipud, fliprl,
    subMatrix, takeRows, dropRows, takeColumns, dropColumns,
    diag, takeDiag, diagRect, ident,
    liftMatrix, liftMatrix2,
) where

import Data.Packed.Internal
import Foreign(Storable)
import Complex
import Data.Packed.Vector

-- | creates a matrix from a vertical list of matrices
joinVert :: Field t => [Matrix t] -> Matrix t
joinVert ms = case common cols ms of
    Nothing -> error "joinVert on matrices with different number of columns"
    Just c  -> reshape c $ join (map cdat ms)

-- | creates a matrix from a horizontal list of matrices
joinHoriz :: Field t => [Matrix t] -> Matrix t
joinHoriz ms = trans. joinVert . map trans $ ms

{- | Creates a matrix from blocks given as a list of lists of matrices:

@\> let a = 'diag' $ 'fromList' [5,7,2]
\> let b = 'reshape' 4 $ 'constant' (-1) 12
\> fromBlocks [[a,b],[b,a]]
(6><7)
 [  5.0,  0.0,  0.0, -1.0, -1.0, -1.0, -1.0
 ,  0.0,  7.0,  0.0, -1.0, -1.0, -1.0, -1.0
 ,  0.0,  0.0,  2.0, -1.0, -1.0, -1.0, -1.0
 , -1.0, -1.0, -1.0, -1.0,  5.0,  0.0,  0.0
 , -1.0, -1.0, -1.0, -1.0,  0.0,  7.0,  0.0
 , -1.0, -1.0, -1.0, -1.0,  0.0,  0.0,  2.0 ]@
-}
fromBlocks :: Field t => [[Matrix t]] -> Matrix t
fromBlocks = joinVert . map joinHoriz 

-- | Reverse rows 
flipud :: Field t => Matrix t -> Matrix t
flipud m = fromRows . reverse . toRows $ m

-- | Reverse columns
fliprl :: Field t => Matrix t -> Matrix t
fliprl m = fromColumns . reverse . toColumns $ m

------------------------------------------------------------

diagRect :: (Field t, Num t) => Vector t -> Int -> Int -> Matrix t
diagRect s r c
    | dim s < min r c = error "diagRect"
    | r == c    = diag s
    | r < c     = trans $ diagRect s c r
    | r > c     = joinVert  [diag s , zeros (r-c,c)]
    where zeros (r,c) = reshape c $ constantD 0 (r*c)

takeDiag :: (Field t) => Matrix t -> Vector t
takeDiag m = fromList [cdat m `at` (k*cols m+k) | k <- [0 .. min (rows m) (cols m) -1]]

ident :: Int -> Matrix Double
ident n = diag (constant 1 n)

------------------------------------------------------------

{- | An easy way to create a matrix:

@\> (2><3)[1..6]
(2><3)
 [ 1.0, 2.0, 3.0
 , 4.0, 5.0, 6.0 ]@

This is the format produced by the instances of Show (Matrix a), which
can also be used for input.
-}
(><) :: (Field a) => Int -> Int -> [a] -> Matrix a
r >< c = f where
    f l | dim v == r*c = matrixFromVector RowMajor c v
        | otherwise    = error $ "inconsistent list size = "
                                 ++show (dim v) ++" in ("++show r++"><"++show c++")"
        where v = fromList l

----------------------------------------------------------------

-- | Creates a matrix with the first n rows of another matrix
takeRows :: Field t => Int -> Matrix t -> Matrix t
takeRows n mat = subMatrix (0,0) (n, cols mat) mat
-- | Creates a copy of a matrix without the first n rows
dropRows :: Field t => Int -> Matrix t -> Matrix t
dropRows n mat = subMatrix (n,0) (rows mat - n, cols mat) mat
-- |Creates a matrix with the first n columns of another matrix
takeColumns :: Field t => Int -> Matrix t -> Matrix t
takeColumns n mat = subMatrix (0,0) (rows mat, n) mat
-- | Creates a copy of a matrix without the first n columns
dropColumns :: Field t => Int -> Matrix t -> Matrix t
dropColumns n mat = subMatrix (0,n) (rows mat, cols mat - n) mat

----------------------------------------------------------------

{- | Creates a vector by concatenation of rows

@\> flatten ('ident' 3)
9 |> [1.0,0.0,0.0,0.0,1.0,0.0,0.0,0.0,1.0]@
-}
flatten :: Field t => Matrix t -> Vector t
flatten = cdat

{- | Creates a 'Matrix' from a list of lists (considered as rows).

@\> fromLists [[1,2],[3,4],[5,6]]
(3><2)
 [ 1.0, 2.0
 , 3.0, 4.0
 , 5.0, 6.0 ]@
-}
fromLists :: Field t => [[t]] -> Matrix t
fromLists = fromRows . map fromList

conjTrans :: Matrix (Complex Double) -> Matrix (Complex Double)
conjTrans = trans . liftMatrix conj

asRow :: Field a => Vector a -> Matrix a
asRow v = reshape (dim v) v

asColumn :: Field a => Vector a -> Matrix a
asColumn v = reshape 1 v

------------------------------------------------

