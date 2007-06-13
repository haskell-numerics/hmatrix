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
    Matrix(rows,cols), Field,
    toLists, (><), (>|<),
    trans,
    reshape,
    fromRows, toRows, fromColumns, toColumns,
    joinVert, joinHoriz,
    flipud, fliprl,
    liftMatrix, liftMatrix2,
    multiply,
    subMatrix, diag, takeDiag, diagRect, ident
) where

import Data.Packed.Internal

-- | creates a matrix from a vertical list of matrices
joinVert :: Field t => [Matrix t] -> Matrix t
joinVert ms = case common cols ms of
    Nothing -> error "joinVert on matrices with different number of columns"
    Just c  -> reshape c $ join (map cdat ms)

-- | creates a matrix from a horizontal list of matrices
joinHoriz :: Field t => [Matrix t] -> Matrix t
joinHoriz ms = trans. joinVert . map trans $ ms

-- | Reverse rows 
flipud :: Field t => Matrix t -> Matrix t
flipud m = fromRows . reverse . toRows $ m

-- | Reverse columns
fliprl :: Field t => Matrix t -> Matrix t
fliprl m = fromColumns . reverse . toColumns $ m

------------------------------------------------------------

diagRect s r c
    | dim s < min r c = error "diagRect"
    | r == c    = diag s
    | r < c     = trans $ diagRect s c r
    | r > c     = joinVert  [diag s , zeros (r-c,c)]
    where zeros (r,c) = reshape c $ constant (r*c) 0

takeDiag m = fromList [cdat m `at` (k*cols m+k) | k <- [0 .. min (rows m) (cols m) -1]]

ident n = diag (constant n 1)

r >< c = f where
    f l | dim v == r*c = matrixFromVector RowMajor c v
        | otherwise    = error $ "inconsistent list size = "
                                 ++show (dim v) ++"in ("++show r++"><"++show c++")"
        where v = fromList l

r >|< c = f where
    f l | dim v == r*c = matrixFromVector ColumnMajor c v
        | otherwise    = error $ "inconsistent list size = "
                                 ++show (dim v) ++"in ("++show r++"><"++show c++")"
        where v = fromList l
