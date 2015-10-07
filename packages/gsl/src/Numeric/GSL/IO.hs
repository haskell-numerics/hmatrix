-----------------------------------------------------------------------------
-- |
-- Module      :  Numeric.GSL.IO
-- Copyright   :  (c) Alberto Ruiz 2007-14
-- License     :  GPL
-- Maintainer  :  Alberto Ruiz
-- Stability   :  provisional
--
-----------------------------------------------------------------------------

module Numeric.GSL.IO (
    saveMatrix,
    fwriteVector, freadVector, fprintfVector, fscanfVector,
    fileDimensions, loadMatrix, fromFile
) where

import Numeric.LinearAlgebra.HMatrix hiding(saveMatrix, loadMatrix)
import Numeric.GSL.Vector
import System.Process(readProcess)


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

