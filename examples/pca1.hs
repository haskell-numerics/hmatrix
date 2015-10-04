-- Principal component analysis

import Numeric.LinearAlgebra
import System.Directory(doesFileExist)
import System.Process(system)
import Control.Monad(when)

type Vec = Vector Double
type Mat = Matrix Double

{-
-- Vector with the mean value of the columns of a matrix
mean a = constant (recip . fromIntegral . rows $ a) (rows a) <> a

-- covariance matrix of a list of observations stored as rows
cov x = (trans xc <> xc) / fromIntegral (rows x - 1)
    where xc = x - asRow (mean x)
-}


-- creates the compression and decompression functions from the desired number of components
pca :: Int -> Mat -> (Vec -> Vec , Vec -> Vec)
pca n dataSet = (encode,decode)
  where
    encode x = vp #> (x - m)
    decode x = x <# vp + m
    (m,c) = meanCov dataSet
    (_,v) = eigSH (trustSym c)
    vp = tr $ takeColumns n v

main = do
    ok <- doesFileExist ("mnist.txt")
    when (not ok)  $ do
        putStrLn "\nTrying to download test datafile..."
        system("wget -nv http://dis.um.es/~alberto/material/sp/mnist.txt.gz")
        system("gunzip mnist.txt.gz")
        return ()
    m <- loadMatrix "mnist.txt" -- fromFile "mnist.txt" (5000,785)
    let xs = takeColumns (cols m -1) m -- the last column is the digit type (class label)
    let x = toRows xs !! 4  -- an arbitrary test Vec
    let (pe,pd) = pca 10 xs
    let y = pe x
    print y  -- compressed version
    print $ norm_2 (x - pd y) / norm_2 x --reconstruction quality
