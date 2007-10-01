-- Principal component analysis

import Numeric.LinearAlgebra
import System.Directory(doesFileExist)
import System(system)
import Control.Monad(when)

type Vec = Vector Double
type Mat = Matrix Double

sumColumns m = constant 1 (rows m) <> m

-- Vec with the mean value of the columns of a Mat
mean x = sumColumns x / fromIntegral (rows x)

-- covariance Mat of a list of observations as rows of a Mat
cov x = (trans xc <> xc) / fromIntegral (rows x -1)
    where xc = center x
          center m = m - constant 1 (rows m) `outer` mean m

-- creates the compression and decompression functions from the desired number of components
pca :: Int -> Mat -> (Vec -> Vec , Vec -> Vec)
pca n dataSet = (encode,decode)
  where    
    encode x = vp <> (x - m)
    decode x = x <> vp + m
    m = mean dataSet
    c = cov dataSet
    (_,v) = eigSH c
    vp = takeRows n (trans v)

main = do
    ok <- doesFileExist ("mnist.txt")
    when (not ok)  $ do
        putStrLn "\nTrying to download test datafile..."
        system("wget -nv http://dis.um.es/~alberto/material/sp/mnist.txt.gz")
        system("gunzip mnist.txt.gz")
        return ()
    m <- fromFile "mnist.txt" (5000,785)
    let xs = takeColumns (cols m -1) m -- the last column is the digit type (class label)
    let x = toRows xs !! 4  -- an arbitrary test Vec
    let (pe,pd) = pca 10 xs
    let y = pe x
    print y  -- compressed version
    print $ norm (x - pd y) / norm x --reconstruction quality
