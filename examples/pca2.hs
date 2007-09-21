-- Improved PCA, including illustrative graphics

import LinearAlgebra
import Graphics.Plot
import System.Directory(doesFileExist)
import System(system)
import Control.Monad(when)

type Vec = Vector Double
type Mat = Matrix Double

sumColumns m = constant 1 (rows m) <> m

-- Vector with the mean value of the columns of a Mat
mean x = sumColumns x / fromIntegral (rows x)

-- covariance matrix of a list of observations as rows of a matrix
cov x = (trans xc <> xc) / fromIntegral (rows x -1) 
    where xc = center x
          center m = m - constant 1 (rows m) `outer` mean m

type Stat = (Vec, [Double], Mat)
-- 1st and 2nd order statistics of a dataset (mean, eigenvalues and eigenvectors of cov)  
stat :: Mat -> Stat
stat x = (m, toList s, trans v) where   
    m = mean x
    (s,v) = eigS (cov x)   

-- creates the compression and decompression functions from the desired reconstruction 
-- quality and the statistics of a data set
pca :: Double -> Stat -> (Vec -> Vec , Vec -> Vec)
pca prec (m,s,v) = (encode,decode)    
  where    
    encode x = vp <> (x - m)
    decode x = x <> vp + m
    vp = takeRows n v    
    n = 1 + (length $ fst $ span (< (prec'*sum s)) $ cumSum s)
    cumSum = tail . scanl (+) 0.0     
    prec' = if prec <=0.0 || prec >= 1.0
                then error "the precision in pca must be 0<prec<1"
                else prec

shdigit :: Vec -> IO ()
shdigit v = imshow (reshape 28 (-v))

-- shows the effect of a given reconstruction quality on a test vector
test :: Stat -> Double -> Vec -> IO ()
test st prec x = do
    let (pe,pd) = pca prec st
    let y = pe x
    print $ dim y
    shdigit (pd y)    

main = do
    ok <- doesFileExist ("mnist.txt")
    when (not ok)  $ do
        putStrLn "\nTrying to download test datafile..."
        system("wget -nv http://dis.um.es/~alberto/material/sp/mnist.txt.gz")
        system("gunzip mnist.txt.gz")
        return ()
    m <- fromFile "mnist.txt" (5000,785)
    let xs = takeColumns (cols m -1) m
    let x = toRows xs !! 4  -- an arbitrary test vector
    shdigit x
    let st = stat xs
    test st 0.90 x
    test st 0.50 x
