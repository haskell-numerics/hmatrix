{-# OPTIONS_GHC -fglasgow-exts -fallow-undecidable-instances #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Packed.Internal.Matrix
-- Copyright   :  (c) Alberto Ruiz 2007
-- License     :  GPL-style
--
-- Maintainer  :  Alberto Ruiz <aruiz@um.es>
-- Stability   :  provisional
-- Portability :  portable (uses FFI)
--
-- Fundamental types
--
-----------------------------------------------------------------------------

module Data.Packed.Internal.Matrix where

import Data.Packed.Internal.Vector

import Foreign hiding (xor)
import Complex
import Control.Monad(when)
import Debug.Trace
import Data.List(transpose,intersperse)
import Data.Typeable
import Data.Maybe(fromJust)

debug x = trace (show x) x


data MatrixOrder = RowMajor | ColumnMajor deriving (Show,Eq)

-- | 2D array
data Matrix t = M { rows    :: Int
                  , cols    :: Int
                  , dat     :: Vector t
                  , tdat    :: Vector t
                  , isTrans :: Bool
                  , order   :: MatrixOrder
                  } deriving Typeable

xor a b = a && not b || b && not a

fortran m = order m == ColumnMajor

cdat m = if fortran m `xor` isTrans m then tdat m else dat m
fdat m = if fortran m `xor` isTrans m then dat m else tdat m

trans m = m { rows = cols m
            , cols = rows m
            , isTrans = not (isTrans m)
            }

type Mt t s = Int -> Int -> Ptr t -> s
infixr 6 ::>
type t ::> s = Mt t s

mat d m f = f (rows m) (cols m) (ptr (d m))

instance (Show a, Storable a) => (Show (Matrix a)) where
    show m = (sizes++) . dsp . map (map show) . toLists $ m
        where sizes = "("++show (rows m)++"><"++show (cols m)++")\n"

partit :: Int -> [a] -> [[a]]
partit _ [] = []
partit n l  = take n l : partit n (drop n l)

toLists m | fortran m = transpose $ partit (rows m) . toList . dat $ m
          | otherwise = partit (cols m) . toList . dat $ m

dsp as = (++" ]") . (" ["++) . init . drop 2 . unlines . map (" , "++) . map unwords' $ transpose mtp
    where
        mt = transpose as
        longs = map (maximum . map length) mt
        mtp = zipWith (\a b -> map (pad a) b) longs mt
        pad n str = replicate (n - length str) ' ' ++ str
        unwords' = concat . intersperse ", "

matrixFromVector RowMajor c v =
    M { rows = r
      , cols = c
      , dat  = v
      , tdat = transdata c v r
      , order = RowMajor
      , isTrans = False
      } where r = dim v `div` c -- TODO check mod=0

matrixFromVector ColumnMajor c v =
    M { rows = r
      , cols = c
      , dat  = v
      , tdat = transdata r v c
      , order = ColumnMajor
      , isTrans = False
      } where r = dim v `div` c -- TODO check mod=0

createMatrix order r c = do
    p <- createVector (r*c)
    return (matrixFromVector order c p)

transdataG :: Storable a => Int -> Vector a -> Int -> Vector a 
transdataG c1 d c2 = fromList . concat . transpose . partit c1 . toList $ d

transdataR :: Int -> Vector Double -> Int -> Vector Double
transdataR = transdataAux ctransR

transdataC :: Int -> Vector (Complex Double) -> Int -> Vector (Complex Double)
transdataC = transdataAux ctransC

transdataAux fun c1 d c2 =
    if noneed
        then d
        else unsafePerformIO $ do
            v <- createVector (dim d)
            fun r1 c1 (ptr d) r2 c2 (ptr v) // check "transdataAux" [d]
            putStrLn "---> transdataAux"
            return v
  where r1 = dim d `div` c1
        r2 = dim d `div` c2
        noneed = r1 == 1 || c1 == 1

foreign import ccall safe "aux.h transR"
    ctransR :: Double ::> Double ::> IO Int
foreign import ccall safe "aux.h transC"
    ctransC :: Complex Double ::> Complex Double ::> IO Int

transdata :: Field a => Int -> Vector a -> Int -> Vector a
transdata c1 d c2 | isReal baseOf d = scast $ transdataR c1 (scast d) c2
                  | isComp baseOf d = scast $ transdataC c1 (scast d) c2
                  | otherwise       = transdataG c1 d c2

--transdata :: Storable a => Int -> Vector a -> Int -> Vector a 
--transdata = transdataG
--{-# RULES "transdataR" transdata=transdataR #-}
--{-# RULES "transdataC" transdata=transdataC #-}

-- | extracts the rows of a matrix as a list of vectors
toRows :: Storable t => Matrix t -> [Vector t]
toRows m = toRows' 0 where
    v = cdat m
    r = rows m
    c = cols m
    toRows' k | k == r*c  = []
              | otherwise = subVector k c v : toRows' (k+c)

------------------------------------------------------------------

dotL a b = sum (zipWith (*) a b)

multiplyL a b = [[dotL x y | y <- transpose b] | x <- a]

transL m = matrixFromVector RowMajor (rows m) $ transdataG (cols m) (cdat m) (rows m)

multiplyG a b = matrixFromVector RowMajor (cols b) $ fromList $ concat $ multiplyL (toLists a) (toLists b)

------------------------------------------------------------------

gmatC m f | fortran m =
                if (isTrans m)
                    then f 0 (rows m) (cols m) (ptr (dat m))
                    else f 1 (cols m) (rows m) (ptr (dat m))
         | otherwise =
                if isTrans m
                    then f 1 (cols m) (rows m) (ptr (dat m))
                    else f 0 (rows m) (cols m) (ptr (dat m))


multiplyAux order fun a b = unsafePerformIO $ do
    when (cols a /= rows b) $ error $ "inconsistent dimensions in contraction "++
                                      show (rows a,cols a) ++ " x " ++ show (rows b, cols b)
    r <- createMatrix order (rows a) (cols b)
    fun // gmatC a // gmatC b // mat dat r // check "multiplyAux" [dat a, dat b]
    return r

foreign import ccall safe "aux.h multiplyR"
    cmultiplyR :: Int -> Double ::> (Int -> Double ::> (Double ::> IO Int))

foreign import ccall safe "aux.h multiplyC"
    cmultiplyC :: Int -> Complex Double ::> (Int -> Complex Double ::> (Complex Double ::> IO Int))

multiply :: (Num a, Field a) => MatrixOrder -> Matrix a -> Matrix a -> Matrix a
multiply RowMajor a b    = multiplyD RowMajor a b
multiply ColumnMajor a b = trans $ multiplyT ColumnMajor a b

multiplyT order a b = multiplyD order (trans b) (trans a)

multiplyD order a b 
    | isReal (baseOf.dat) a = scast $ multiplyAux order cmultiplyR (scast a) (scast b)
    | isComp (baseOf.dat) a = scast $ multiplyAux order cmultiplyC (scast a) (scast b)
    | otherwise             = multiplyG a b

