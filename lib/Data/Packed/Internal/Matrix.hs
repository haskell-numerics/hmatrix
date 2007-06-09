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

-- | obtains the common value of a property of a list
common :: (Eq a) => (b->a) -> [b] -> Maybe a
common f = commonval . map f where
    commonval :: (Eq a) => [a] -> Maybe a
    commonval [] = Nothing
    commonval [a] = Just a
    commonval (a:b:xs) = if a==b then commonval (b:xs) else Nothing


toLists m = partit (cols m) . toList . cdat $ m

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

reshape c v = matrixFromVector RowMajor c v

singleton x = reshape 1 (fromList [x])

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
            --putStrLn "---> transdataAux"
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

-----------------------------------------------------------------------------

-- | creates a Matrix from a list of vectors
fromRows :: Field t => [Vector t] -> Matrix t
fromRows vs = case common dim vs of
    Nothing -> error "fromRows applied to [] or to vectors with different sizes"
    Just c  -> reshape c (join vs)

-- | extracts the rows of a matrix as a list of vectors
toRows :: Storable t => Matrix t -> [Vector t]
toRows m = toRows' 0 where
    v = cdat m
    r = rows m
    c = cols m
    toRows' k | k == r*c  = []
              | otherwise = subVector k c v : toRows' (k+c)

-- | Creates a matrix from a list of vectors, as columns
fromColumns :: Field t => [Vector t] -> Matrix t
fromColumns m = trans . fromRows $ m

-- | Creates a list of vectors from the columns of a matrix
toColumns :: Field t => Matrix t -> [Vector t]
toColumns m = toRows . trans $ m

-- | creates a matrix from a vertical list of matrices
joinVert :: Field t => [Matrix t] -> Matrix t
joinVert ms = case common cols ms of
    Nothing -> error "joinVert on matrices with different number of columns"
    Just c  -> reshape c $ join (map cdat ms)

-- | creates a matrix from a horizontal list of matrices
joinHoriz :: Field t => [Matrix t] -> Matrix t
joinHoriz ms = trans. joinVert . map trans $ ms

-- | creates a complex vector from vectors with real and imaginary parts
toComplex :: (Vector Double, Vector Double) ->  Vector (Complex Double)
toComplex (r,i) = asComplex $ cdat $ fromColumns [r,i]

-- | obtains the complex conjugate of a complex vector
conj :: Vector (Complex Double) -> Vector (Complex Double)
conj v = asComplex $ cdat $ reshape 2 (asReal v) `mulC` diag (fromList [1,-1])
    where mulC = multiply RowMajor

------------------------------------------------------------------------------

-- | Reverse rows 
flipud :: Field t => Matrix t -> Matrix t
flipud m = fromRows . reverse . toRows $ m

-- | Reverse columns
fliprl :: Field t => Matrix t -> Matrix t
fliprl m = fromColumns . reverse . toColumns $ m

-----------------------------------------------------------------

liftMatrix f m = m { dat = f (dat m), tdat = f (tdat m) } -- check sizes

------------------------------------------------------------------

dotL a b = sum (zipWith (*) a b)

multiplyL a b | ok = [[dotL x y | y <- transpose b] | x <- a]
              | otherwise = error "inconsistent dimensions in contraction "
    where ok = case common length a of
                   Nothing -> False
                   Just c  -> c == length b

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
multiply ColumnMajor a b = m {rows = cols m, cols = rows m, order = ColumnMajor}
    where m = multiplyD RowMajor (trans b) (trans a)

multiplyD order a b
    | isReal (baseOf.dat) a = scast $ multiplyAux order cmultiplyR (scast a) (scast b)
    | isComp (baseOf.dat) a = scast $ multiplyAux order cmultiplyC (scast a) (scast b)
    | otherwise             = multiplyG a b

----------------------------------------------------------------------

outer u v = dat (multiply RowMajor r c)
    where r = matrixFromVector RowMajor 1 u
          c = matrixFromVector RowMajor (dim v) v

dot u v = dat (multiply RowMajor r c) `at` 0
    where r = matrixFromVector RowMajor (dim u) u
          c = matrixFromVector RowMajor 1 v

----------------------------------------------------------------------

-- | extraction of a submatrix of a real matrix
subMatrixR :: (Int,Int) -- ^ (r0,c0) starting position 
           -> (Int,Int) -- ^ (rt,ct) dimensions of submatrix
           -> Matrix Double -> Matrix Double
subMatrixR (r0,c0) (rt,ct) x = unsafePerformIO $ do
    r <- createMatrix RowMajor rt ct
    c_submatrixR r0 (r0+rt-1) c0 (c0+ct-1) // mat cdat x // mat cdat r // check "subMatrixR" [dat r]
    return r
foreign import ccall "aux.h submatrixR"
    c_submatrixR :: Int -> Int -> Int -> Int -> Double ::> Double ::> IO Int

-- | extraction of a submatrix of a complex matrix
subMatrixC :: (Int,Int) -- ^ (r0,c0) starting position
           -> (Int,Int) -- ^ (rt,ct) dimensions of submatrix
           -> Matrix (Complex Double) -> Matrix (Complex Double)
subMatrixC (r0,c0) (rt,ct) x =
    reshape ct . asComplex . cdat .
    subMatrixR (r0,2*c0) (rt,2*ct) .
    reshape (2*cols x) . asReal . cdat $ x

subMatrix :: (Field a) 
          => (Int,Int) -- ^ (r0,c0) starting position 
          -> (Int,Int) -- ^ (rt,ct) dimensions of submatrix
          -> Matrix a -> Matrix a
subMatrix st sz m
    | isReal (baseOf.dat) m = scast $ subMatrixR st sz (scast m)
    | isComp (baseOf.dat) m = scast $ subMatrixC st sz (scast m)
    | otherwise             = subMatrixG st sz m

subMatrixG (r0,c0) (rt,ct) x = reshape ct $ fromList $ concat $ map (subList c0 ct) (subList r0 rt (toLists x))
    where subList s n = take n . drop s

---------------------------------------------------------------------

diagAux fun msg (v@V {dim = n}) = unsafePerformIO $ do
    m <- createMatrix RowMajor n n
    fun // vec v // mat dat m // check msg [dat m]
    return m {tdat = dat m}

-- | diagonal matrix from a real vector
diagR :: Vector Double -> Matrix Double
diagR = diagAux c_diagR "diagR"
foreign import ccall "aux.h diagR" c_diagR :: Double :> Double ::> IO Int

-- | diagonal matrix from a real vector
diagC :: Vector (Complex Double) -> Matrix (Complex Double)
diagC = diagAux c_diagC "diagC"
foreign import ccall "aux.h diagC" c_diagC :: (Complex Double) :> (Complex Double) ::> IO Int

-- | diagonal matrix from a vector
diag :: (Num a, Field a) => Vector a -> Matrix a
diag v
    | isReal (baseOf) v = scast $ diagR (scast v)
    | isComp (baseOf) v = scast $ diagC (scast v)
    | otherwise             = diagG v

diagG v = reshape c $ fromList $ [ l!!(i-1) * delta k i | k <- [1..c], i <- [1..c]]
    where c = dim v
          l = toList v
          delta i j | i==j      = 1
                    | otherwise = 0

diagRect s r c
    | dim s < min r c = error "diagRect"
    | r == c    = diag s
    | r < c     = trans $ diagRect s c r
    | r > c     = joinVert  [diag s , zeros (r-c,c)]
    where zeros (r,c) = reshape c $ constant (r*c) 0
