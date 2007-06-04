{-# OPTIONS_GHC -fglasgow-exts -fallow-undecidable-instances #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Packed.Internal
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

module Data.Packed.Internal where

import Foreign hiding (xor)
import Complex
import Control.Monad(when)
import Debug.Trace
import Data.List(transpose,intersperse)
import Data.Typeable
import Data.Maybe(fromJust)

debug x = trace (show x) x

----------------------------------------------------------------------
instance (Storable a, RealFloat a) => Storable (Complex a) where    --
    alignment x = alignment (realPart x)                            --
    sizeOf x    = 2 * sizeOf (realPart x)                           --
    peek p = do                                                     --
        [re,im] <- peekArray 2 (castPtr p)                          --
        return (re :+ im)                                           --
    poke p (a :+ b) = pokeArray (castPtr p) [a,b]                   --
----------------------------------------------------------------------

(//) :: x -> (x -> y) -> y
infixl 0 //
(//) = flip ($)

check msg ls f = do
    err <- f
    when (err/=0) (error msg)
    mapM_ (touchForeignPtr . fptr) ls
    return ()

----------------------------------------------------------------------

data Vector t = V { dim  :: Int
                  , fptr :: ForeignPtr t
                  , ptr  :: Ptr t
                  } deriving Typeable

type Vc t s = Int -> Ptr t -> s
infixr 5 :>
type t :> s = Vc t s

vec :: Vector t -> (Vc t s) -> s
vec v f = f (dim v) (ptr v)

createVector :: Storable a => Int -> IO (Vector a)
createVector n = do
    when (n <= 0) $ error ("trying to createVector of dim "++show n)
    fp <- mallocForeignPtrArray n
    let p = unsafeForeignPtrToPtr fp
    --putStrLn ("\n---------> V"++show n)
    return $ V n fp p

fromList :: Storable a => [a] -> Vector a
fromList l = unsafePerformIO $ do
    v <- createVector (length l)
    let f _ p = pokeArray p l >> return 0
    f // vec v // check "fromList" []
    return v

toList :: Storable a => Vector a -> [a]
toList v = unsafePerformIO $ peekArray (dim v) (ptr v)

n # l = if length l == n then fromList l else error "# with wrong size"

at' :: Storable a => Vector a -> Int -> a
at' v n = unsafePerformIO $ peekElemOff (ptr v) n

at :: Storable a => Vector a -> Int -> a
at v n | n >= 0 && n < dim v = at' v n
       | otherwise          = error "vector index out of range"

instance (Show a, Storable a) => (Show (Vector a)) where
    show v = (show (dim v))++" # " ++ show (toList v)

------------------------------------------------------------------------

data MatrixOrder = RowMajor | ColumnMajor deriving (Show,Eq)

-- | 2D array
data Matrix t = M { rows     :: Int
                  , cols     :: Int
                  , cmat     :: Vector t
                  , fmat     :: Vector t
                  , isTrans  :: Bool
                  , order    :: MatrixOrder
                  } deriving Typeable

xor a b = a && not b || b && not a

fortran m = order m == ColumnMajor

dat m = if fortran m `xor` isTrans m then fmat m else cmat m

pref m = if fortran m then fmat m else cmat m

trans m = m { rows = cols m
            , cols = rows m
            , isTrans = not (isTrans m)
            }

type Mt t s = Int -> Int -> Ptr t -> s
infixr 6 ::>
type t ::> s = Mt t s

mat :: Matrix t -> (Mt t s) -> s
mat m f = f (rows m) (cols m) (ptr (dat m))

gmat m f | fortran m =
                if (isTrans m)
                    then f 0 (rows m) (cols m) (ptr (fmat m))
                    else f 1 (cols m) (rows m) (ptr (fmat m))
         | otherwise =
                if isTrans m
                    then f 1 (cols m) (rows m) (ptr (cmat m))
                    else f 0 (rows m) (cols m) (ptr (cmat m))

instance (Show a, Storable a) => (Show (Matrix a)) where
    show m = (sizes++) . dsp . map (map show) . toLists $ m
        where sizes = "("++show (rows m)++"><"++show (cols m)++")\n"

partit :: Int -> [a] -> [[a]]
partit _ [] = []
partit n l  = take n l : partit n (drop n l)

toLists m = partit (cols m) . toList . cmat $ m

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
      , cmat  = v
      , fmat = transdata c v r
      , order = RowMajor
      , isTrans = False
      } where r = dim v `div` c -- TODO check mod=0

matrixFromVector ColumnMajor c v =
    M { rows = r
      , cols = c
      , fmat  = v
      , cmat = transdata c v r
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

transdataAux fun c1 d c2 = unsafePerformIO $ do
    v <- createVector (dim d)
    let r1 = dim d `div` c1
        r2 = dim d `div` c2
    fun r1 c1 (ptr d) r2 c2 (ptr v) // check "transdataAux" [d]
    --putStrLn "---> transdataAux"
    return v

foreign import ccall safe "aux.h transR"
    ctransR :: Double ::> Double ::> IO Int
foreign import ccall safe "aux.h transC"
    ctransC :: Complex Double ::> Complex Double ::> IO Int


class (Storable a, Typeable a) => Field a where
instance (Storable a, Typeable a) => Field a where

isReal w x   = typeOf (undefined :: Double) == typeOf (w x)
isComp w x = typeOf (undefined :: Complex Double) == typeOf (w x)
baseOf v = (v `at` 0)

scast :: forall a . forall b . (Typeable a, Typeable b) => a -> b
scast = fromJust . cast

transdata :: Field a => Int -> Vector a -> Int -> Vector a
transdata c1 d c2 | isReal baseOf d = scast $ transdataR c1 (scast d) c2
                  | isComp baseOf d = scast $ transdataC c1 (scast d) c2
                  | otherwise       = transdataG c1 d c2

--transdata :: Storable a => Int -> Vector a -> Int -> Vector a 
--transdata = transdataG
--{-# RULES "transdataR" transdata=transdataR #-}
--{-# RULES "transdataC" transdata=transdataC #-}

------------------------------------------------------------------

constantG n x = fromList (replicate n x)

constantR :: Int -> Double -> Vector Double
constantR = constantAux cconstantR

constantC :: Int -> Complex Double -> Vector (Complex Double)
constantC = constantAux cconstantC

constantAux fun n x = unsafePerformIO $ do
    v <- createVector n
    px <- newArray [x]
    fun px // vec v // check "constantAux" []
    free px
    return v

foreign import ccall safe "aux.h constantR"
    cconstantR :: Ptr Double -> Double :> IO Int

foreign import ccall safe "aux.h constantC"
    cconstantC :: Ptr (Complex Double) -> Complex Double :> IO Int

constant :: Field a => Int -> a -> Vector a
constant n x | isReal id x = scast $ constantR n (scast x)
             | isComp id x = scast $ constantC n (scast x)
             | otherwise   = constantG n x

------------------------------------------------------------------

dotL a b = sum (zipWith (*) a b)

multiplyL a b = [[dotL x y | y <- transpose b] | x <- a]

transL m = m {rows = cols m, cols = rows m, cmat = v, fmat = cmat m}
    where v = transdataG (cols m) (cmat m) (rows m)

------------------------------------------------------------------

multiplyG a b = matrixFromVector RowMajor (cols b) $ fromList $ concat $ multiplyL (toLists a) (toLists b)

multiplyAux order fun a b = unsafePerformIO $ do
    when (cols a /= rows b) $ error $ "inconsistent dimensions in contraction "++
                                      show (rows a,cols a) ++ " x " ++ show (rows b, cols b)
    r <- createMatrix order (rows a) (cols b)
    fun // gmat a // gmat b // mat r // check "multiplyAux" [pref a, pref b]
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

--------------------------------------------------------------------

data IdxTp = Covariant | Contravariant

-- | multidimensional array
data Tensor t = T { rank   :: Int
                  , dims   :: [Int]
                  , idxNm  :: [String]
                  , idxTp  :: [IdxTp]
                  , ten    :: Vector t
                  }

