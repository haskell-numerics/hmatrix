{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}

{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-----------------------------------------------------------------------------
{- |
Module      :  Internal.Util
Copyright   :  (c) Alberto Ruiz 2013
License     :  BSD3
Maintainer  :  Alberto Ruiz
Stability   :  provisional

-}
-----------------------------------------------------------------------------

module Internal.Util(

    -- * Convenience functions
    vector, matrix,
    disp,
    formatSparse,
    approxInt,
    dispDots,
    dispBlanks,
    formatShort,
    dispShort,
    zeros, ones,
    diagl,
    row,
    col,
    (&), (¦), (|||), (——), (===),
    (?), (¿),
    Indexable(..), size,
    Numeric,
    rand, randn,
    cross,
    norm,
    ℕ,ℤ,ℝ,ℂ,iC,
    Normed(..), norm_Frob, norm_nuclear,
    magnit,
    normalize,
    mt,
    (~!~),
    pairwiseD2,
    rowOuters,
    null1,
    null1sym,
    -- * Convolution
    -- ** 1D
    corr, conv, corrMin,
    -- ** 2D
    corr2, conv2, separable,
    block2x2,block3x3,view1,unView1,foldMatrix,
    gaussElim_1, gaussElim_2, gaussElim,
    luST, luSolve', luSolve'', luPacked', luPacked'',
    invershur
) where

import Internal.Vector
import Internal.Matrix hiding (size)
import Internal.Numeric
import Internal.Element
import Internal.Container
import Internal.Vectorized
import Internal.IO
import Internal.Algorithms hiding (Normed,linearSolve',luSolve', luPacked')
import Numeric.Matrix()
import Numeric.Vector()
import Internal.Random
import Internal.Convolution
import Control.Monad(when,forM_)
import Text.Printf
import Data.List.Split(splitOn)
import Data.List(intercalate,sortBy,foldl')
import Control.Arrow((&&&),(***))
import Data.Complex
import Data.Function(on)
import Internal.ST
#if MIN_VERSION_base(4,11,0)
import Prelude hiding ((<>))
#endif

type ℝ = Double
type ℕ = Int
type ℤ = Int
type ℂ = Complex Double

-- | imaginary unit
iC :: C
iC = 0:+1

{- | Create a real vector.

>>> vector [1..5]
[1.0,2.0,3.0,4.0,5.0]
it :: Vector R

-}
vector :: [R] -> Vector R
vector = fromList

{- | Create a real matrix.

>>> matrix 5 [1..15]
(3><5)
 [  1.0,  2.0,  3.0,  4.0,  5.0
 ,  6.0,  7.0,  8.0,  9.0, 10.0
 , 11.0, 12.0, 13.0, 14.0, 15.0 ]

-}
matrix
  :: Int -- ^ number of columns
  -> [R] -- ^ elements in row order
  -> Matrix R
matrix c = reshape c . fromList


{- | print a real matrix with given number of digits after the decimal point

>>> disp 5 $ ident 2 / 3
2x2
0.33333  0.00000
0.00000  0.33333

-}
disp :: Int -> Matrix Double -> IO ()

disp n = putStr . dispf n


{- | create a real diagonal matrix from a list

>>> diagl [1,2,3]
(3><3)
 [ 1.0, 0.0, 0.0
 , 0.0, 2.0, 0.0
 , 0.0, 0.0, 3.0 ]

-}
diagl :: [Double] -> Matrix Double
diagl = diag . fromList

-- | a real matrix of zeros
zeros :: Int -- ^ rows
      -> Int -- ^ columns
      -> Matrix Double
zeros r c = konst 0 (r,c)

-- | a real matrix of ones
ones :: Int -- ^ rows
     -> Int -- ^ columns
     -> Matrix Double
ones r c = konst 1 (r,c)

-- | concatenation of real vectors
infixl 3 &
(&) :: Vector Double -> Vector Double -> Vector Double
a & b = vjoin [a,b]

{- | horizontal concatenation

>>> ident 3 ||| konst 7 (3,4)
(3><7)
 [ 1.0, 0.0, 0.0, 7.0, 7.0, 7.0, 7.0
 , 0.0, 1.0, 0.0, 7.0, 7.0, 7.0, 7.0
 , 0.0, 0.0, 1.0, 7.0, 7.0, 7.0, 7.0 ]

-}
infixl 3 |||
(|||) :: Element t => Matrix t -> Matrix t -> Matrix t
a ||| b = fromBlocks [[a,b]]

-- | a synonym for ('|||') (unicode 0x00a6, broken bar)
infixl 3 ¦
(¦) :: Matrix Double -> Matrix Double -> Matrix Double
(¦) = (|||)


-- | vertical concatenation
--
(===) :: Element t => Matrix t -> Matrix t -> Matrix t
infixl 2 ===
a === b = fromBlocks [[a],[b]]

-- | a synonym for ('===') (unicode 0x2014, em dash)
(——) :: Matrix Double -> Matrix Double -> Matrix Double
infixl 2 ——
(——) = (===)


-- | create a single row real matrix from a list
--
-- >>> row [2,3,1,8]
-- (1><4)
--  [ 2.0, 3.0, 1.0, 8.0 ]
--
row :: [Double] -> Matrix Double
row = asRow . fromList

-- | create a single column real matrix from a list
--
-- >>> col [7,-2,4]
-- (3><1)
--  [  7.0
--  , -2.0
--  ,  4.0 ]
--
col :: [Double] -> Matrix Double
col = asColumn . fromList

{- | extract rows

>>> (20><4) [1..] ? [2,1,1]
(3><4)
 [ 9.0, 10.0, 11.0, 12.0
 , 5.0,  6.0,  7.0,  8.0
 , 5.0,  6.0,  7.0,  8.0 ]

-}
infixl 9 ?
(?) :: Element t => Matrix t -> [Int] -> Matrix t
(?) = flip extractRows

{- | extract columns

(unicode 0x00bf, inverted question mark, Alt-Gr ?)

>>> (3><4) [1..] ¿ [3,0]
(3><2)
 [  4.0, 1.0
 ,  8.0, 5.0
 , 12.0, 9.0 ]

-}
infixl 9 ¿
(¿) :: Element t => Matrix t -> [Int] -> Matrix t
(¿)= flip extractColumns


cross :: Product t => Vector t -> Vector t -> Vector t
-- ^ cross product (for three-element vectors)
cross x y | dim x == 3 && dim y == 3 = fromList [z1,z2,z3]
          | otherwise = error $ "the cross product requires 3-element vectors (sizes given: "
                                ++show (dim x)++" and "++show (dim y)++")"
  where
    [x1,x2,x3] = toList x
    [y1,y2,y3] = toList y
    z1 = x2*y3-x3*y2
    z2 = x3*y1-x1*y3
    z3 = x1*y2-x2*y1

{-# SPECIALIZE cross :: Vector Double -> Vector Double -> Vector Double #-}
{-# SPECIALIZE cross :: Vector (Complex Double) -> Vector (Complex Double) -> Vector (Complex Double) #-}

norm :: Vector Double -> Double
-- ^ 2-norm of real vector
norm = pnorm PNorm2

-- | p-norm for vectors, operator norm for matrices
class Normed a
  where
    norm_0   :: a -> R
    norm_1   :: a -> R
    norm_2   :: a -> R
    norm_Inf :: a -> R


instance Normed (Vector R)
  where
    norm_0 v = sumElements (step (abs v - scalar (eps*normInf v)))
    norm_1 = pnorm PNorm1
    norm_2 = pnorm PNorm2
    norm_Inf = pnorm Infinity

instance Normed (Vector C)
  where
    norm_0 v = sumElements (step (fst (fromComplex (abs v)) - scalar (eps*normInf v)))
    norm_1 = pnorm PNorm1
    norm_2 = pnorm PNorm2
    norm_Inf = pnorm Infinity

instance Normed (Matrix R)
  where
    norm_0 = norm_0 . flatten
    norm_1 = pnorm PNorm1
    norm_2 = pnorm PNorm2
    norm_Inf = pnorm Infinity

instance Normed (Matrix C)
  where
    norm_0 = norm_0 . flatten
    norm_1 = pnorm PNorm1
    norm_2 = pnorm PNorm2
    norm_Inf = pnorm Infinity

instance Normed (Vector I)
  where
    norm_0 = fromIntegral . sumElements . step . abs
    norm_1 = fromIntegral . norm1
    norm_2 v = sqrt . fromIntegral $ dot v v
    norm_Inf = fromIntegral . normInf

instance Normed (Vector Z)
  where
    norm_0 = fromIntegral . sumElements . step . abs
    norm_1 = fromIntegral . norm1
    norm_2 v = sqrt . fromIntegral $ dot v v
    norm_Inf = fromIntegral . normInf

instance Normed (Vector Float)
  where
    norm_0 = norm_0 . double
    norm_1 = norm_1 . double
    norm_2 = norm_2 . double
    norm_Inf = norm_Inf . double

instance Normed (Vector (Complex Float))
  where
    norm_0 = norm_0 . double
    norm_1 = norm_1 . double
    norm_2 = norm_2 . double
    norm_Inf = norm_Inf . double

-- | Frobenius norm (Schatten p-norm with p=2)
norm_Frob :: (Normed (Vector t), Element t) => Matrix t -> R
norm_Frob = norm_2 . flatten

-- | Sum of singular values (Schatten p-norm with p=1)
norm_nuclear :: Field t => Matrix t -> R
norm_nuclear = sumElements . singularValues

{- | Check if the absolute value or complex magnitude is greater than a given threshold

>>> magnit 1E-6 (1E-12 :: R)
False
>>> magnit 1E-6 (3+iC :: C)
True
>>> magnit 0 (3 :: I ./. 5)
True

-}
magnit :: (Element t, Normed (Vector t)) => R -> t -> Bool
magnit e x = norm_1 (fromList [x]) > e


-- | Obtains a vector in the same direction with 2-norm=1
normalize :: (Normed (Vector t), Num (Vector t), Field t) => Vector t -> Vector t
normalize v = v / real (scalar (norm_2 v))


-- | trans . inv
mt :: Matrix Double -> Matrix Double
mt = trans . inv

--------------------------------------------------------------------------------
{- |

>>> size $ vector [1..10]
10
>>> size $ (2><5)[1..10::Double]
(2,5)

-}
size :: Container c t => c t -> IndexOf c
size = size'

{- | Alternative indexing function.

>>> vector [1..10] ! 3
4.0

On a matrix it gets the k-th row as a vector:

>>> matrix 5 [1..15] ! 1
[6.0,7.0,8.0,9.0,10.0]
it :: Vector Double

>>> matrix 5 [1..15] ! 1 ! 3
9.0

-}
class Indexable c t | c -> t , t -> c
  where
    infixl 9 !
    (!) :: c -> Int -> t

instance Indexable (Vector Double) Double
  where
    (!) = (@>)

instance Indexable (Vector Float) Float
  where
    (!) = (@>)

instance Indexable (Vector I) I
  where
    (!) = (@>)

instance Indexable (Vector Z) Z
  where
    (!) = (@>)

instance Indexable (Vector (Complex Double)) (Complex Double)
  where
    (!) = (@>)

instance Indexable (Vector (Complex Float)) (Complex Float)
  where
    (!) = (@>)

instance Element t => Indexable (Matrix t) (Vector t)
  where
    m ! j = subVector (j*c) c (flatten m)
      where
        c = cols m

--------------------------------------------------------------------------------

-- | Matrix of pairwise squared distances of row vectors
-- (using the matrix product trick in blog.smola.org)
pairwiseD2 :: Matrix Double -> Matrix Double -> Matrix Double
pairwiseD2 x y | ok = x2 `outer` oy + ox `outer` y2 - 2* x <> trans y
               | otherwise = error $ "pairwiseD2 with different number of columns: "
                                   ++ show (size x) ++ ", " ++ show (size y)
  where
    ox = one (rows x)
    oy = one (rows y)
    oc = one (cols x)
    one k = konst 1 k
    x2 = x * x <> oc
    y2 = y * y <> oc
    ok = cols x == cols y

--------------------------------------------------------------------------------

{- | outer products of rows

>>> a
(3><2)
 [   1.0,   2.0
 ,  10.0,  20.0
 , 100.0, 200.0 ]
>>> b
(3><3)
 [ 1.0, 2.0, 3.0
 , 4.0, 5.0, 6.0
 , 7.0, 8.0, 9.0 ]

>>> rowOuters a (b ||| 1)
(3><8)
 [   1.0,   2.0,   3.0,   1.0,    2.0,    4.0,    6.0,   2.0
 ,  40.0,  50.0,  60.0,  10.0,   80.0,  100.0,  120.0,  20.0
 , 700.0, 800.0, 900.0, 100.0, 1400.0, 1600.0, 1800.0, 200.0 ]

-}
rowOuters :: Matrix Double -> Matrix Double -> Matrix Double
rowOuters a b = a' * b'
  where
    a' = kronecker a (ones 1 (cols b))
    b' = kronecker (ones 1 (cols a)) b

--------------------------------------------------------------------------------

-- | solution of overconstrained homogeneous linear system
null1 :: Matrix R -> Vector R
null1 = last . toColumns . snd . rightSV

-- | solution of overconstrained homogeneous symmetric linear system
null1sym :: Herm R -> Vector R
null1sym = last . toColumns . snd . eigSH

--------------------------------------------------------------------------------

infixl 0 ~!~
c ~!~ msg = when c (error msg)

--------------------------------------------------------------------------------

formatSparse :: String -> String -> String -> Int -> Matrix Double -> String

formatSparse zeroI _zeroF sep _ (approxInt -> Just m) = format sep f m
  where
    f 0 = zeroI
    f x = printf "%.0f" x

formatSparse zeroI zeroF sep n m = format sep f m
  where
    f x | abs (x::Double) < 2*peps = zeroI++zeroF
        | abs (fromIntegral (round x::Int) - x) / abs x < 2*peps
            = printf ("%.0f."++replicate n ' ') x
        | otherwise = printf ("%."++show n++"f") x

approxInt m
    | norm_Inf (v - vi) < 2*peps * norm_Inf v = Just (reshape (cols m) vi)
    | otherwise = Nothing
  where
    v = flatten m
    vi = roundVector v

dispDots n = putStr . formatSparse "." (replicate n ' ') "  " n

dispBlanks n = putStr . formatSparse "" "" "  " n

formatShort sep fmt maxr maxc m = auxm4
  where
    (rm,cm) = size m
    (r1,r2,r3)
        | rm <= maxr = (rm,0,0)
        | otherwise  = (maxr-3,rm-maxr+1,2)
    (c1,c2,c3)
        | cm <= maxc = (cm,0,0)
        | otherwise  = (maxc-3,cm-maxc+1,2)
    [ [a,_,b]
     ,[_,_,_]
     ,[c,_,d]] = toBlocks [r1,r2,r3]
                          [c1,c2,c3] m
    auxm = fromBlocks [[a,b],[c,d]]
    auxm2
        | cm > maxc = format "|" fmt auxm
        | otherwise = format sep fmt auxm
    auxm3
        | cm > maxc = map (f . splitOn "|") (lines auxm2)
        | otherwise = (lines auxm2)
    f items = intercalate sep (take (maxc-3) items) ++ "  .. " ++
              intercalate sep (drop (maxc-3) items)
    auxm4
        | rm > maxr = unlines (take (maxr-3) auxm3 ++ vsep : drop (maxr-3) auxm3)
        | otherwise = unlines auxm3
    vsep = map g (head auxm3)
    g '.' = ':'
    g _ = ' '


dispShort :: Int -> Int -> Int -> Matrix Double -> IO ()
dispShort maxr maxc dec m =
    printf "%dx%d\n%s" (rows m) (cols m) (formatShort "  " fmt maxr maxc m)
  where
    fmt = printf ("%."++show dec ++"f")

--------------------------------------------------------------------------------

-- matrix views

block2x2 r c m = [[m11,m12],[m21,m22]]
  where
    m11 = m ?? (Take r, Take c)
    m12 = m ?? (Take r, Drop c)
    m21 = m ?? (Drop r, Take c)
    m22 = m ?? (Drop r, Drop c)

block3x3 r nr c nc m = [[m ?? (er !! i, ec !! j) | j <- [0..2] ] | i <- [0..2] ]
  where
    er = [ Range 0 1 (r-1), Range r 1 (r+nr-1), Drop (nr+r) ]
    ec = [ Range 0 1 (c-1), Range c 1 (c+nc-1), Drop (nc+c) ]

view1 :: Numeric t => Matrix t -> Maybe (View1 t)
view1 m
    | rows m > 0 && cols m > 0 = Just (e, flatten m12, flatten m21 , m22)
    | otherwise = Nothing
  where
    [[m11,m12],[m21,m22]] = block2x2 1 1 m
    e = m11 `atIndex` (0, 0)

unView1 :: Numeric t => View1 t -> Matrix t
unView1 (e,r,c,m) = fromBlocks [[scalar e, asRow r],[asColumn c, m]]

type View1 t = (t, Vector t, Vector t, Matrix t)

foldMatrix :: Numeric t => (Matrix t -> Matrix t) -> (View1 t -> View1 t) -> (Matrix t -> Matrix t)
foldMatrix g f ( (f <$>) . view1 . g -> Just (e,r,c,m)) = unView1 (e, r, c, foldMatrix g f m)
foldMatrix _ _ m = m


swapMax k m
    | rows m > 0 && j>0 = (j, m ?? (Pos (idxs swapped), All))
    | otherwise  = (0,m)
  where
    j = maxIndex $ abs (tr m ! k)
    swapped = j:[1..j-1] ++ 0:[j+1..rows m-1]

down g a = foldMatrix g f a
  where
    f (e,r,c,m)
        | e /= 0    = (1, r', 0, m - outer c r')
        | otherwise = error "singular!"
      where
        r' = r / scalar e


-- | generic reference implementation of gaussian elimination
--
-- @a <> gaussElim a b = b@
--
gaussElim_2
  :: (Eq t, Fractional t, Num (Vector t), Numeric t)
  => Matrix t -> Matrix t -> Matrix t

gaussElim_2 a b = flipudrl r
  where
    flipudrl = flipud . fliprl
    splitColsAt n = (takeColumns n &&& dropColumns n)
    go f x y = splitColsAt (cols a) (down f $ x ||| y)
    (a1,b1) = go (snd . swapMax 0) a b
    ( _, r) = go id (flipudrl $ a1) (flipudrl $ b1)

--------------------------------------------------------------------------------

gaussElim_1
  :: (Fractional t, Num (Vector t), Ord t, Indexable (Vector t) t, Numeric t)
  => Matrix t -> Matrix t -> Matrix t

gaussElim_1 x y = dropColumns (rows x) (flipud $ fromRows s2)
  where
    rs = toRows $ x ||| y
    s1 = fromRows $ pivotDown (rows x) 0 rs      -- interesting
    s2 = pivotUp (rows x-1) (toRows $ flipud s1)

pivotDown
  :: forall t . (Fractional t, Num (Vector t), Ord t, Indexable (Vector t) t, Numeric t)
  => Int -> Int -> [Vector t] -> [Vector t]
pivotDown t n xs
    | t == n    = []
    | otherwise = y : pivotDown t (n+1) ys
  where
    y:ys = redu (pivot n xs)

    pivot k = (const k &&& id)
            . sortBy (flip compare `on` (abs. (!k)))

    redu :: (Int, [Vector t]) -> [Vector t]
    redu (k,x:zs)
        | p == 0 = error "gauss: singular!"  -- FIXME
        | otherwise = u : map f zs
      where
        p = x!k
        u = scale (recip (x!k)) x
        f z = z - scale (z!k) u
    redu (_,[]) = []


pivotUp
  :: forall t . (Fractional t, Num (Vector t), Ord t, Indexable (Vector t) t, Numeric t)
  => Int -> [Vector t] -> [Vector t]
pivotUp n xs
    | n == -1 = []
    | otherwise = y : pivotUp (n-1) ys
  where
    y:ys = redu' (n,xs)

    redu' :: (Int, [Vector t]) -> [Vector t]
    redu' (k,x:zs) = u : map f zs
      where
        u = x
        f z = z - scale (z!k) u
    redu' (_,[]) = []

--------------------------------------------------------------------------------

gaussElim a b = dropColumns (rows a) $ fst $ mutable gaussST (a ||| b)

gaussST (r,_) x = do
    let n = r-1
        axpy m a i j = rowOper (AXPY a i j AllCols)     m
        swap m i j   = rowOper (SWAP i j AllCols)       m
        scal m a i   = rowOper (SCAL a (Row i) AllCols) m
    forM_ [0..n] $ \i -> do
        c <- maxIndex . abs . flatten <$> extractMatrix x (FromRow i) (Col i)
        swap x i (i+c)
        a <- readMatrix x i i
        when (a == 0) $ error "singular!"
        scal x (recip a) i
        forM_ [i+1..n] $ \j -> do
            b <- readMatrix x j i
            axpy x (-b) i j
    forM_ [n,n-1..1] $ \i -> do
        forM_ [i-1,i-2..0] $ \j -> do
            b <- readMatrix x j i
            axpy x (-b) i j



luST ok (r,_) x = do
    let axpy m a i j = rowOper (AXPY a i j (FromCol (i+1))) m
        swap m i j   = rowOper (SWAP i j AllCols)           m
    p <- newUndefinedVector r
    forM_ [0..r-1] $ \i -> do
        k <- maxIndex . abs . flatten <$> extractMatrix x (FromRow i) (Col i)
        writeVector p i (k+i)
        swap x i (i+k)
        a <- readMatrix x i i
        when (ok a) $ do
            forM_ [i+1..r-1] $ \j -> do
                b <- (/a) <$> readMatrix x j i
                axpy x (-b) i j
                writeMatrix x j i b
    v <- unsafeFreezeVector p
    return (toList v)

{- | Experimental implementation of 'luPacked'
     for any Fractional element type, including 'Mod' n 'I' and 'Mod' n 'Z'.

>>> let m = ident 5 + (5><5) [0..] :: Matrix (Z ./. 17)
(5><5)
 [  1,  1,  2,  3,  4
 ,  5,  7,  7,  8,  9
 , 10, 11, 13, 13, 14
 , 15, 16,  0,  2,  2
 ,  3,  4,  5,  6,  8 ]

>>> let (l,u,p,s) = luFact $ luPacked' m
>>> l
(5><5)
 [  1,  0, 0,  0, 0
 ,  6,  1, 0,  0, 0
 , 12,  7, 1,  0, 0
 ,  7, 10, 7,  1, 0
 ,  8,  2, 6, 11, 1 ]
>>> u
(5><5)
 [ 15, 16,  0,  2,  2
 ,  0, 13,  7, 13, 14
 ,  0,  0, 15,  0, 11
 ,  0,  0,  0, 15, 15
 ,  0,  0,  0,  0,  1 ]

-}
luPacked' x = LU m p
  where
    (m,p) = mutable (luST (magnit 0)) x

--------------------------------------------------------------------------------

scalS a (Slice x r0 c0 nr nc) = rowOper (SCAL a (RowRange r0 (r0+nr-1)) (ColRange c0 (c0+nc-1))) x

view x k r = do
    d <- readMatrix x k k
    let rr = r-1-k
        o  = if k < r-1 then 1 else 0
        s = Slice x (k+1) (k+1) rr rr
        u = Slice x k     (k+1) o  rr
        l = Slice x (k+1) k     rr o
    return (d,u,l,s)

withVec r f = \s x -> do
    p <- newUndefinedVector r
    _ <- f s x p
    v <- unsafeFreezeVector p
    return v


luPacked'' m = (id *** toList) (mutable (withVec (rows m) lu2) m)
  where
    lu2 (r,_) x p = do
        forM_ [0..r-1] $ \k -> do
            pivot x p k
            (d,u,l,s) <- view x k r
            when (magnit 0 d) $ do
                scalS (recip d) l
                gemmm 1 s (-1) l u

    pivot x p k = do
        j <- maxIndex . abs . flatten <$> extractMatrix x (FromRow k) (Col k)
        writeVector p k (j+k)
        swap k (k+j)
      where
        swap i j = rowOper (SWAP i j AllCols) x

--------------------------------------------------------------------------------

rowRange m = [0..rows m -1]

at k = Pos (idxs[k])

backSust' lup rhs = foldl' f (rhs?[]) (reverse ls)
  where
    ls  = [ (d k , u k , b k) | k <- rowRange lup ]
      where
        d k = lup ?? (at k, at k)
        u k = lup ?? (at k, Drop (k+1))
        b k = rhs ?? (at k, All)

    f x (d,u,b) = (b - u<>x) / d
                       ===
                        x


forwSust' lup rhs = foldl' f (rhs?[]) ls
  where
    ls  = [ (l k , b k) | k <- rowRange lup ]
      where
        l k = lup ?? (at k, Take k)
        b k = rhs ?? (at k, All)

    f x (l,b) =     x
                   ===
                (b - l<>x)


luSolve'' (LU lup p) b = backSust' lup (forwSust' lup pb)
  where
    pb = b ?? (Pos (fixPerm' p), All)

--------------------------------------------------------------------------------

forwSust lup rhs = fst $ mutable f rhs
  where
    f (r,c) x = do
        l <- unsafeThawMatrix lup
        let go k = gemmm 1 (Slice x k 0 1 c) (-1) (Slice l k 0 1 k) (Slice x 0 0 k c)
        mapM_ go [0..r-1]


backSust lup rhs = fst $ mutable f rhs
  where
    f (r,c) m = do
        l <- unsafeThawMatrix lup
        let d k = recip (lup `atIndex` (k,k))
            u k = Slice l k (k+1) 1 (r-1-k)
            b k = Slice m k 0 1 c
            x k = Slice m (k+1) 0 (r-1-k) c
            scal k = rowOper (SCAL (d k) (Row k) AllCols) m

            go k = gemmm 1 (b k) (-1) (u k) (x k) >> scal k
        mapM_ go [r-1,r-2..0]


{- | Experimental implementation of 'luSolve' for any Fractional element type, including 'Mod' n 'I' and 'Mod' n 'Z'.

>>> let a = (2><2) [1,2,3,5] :: Matrix (Z ./. 13)
(2><2)
 [ 1, 2
 , 3, 5 ]
>>> b
(2><3)
 [ 5, 1, 3
 , 8, 6, 3 ]

>>> luSolve' (luPacked' a) b
(2><3)
 [ 4,  7, 4
 , 7, 10, 6 ]

-}
luSolve' (LU lup p) b = backSust lup (forwSust lup pb)
  where
    pb = b ?? (Pos (fixPerm' p), All)


--------------------------------------------------------------------------------

data MatrixView t b
    = Elem t
    | Block b b b b
  deriving Show


viewBlock' r c m
    | (rt,ct) == (1,1) = Elem (atM' m 0 0)
    | otherwise        = Block m11 m12 m21 m22
  where
    (rt,ct) = size m
    m11 = subm (0,0) (r,c)       m
    m12 = subm (0,c) (r,ct-c)    m
    m21 = subm (r,0) (rt-r,c)    m
    m22 = subm (r,c) (rt-r,ct-c) m
    subm = subMatrix

viewBlock m = viewBlock' n n m
  where
    n = rows m `div` 2

invershur (viewBlock -> Block a b c d) = fromBlocks [[a',b'],[c',d']]
  where
    r1 = invershur a
    r2 = c <> r1
    r3 = r1 <> b
    r4 = c <> r3
    r5 = r4-d
    r6 = invershur r5
    b' = r3 <> r6
    c' = r6 <> r2
    r7 = r3 <> c'
    a' = r1-r7
    d' = -r6

invershur x = recip x

--------------------------------------------------------------------------------

instance Testable (Matrix I) where
   checkT _ = test

test :: (Bool, IO())
test = (and ok, return ())
  where
    m  = (3><4) [1..12] :: Matrix I
    r  = (2><3) [1,2,3,4,3,2]
    c  = (3><2) [0,4,4,1,2,3]
    p  = (9><10) [0..89] :: Matrix I
    ep = (2><3) [10,24,32,44,31,23]
    md = fromInt m      :: Matrix Double
    ok = [ tr m <> m == toInt (tr md <> md)
         , m <> tr m == toInt (md <> tr md)
         , m ?? (Take 2, Take 3) == remap (asColumn (range 2)) (asRow (range 3)) m
         , remap r (tr c) p == ep
         , tr p ?? (PosCyc (idxs[-5,13]), Pos (idxs[3,7,1])) == (2><3) [35,75,15,33,73,13]
         ]
