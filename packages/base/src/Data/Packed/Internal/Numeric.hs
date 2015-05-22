{-# LANGUAGE CPP #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE UndecidableInstances #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Packed.Internal.Numeric
-- Copyright   :  (c) Alberto Ruiz 2010-14
-- License     :  BSD3
-- Maintainer  :  Alberto Ruiz
-- Stability   :  provisional
--
-----------------------------------------------------------------------------

module Data.Packed.Internal.Numeric (
    -- * Basic functions
    ident, diag, ctrans,
    -- * Generic operations
    SContainer(..), Container(..),
    scalar, conj, scale, arctan2, cmap,
    atIndex, minIndex, maxIndex, minElement, maxElement,
    sumElements, prodElements,
    step, cond, find, assoc, accum,
    Transposable(..), Linear(..), Testable(..),
    -- * Matrix product and related functions
    Product(..), udot,
    mXm,mXv,vXm,
    outer, kronecker,
    -- * sorting
    sortVector,
    -- * Element conversion
    Convert(..),
    Complexable(),
    RealElement(),
    roundVector,
    RealOf, ComplexOf, SingleOf, DoubleOf,
    IndexOf,
    CInt, Extractor(..), (??),(¿¿),
    module Data.Complex
) where

import Data.Packed
import Data.Packed.ST as ST
import Numeric.Conversion
import Data.Packed.Development
import Numeric.Vectorized
import Data.Complex

import Numeric.LinearAlgebra.LAPACK(multiplyR,multiplyC,multiplyF,multiplyQ)
import Data.Packed.Internal
import Foreign.C.Types(CInt)
import Text.Printf(printf)

-------------------------------------------------------------------

type family IndexOf (c :: * -> *)

type instance IndexOf Vector = Int
type instance IndexOf Matrix = (Int,Int)

type family ArgOf (c :: * -> *) a

type instance ArgOf Vector a = a -> a
type instance ArgOf Matrix a = a -> a -> a

--------------------------------------------------------------------------

data Extractor = All | Range Int Int | At [Int] | AtCyc [Int] | Take Int | Drop Int

idxs js = fromList (map fromIntegral js) :: Idxs

infixl 9 ??, ¿¿
(??),(¿¿) :: Element t => Matrix t -> Extractor -> Matrix t

m ?? All = m
m ?? Take 0 = (0><cols m) []
m ?? Take n | abs n >= rows m = m
m ?? Drop 0 = m
m ?? Drop n | abs n >= rows m = (0><cols m) []
m ?? Range a b | a > b = m ?? Take 0
m ?? Range a b | a < 0 || b >= cols m = error $
    printf "can't extract rows %d to %d from matrix %dx%d" a b (rows m) (cols m)
m ?? At ps | minimum ps < 0 || maximum ps >= rows m = error $
    printf "can't extract rows %s from matrix %dx%d" (show ps) (rows m) (cols m)

m ?? er = extractR m mode js
  where
    (mode,js) = mkExt (rows m) er
    ran a b = (0, idxs [a,b])
    pos ks  = (1, idxs ks)
    mkExt _ (At ks)       = pos ks
    mkExt n (AtCyc ks)    = pos (map (`mod` n) ks)
    mkExt n All           = ran 0 (n-1)
    mkExt _ (Range mn mx) = ran mn mx
    mkExt n (Take k)
        | k >= 0          = ran 0 (k-1)
        | otherwise       = mkExt n (Drop (n+k))
    mkExt n (Drop k)
        | k >= 0          = ran k (n-1)
        | otherwise       = mkExt n (Take (n+k))


m ¿¿ Range a b | a < 0 || b > cols m -1 = error $
    printf "can't extract columns %d to %d from matrix %dx%d" a b (rows m) (cols m)

m ¿¿ At ps | minimum ps < 0 || maximum ps >= cols m = error $
    printf "can't extract columns %s from matrix %dx%d" (show ps) (rows m) (cols m)
m ¿¿ ec = trans (trans m ?? ec)

-------------------------------------------------------------------


-- | Basic element-by-element functions for numeric containers
class Element e => SContainer c e
  where
    size'        :: c e -> IndexOf c
    scalar'      :: e -> c e
    scale'       :: e -> c e -> c e
    addConstant :: e -> c e -> c e
    add         :: c e -> c e -> c e
    sub         :: c e -> c e -> c e
    -- | element by element multiplication
    mul         :: c e -> c e -> c e
    equal       :: c e -> c e -> Bool
    cmap'        :: (Element b) => (e -> b) -> c e -> c b
    konst'      :: e -> IndexOf c -> c e
    build'       :: IndexOf c -> (ArgOf c e) -> c e
    atIndex'     :: c e -> IndexOf c -> e
    minIndex'    :: c e -> IndexOf c
    maxIndex'    :: c e -> IndexOf c
    minElement'  :: c e -> e
    maxElement'  :: c e -> e
    sumElements' :: c e -> e
    prodElements' :: c e -> e
    step' :: RealElement e => c e -> c e
    cond' :: RealElement e
         => c e -- ^ a
         -> c e -- ^ b
         -> c e -- ^ l
         -> c e -- ^ e
         -> c e -- ^ g
         -> c e -- ^ result
    find' :: (e -> Bool) -> c e -> [IndexOf c]
    assoc' :: IndexOf c       -- ^ size
          -> e                -- ^ default value
          -> [(IndexOf c, e)] -- ^ association list
          -> c e              -- ^ result
    accum' :: c e             -- ^ initial structure
          -> (e -> e -> e)    -- ^ update function
          -> [(IndexOf c, e)] -- ^ association list
          -> c e              -- ^ result


-- | Basic element-by-element functions for numeric containers
class (Complexable c, Fractional e, SContainer c e) => Container c e
  where
    conj'        :: c e -> c e
    -- | scale the element by element reciprocal of the object:
    --
    -- @scaleRecip 2 (fromList [5,i]) == 2 |> [0.4 :+ 0.0,0.0 :+ (-2.0)]@
    scaleRecip  :: e -> c e -> c e
    -- | element by element division
    divide      :: c e -> c e -> c e
    --
    -- element by element inverse tangent
    arctan2'     :: c e -> c e -> c e


--------------------------------------------------------------------------

instance SContainer Vector CInt
  where
    size' = dim
--    scale' = vectorMapValF Scale
--    addConstant = vectorMapValF AddConstant
--    add = vectorZipF Add
--    sub = vectorZipF Sub
--    mul = vectorZipF Mul
--    equal u v = dim u == dim v && maxElement (vectorMapF Abs (sub u v)) == 0.0
    scalar' x = fromList [x]
    konst' = constantD
    build' = buildV
    cmap' = mapVector
    atIndex' = (@>)
--    minIndex'     = emptyErrorV "minIndex"   (round . toScalarF MinIdx)
--    maxIndex'     = emptyErrorV "maxIndex"   (round . toScalarF MaxIdx)
--    minElement'   = emptyErrorV "minElement" (toScalarF Min)
--    maxElement'   = emptyErrorV "maxElement" (toScalarF Max)
--    sumElements'  = sumF
--    prodElements' = prodF
--    step' = stepF
    find' = findV
    assoc' = assocV
    accum' = accumV
--    cond' = condV condI


instance SContainer Vector Float
  where
    size' = dim
    scale' = vectorMapValF Scale
    addConstant = vectorMapValF AddConstant
    add = vectorZipF Add
    sub = vectorZipF Sub
    mul = vectorZipF Mul
    equal u v = dim u == dim v && maxElement (vectorMapF Abs (sub u v)) == 0.0
    scalar' x = fromList [x]
    konst' = constantD
    build' = buildV
    cmap' = mapVector
    atIndex' = (@>)
    minIndex'     = emptyErrorV "minIndex"   (round . toScalarF MinIdx)
    maxIndex'     = emptyErrorV "maxIndex"   (round . toScalarF MaxIdx)
    minElement'   = emptyErrorV "minElement" (toScalarF Min)
    maxElement'   = emptyErrorV "maxElement" (toScalarF Max)
    sumElements'  = sumF
    prodElements' = prodF
    step' = stepF
    find' = findV
    assoc' = assocV
    accum' = accumV
    cond' = condV condF

instance Container Vector Float
  where
    scaleRecip = vectorMapValF Recip
    divide = vectorZipF Div
    arctan2' = vectorZipF ATan2
    conj' = id


instance SContainer Vector Double
  where
    size' = dim
    scale' = vectorMapValR Scale
    addConstant = vectorMapValR AddConstant
    add = vectorZipR Add
    sub = vectorZipR Sub
    mul = vectorZipR Mul
    equal u v = dim u == dim v && maxElement (vectorMapR Abs (sub u v)) == 0.0
    scalar' x = fromList [x]
    konst' = constantD
    build' = buildV
    cmap' = mapVector
    atIndex' = (@>)
    minIndex'     = emptyErrorV "minIndex"   (round . toScalarR MinIdx)
    maxIndex'     = emptyErrorV "maxIndex"   (round . toScalarR MaxIdx)
    minElement'   = emptyErrorV "minElement" (toScalarR Min)
    maxElement'   = emptyErrorV "maxElement" (toScalarR Max)
    sumElements'  = sumR
    prodElements' = prodR
    step' = stepD
    find' = findV
    assoc' = assocV
    accum' = accumV
    cond' = condV condD

instance Container Vector Double
  where
    scaleRecip = vectorMapValR Recip
    divide = vectorZipR Div
    arctan2' = vectorZipR ATan2
    conj' = id

instance SContainer Vector (Complex Double)
  where
    size' = dim
    scale' = vectorMapValC Scale
    addConstant = vectorMapValC AddConstant
    add = vectorZipC Add
    sub = vectorZipC Sub
    mul = vectorZipC Mul
    equal u v = dim u == dim v && maxElement (mapVector magnitude (sub u v)) == 0.0
    scalar' x = fromList [x]
    konst' = constantD
    build' = buildV
    cmap' = mapVector
    atIndex' = (@>)
    minIndex'     = emptyErrorV "minIndex" (minIndex' . fst . fromComplex . (mul <*> conj'))
    maxIndex'     = emptyErrorV "maxIndex" (maxIndex' . fst . fromComplex . (mul <*> conj'))
    minElement'   = emptyErrorV "minElement" (atIndex' <*> minIndex')
    maxElement'   = emptyErrorV "maxElement" (atIndex' <*> maxIndex')
    sumElements'  = sumC
    prodElements' = prodC
    step' = undefined -- cannot match
    find' = findV
    assoc' = assocV
    accum' = accumV
    cond' = undefined -- cannot match


instance Container Vector (Complex Double)
  where
    scaleRecip = vectorMapValC Recip
    divide = vectorZipC Div
    arctan2' = vectorZipC ATan2
    conj' = conjugateC


instance SContainer Vector (Complex Float)
  where
    size' = dim
    scale' = vectorMapValQ Scale
    addConstant = vectorMapValQ AddConstant
    add = vectorZipQ Add
    sub = vectorZipQ Sub
    mul = vectorZipQ Mul
    equal u v = dim u == dim v && maxElement (mapVector magnitude (sub u v)) == 0.0
    scalar' x = fromList [x]
    konst' = constantD
    build' = buildV
    cmap' = mapVector
    atIndex' = (@>)
    minIndex'     = emptyErrorV "minIndex" (minIndex' . fst . fromComplex . (mul <*> conj'))
    maxIndex'     = emptyErrorV "maxIndex" (maxIndex' . fst . fromComplex . (mul <*> conj'))
    minElement'   = emptyErrorV "minElement" (atIndex' <*> minIndex')
    maxElement'   = emptyErrorV "maxElement" (atIndex' <*> maxIndex')
    sumElements'  = sumQ
    prodElements' = prodQ
    step' = undefined -- cannot match
    find' = findV
    assoc' = assocV
    accum' = accumV
    cond' = undefined -- cannot match

instance Container Vector (Complex Float)
  where
    scaleRecip = vectorMapValQ Recip
    divide = vectorZipQ Div
    arctan2' = vectorZipQ ATan2
    conj' = conjugateQ


---------------------------------------------------------------

instance (Num a, Element a, SContainer Vector a) => SContainer Matrix a
  where
    size' = size
    scale' x = liftMatrix (scale' x)
    addConstant x = liftMatrix (addConstant x)
    add = liftMatrix2 add
    sub = liftMatrix2 sub
    mul = liftMatrix2 mul
    equal a b = cols a == cols b && flatten a `equal` flatten b
    scalar' x = (1><1) [x]
    konst' v (r,c) = matrixFromVector RowMajor r c (konst' v (r*c))
    build' = buildM
    cmap' f = liftMatrix (mapVector f)
    atIndex' = (@@>)
    minIndex' = emptyErrorM "minIndex of Matrix" $
                \m -> divMod (minIndex' $ flatten m) (cols m)
    maxIndex' = emptyErrorM "maxIndex of Matrix" $
                \m -> divMod (maxIndex' $ flatten m) (cols m)
    minElement' = emptyErrorM "minElement of Matrix" (atIndex' <*> minIndex')
    maxElement' = emptyErrorM "maxElement of Matrix" (atIndex' <*> maxIndex')
    sumElements' = sumElements' . flatten
    prodElements' = prodElements' . flatten
    step' = liftMatrix step'
    find' = findM
    assoc' = assocM
    accum' = accumM
    cond' = condM


instance (Fractional a, Container Vector a) => Container Matrix a
  where
    scaleRecip x = liftMatrix (scaleRecip x)
    divide = liftMatrix2 divide
    arctan2' = liftMatrix2 arctan2'
    conj' = liftMatrix conj'


emptyErrorV msg f v =
    if dim v > 0
        then f v
        else error $ msg ++ " of Vector with dim = 0"

emptyErrorM msg f m =
    if rows m > 0 && cols m > 0
        then f m
        else error $ msg++" "++shSize m

--------------------------------------------------------------------------------

-- | create a structure with a single element
--
-- >>> let v = fromList [1..3::Double]
-- >>> v / scalar (norm2 v)
-- fromList [0.2672612419124244,0.5345224838248488,0.8017837257372732]
--
scalar :: Container c e => e -> c e
scalar = scalar'

-- | complex conjugate
conj :: Container c e => c e -> c e
conj = conj'

-- | multiplication by scalar
scale :: Container c e => e -> c e -> c e
scale = scale'

arctan2 :: Container c e => c e -> c e -> c e
arctan2 = arctan2'

-- | like 'fmap' (cannot implement instance Functor because of Element class constraint)
cmap :: (Element b, Container c e) => (e -> b) -> c e -> c b
cmap = cmap'

-- | indexing function
atIndex :: Container c e => c e -> IndexOf c -> e
atIndex = atIndex'

-- | index of minimum element
minIndex :: Container c e => c e -> IndexOf c
minIndex = minIndex'

-- | index of maximum element
maxIndex :: Container c e => c e -> IndexOf c
maxIndex = maxIndex'

-- | value of minimum element
minElement :: Container c e => c e -> e
minElement = minElement'

-- | value of maximum element
maxElement :: Container c e => c e -> e
maxElement = maxElement'

-- | the sum of elements
sumElements :: Container c e => c e -> e
sumElements = sumElements'

-- | the product of elements
prodElements :: Container c e => c e -> e
prodElements = prodElements'


-- | A more efficient implementation of @cmap (\\x -> if x>0 then 1 else 0)@
--
-- >>> step $ linspace 5 (-1,1::Double)
-- 5 |> [0.0,0.0,0.0,1.0,1.0]
--
step
  :: (RealElement e, Container c e)
    => c e
    -> c e
step = step'


-- | Element by element version of @case compare a b of {LT -> l; EQ -> e; GT -> g}@.
--
-- Arguments with any dimension = 1 are automatically expanded:
--
-- >>> cond ((1><4)[1..]) ((3><1)[1..]) 0 100 ((3><4)[1..]) :: Matrix Double
-- (3><4)
-- [ 100.0,   2.0,   3.0,  4.0
-- ,   0.0, 100.0,   7.0,  8.0
-- ,   0.0,   0.0, 100.0, 12.0 ]
--
cond
    :: (RealElement e, Container c e)
    => c e -- ^ a
    -> c e -- ^ b
    -> c e -- ^ l
    -> c e -- ^ e
    -> c e -- ^ g
    -> c e -- ^ result
cond = cond'


-- | Find index of elements which satisfy a predicate
--
-- >>> find (>0) (ident 3 :: Matrix Double)
-- [(0,0),(1,1),(2,2)]
--
find
  :: Container c e
    => (e -> Bool)
    -> c e
    -> [IndexOf c]
find = find'


-- | Create a structure from an association list
--
-- >>> assoc 5 0 [(3,7),(1,4)] :: Vector Double
-- fromList [0.0,4.0,0.0,7.0,0.0]
--
-- >>> assoc (2,3) 0 [((0,2),7),((1,0),2*i-3)] :: Matrix (Complex Double)
-- (2><3)
--  [    0.0 :+ 0.0, 0.0 :+ 0.0, 7.0 :+ 0.0
--  , (-3.0) :+ 2.0, 0.0 :+ 0.0, 0.0 :+ 0.0 ]
--
assoc
  :: Container c e
    => IndexOf c        -- ^ size
    -> e                -- ^ default value
    -> [(IndexOf c, e)] -- ^ association list
    -> c e              -- ^ result
assoc = assoc'


-- | Modify a structure using an update function
--
-- >>> accum (ident 5) (+) [((1,1),5),((0,3),3)] :: Matrix Double
-- (5><5)
--  [ 1.0, 0.0, 0.0, 3.0, 0.0
--  , 0.0, 6.0, 0.0, 0.0, 0.0
--  , 0.0, 0.0, 1.0, 0.0, 0.0
--  , 0.0, 0.0, 0.0, 1.0, 0.0
--  , 0.0, 0.0, 0.0, 0.0, 1.0 ]
--
-- computation of histogram:
--
-- >>> accum (konst 0 7) (+) (map (flip (,) 1) [4,5,4,1,5,2,5]) :: Vector Double
-- fromList [0.0,1.0,1.0,0.0,2.0,3.0,0.0]
--
accum
  :: Container c e
    => c e              -- ^ initial structure
    -> (e -> e -> e)    -- ^ update function
    -> [(IndexOf c, e)] -- ^ association list
    -> c e              -- ^ result
accum = accum'


--------------------------------------------------------------------------------

-- | Matrix product and related functions
class (Num e, Element e) => Product e where
    -- | matrix product
    multiply :: Matrix e -> Matrix e -> Matrix e
    -- | sum of absolute value of elements (differs in complex case from @norm1@)
    absSum     :: Vector e -> RealOf e
    -- | sum of absolute value of elements
    norm1      :: Vector e -> RealOf e
    -- | euclidean norm
    norm2      :: Vector e -> RealOf e
    -- | element of maximum magnitude
    normInf    :: Vector e -> RealOf e

instance Product Float where
    norm2      = emptyVal (toScalarF Norm2)
    absSum     = emptyVal (toScalarF AbsSum)
    norm1      = emptyVal (toScalarF AbsSum)
    normInf    = emptyVal (maxElement . vectorMapF Abs)
    multiply   = emptyMul multiplyF

instance Product Double where
    norm2      = emptyVal (toScalarR Norm2)
    absSum     = emptyVal (toScalarR AbsSum)
    norm1      = emptyVal (toScalarR AbsSum)
    normInf    = emptyVal (maxElement . vectorMapR Abs)
    multiply   = emptyMul multiplyR

instance Product (Complex Float) where
    norm2      = emptyVal (toScalarQ Norm2)
    absSum     = emptyVal (toScalarQ AbsSum)
    norm1      = emptyVal (sumElements . fst . fromComplex . vectorMapQ Abs)
    normInf    = emptyVal (maxElement . fst . fromComplex . vectorMapQ Abs)
    multiply   = emptyMul multiplyQ

instance Product (Complex Double) where
    norm2      = emptyVal (toScalarC Norm2)
    absSum     = emptyVal (toScalarC AbsSum)
    norm1      = emptyVal (sumElements . fst . fromComplex . vectorMapC Abs)
    normInf    = emptyVal (maxElement . fst . fromComplex . vectorMapC Abs)
    multiply   = emptyMul multiplyC

emptyMul m a b
    | x1 == 0 && x2 == 0 || r == 0 || c == 0 = konst' 0 (r,c)
    | otherwise = m a b
  where
    r  = rows a
    x1 = cols a
    x2 = rows b
    c  = cols b

emptyVal f v =
    if dim v > 0
        then f v
        else 0

-- FIXME remove unused C wrappers
-- | unconjugated dot product
udot :: Product e => Vector e -> Vector e -> e
udot u v
    | dim u == dim v = val (asRow u `multiply` asColumn v)
    | otherwise = error $ "different dimensions "++show (dim u)++" and "++show (dim v)++" in dot product"
  where
    val m | dim u > 0 = m@@>(0,0)
          | otherwise = 0

----------------------------------------------------------

-- synonym for matrix product
mXm :: Product t => Matrix t -> Matrix t -> Matrix t
mXm = multiply

-- matrix - vector product
mXv :: Product t => Matrix t -> Vector t -> Vector t
mXv m v = flatten $ m `mXm` (asColumn v)

-- vector - matrix product
vXm :: Product t => Vector t -> Matrix t -> Vector t
vXm v m = flatten $ (asRow v) `mXm` m

{- | Outer product of two vectors.

>>> fromList [1,2,3] `outer` fromList [5,2,3]
(3><3)
 [  5.0, 2.0, 3.0
 , 10.0, 4.0, 6.0
 , 15.0, 6.0, 9.0 ]

-}
outer :: (Product t) => Vector t -> Vector t -> Matrix t
outer u v = asColumn u `multiply` asRow v

{- | Kronecker product of two matrices.

@m1=(2><3)
 [ 1.0,  2.0, 0.0
 , 0.0, -1.0, 3.0 ]
m2=(4><3)
 [  1.0,  2.0,  3.0
 ,  4.0,  5.0,  6.0
 ,  7.0,  8.0,  9.0
 , 10.0, 11.0, 12.0 ]@

>>> kronecker m1 m2
(8><9)
 [  1.0,  2.0,  3.0,   2.0,   4.0,   6.0,  0.0,  0.0,  0.0
 ,  4.0,  5.0,  6.0,   8.0,  10.0,  12.0,  0.0,  0.0,  0.0
 ,  7.0,  8.0,  9.0,  14.0,  16.0,  18.0,  0.0,  0.0,  0.0
 , 10.0, 11.0, 12.0,  20.0,  22.0,  24.0,  0.0,  0.0,  0.0
 ,  0.0,  0.0,  0.0,  -1.0,  -2.0,  -3.0,  3.0,  6.0,  9.0
 ,  0.0,  0.0,  0.0,  -4.0,  -5.0,  -6.0, 12.0, 15.0, 18.0
 ,  0.0,  0.0,  0.0,  -7.0,  -8.0,  -9.0, 21.0, 24.0, 27.0
 ,  0.0,  0.0,  0.0, -10.0, -11.0, -12.0, 30.0, 33.0, 36.0 ]

-}
kronecker :: (Product t) => Matrix t -> Matrix t -> Matrix t
kronecker a b = fromBlocks
              . splitEvery (cols a)
              . map (reshape (cols b))
              . toRows
              $ flatten a `outer` flatten b

-------------------------------------------------------------------


class Convert t where
    real    :: Container c t => c (RealOf t) -> c t
    complex :: Container c t => c t -> c (ComplexOf t)
    single  :: Container c t => c t -> c (SingleOf t)
    double  :: Container c t => c t -> c (DoubleOf t)
    toComplex   :: (Container c t, RealElement t) => (c t, c t) -> c (Complex t)
    fromComplex :: (Container c t, RealElement t) => c (Complex t) -> (c t, c t)


instance Convert Double where
    real = id
    complex = comp'
    single = single'
    double = id
    toComplex = toComplex'
    fromComplex = fromComplex'

instance Convert Float where
    real = id
    complex = comp'
    single = id
    double = double'
    toComplex = toComplex'
    fromComplex = fromComplex'

instance Convert (Complex Double) where
    real = comp'
    complex = id
    single = single'
    double = id
    toComplex = toComplex'
    fromComplex = fromComplex'

instance Convert (Complex Float) where
    real = comp'
    complex = id
    single = id
    double = double'
    toComplex = toComplex'
    fromComplex = fromComplex'

-------------------------------------------------------------------

type family RealOf x

type instance RealOf Double = Double
type instance RealOf (Complex Double) = Double

type instance RealOf Float = Float
type instance RealOf (Complex Float) = Float

type family ComplexOf x

type instance ComplexOf Double = Complex Double
type instance ComplexOf (Complex Double) = Complex Double

type instance ComplexOf Float = Complex Float
type instance ComplexOf (Complex Float) = Complex Float

type family SingleOf x

type instance SingleOf Double = Float
type instance SingleOf Float  = Float

type instance SingleOf (Complex a) = Complex (SingleOf a)

type family DoubleOf x

type instance DoubleOf Double = Double
type instance DoubleOf Float  = Double

type instance DoubleOf (Complex a) = Complex (DoubleOf a)

type family ElementOf c

type instance ElementOf (Vector a) = a
type instance ElementOf (Matrix a) = a

------------------------------------------------------------

buildM (rc,cc) f = fromLists [ [f r c | c <- cs] | r <- rs ]
    where rs = map fromIntegral [0 .. (rc-1)]
          cs = map fromIntegral [0 .. (cc-1)]

buildV n f = fromList [f k | k <- ks]
    where ks = map fromIntegral [0 .. (n-1)]

--------------------------------------------------------
-- | conjugate transpose
ctrans :: (Container Vector e, Element e) => Matrix e -> Matrix e
ctrans = liftMatrix conj' . trans

-- | Creates a square matrix with a given diagonal.
diag :: (Num a, Element a) => Vector a -> Matrix a
diag v = diagRect 0 v n n where n = dim v

-- | creates the identity matrix of given dimension
ident :: (Num a, Element a) => Int -> Matrix a
ident n = diag (constantD 1 n)

--------------------------------------------------------

findV p x = foldVectorWithIndex g [] x where
    g k z l = if p z then k:l else l

findM p x = map ((`divMod` cols x)) $ findV p (flatten x)

assocV n z xs = ST.runSTVector $ do
        v <- ST.newVector z n
        mapM_ (\(k,x) -> ST.writeVector v k x) xs
        return v

assocM (r,c) z xs = ST.runSTMatrix $ do
        m <- ST.newMatrix z r c
        mapM_ (\((i,j),x) -> ST.writeMatrix m i j x) xs
        return m

accumV v0 f xs = ST.runSTVector $ do
        v <- ST.thawVector v0
        mapM_ (\(k,x) -> ST.modifyVector v k (f x)) xs
        return v

accumM m0 f xs = ST.runSTMatrix $ do
        m <- ST.thawMatrix m0
        mapM_ (\((i,j),x) -> ST.modifyMatrix m i j (f x)) xs
        return m

----------------------------------------------------------------------

condM a b l e t = matrixFromVector RowMajor (rows a'') (cols a'') $ cond' a' b' l' e' t'
  where
    args@(a'':_) = conformMs [a,b,l,e,t]
    [a', b', l', e', t'] = map flatten args

condV f a b l e t = f a' b' l' e' t'
  where
    [a', b', l', e', t'] = conformVs [a,b,l,e,t]

--------------------------------------------------------------------------------

class Transposable m mt | m -> mt, mt -> m
  where
    -- | (conjugate) transpose
    tr :: m -> mt

instance (Container Vector t) => Transposable (Matrix t) (Matrix t)
  where
    tr = ctrans

class Linear t v
  where
    scalarL :: t -> v
    addL    :: v -> v -> v
    scaleL  :: t -> v -> v


class Testable t
  where
    checkT   :: t -> (Bool, IO())
    ioCheckT :: t -> IO (Bool, IO())
    ioCheckT = return . checkT

--------------------------------------------------------------------------------

