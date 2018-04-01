{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Packed.Internal.Numeric
-- Copyright   :  (c) Alberto Ruiz 2010-14
-- License     :  BSD3
-- Maintainer  :  Alberto Ruiz
-- Stability   :  provisional
--
-----------------------------------------------------------------------------

module Internal.Numeric where

import Internal.Vector
import Internal.Matrix
import Internal.Element
import Internal.ST as ST
import Internal.Conversion
import Internal.Vectorized
import Internal.LAPACK(multiplyR,multiplyC,multiplyF,multiplyQ,multiplyI,multiplyL)
import Data.List.Split(chunksOf)
import qualified Data.Vector.Storable as V

--------------------------------------------------------------------------------

type family IndexOf (c :: * -> *)

type instance IndexOf Vector = Int
type instance IndexOf Matrix = (Int,Int)

type family ArgOf (c :: * -> *) a

type instance ArgOf Vector a = a -> a
type instance ArgOf Matrix a = a -> a -> a

--------------------------------------------------------------------------------

-- | Basic element-by-element functions for numeric containers
class Element e => Container c e
  where
    conj'        :: c e -> c e
    size'        :: c e -> IndexOf c
    scalar'      :: e -> c e
    scale'       :: e -> c e -> c e
    addConstant :: e -> c e -> c e
    add'        :: c e -> c e -> c e
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
    step' :: Ord e => c e -> c e
    ccompare' :: Ord e => c e -> c e -> c I
    cselect'  :: c I -> c e -> c e -> c e -> c e
    find' :: (e -> Bool) -> c e -> [IndexOf c]
    assoc' :: IndexOf c       -- ^ size
          -> e                -- ^ default value
          -> [(IndexOf c, e)] -- ^ association list
          -> c e              -- ^ result
    accum' :: c e             -- ^ initial structure
          -> (e -> e -> e)    -- ^ update function
          -> [(IndexOf c, e)] -- ^ association list
          -> c e              -- ^ result

    -- | scale the element by element reciprocal of the object:
    --
    -- @scaleRecip 2 (fromList [5,i]) == 2 |> [0.4 :+ 0.0,0.0 :+ (-2.0)]@
    scaleRecip  :: Fractional e => e -> c e -> c e
    -- | element by element division
    divide      :: Fractional e => c e -> c e -> c e
    --
    -- element by element inverse tangent
    arctan2'     :: Fractional e => c e -> c e -> c e
    cmod'        :: Integral   e => e -> c e -> c e
    fromInt'     :: c I -> c e
    toInt'       :: c e -> c I
    fromZ'       :: c Z -> c e
    toZ'         :: c e -> c Z

--------------------------------------------------------------------------

instance Container Vector I
  where
    conj' = id
    size' = dim
    scale' = vectorMapValI Scale
    addConstant = vectorMapValI AddConstant
    add' = vectorZipI Add
    sub = vectorZipI Sub
    mul = vectorZipI Mul
    equal = (==)
    scalar' = V.singleton
    konst' = constantD
    build' = buildV
    cmap' = mapVector
    atIndex' = (@>)
    minIndex'     = emptyErrorV "minIndex"   (fromIntegral . toScalarI MinIdx)
    maxIndex'     = emptyErrorV "maxIndex"   (fromIntegral . toScalarI MaxIdx)
    minElement'   = emptyErrorV "minElement" (toScalarI Min)
    maxElement'   = emptyErrorV "maxElement" (toScalarI Max)
    sumElements'  = sumI 1
    prodElements' = prodI 1
    step' = stepI
    find' = findV
    assoc' = assocV
    accum' = accumV
    ccompare' = compareCV compareV
    cselect' = selectCV selectV
    scaleRecip = undefined -- cannot match
    divide = undefined
    arctan2' = undefined
    cmod' m x
        | m /= 0    = vectorMapValI ModVS m x
        | otherwise = error $ "cmod 0 on vector of size "++(show $ dim x)
    fromInt' = id
    toInt'   = id
    fromZ'   = long2intV
    toZ'     = int2longV


instance Container Vector Z
  where
    conj' = id
    size' = dim
    scale' = vectorMapValL Scale
    addConstant = vectorMapValL AddConstant
    add' = vectorZipL Add
    sub = vectorZipL Sub
    mul = vectorZipL Mul
    equal = (==)
    scalar' = V.singleton
    konst' = constantD
    build' = buildV
    cmap' = mapVector
    atIndex' = (@>)
    minIndex'     = emptyErrorV "minIndex"   (fromIntegral . toScalarL MinIdx)
    maxIndex'     = emptyErrorV "maxIndex"   (fromIntegral . toScalarL MaxIdx)
    minElement'   = emptyErrorV "minElement" (toScalarL Min)
    maxElement'   = emptyErrorV "maxElement" (toScalarL Max)
    sumElements'  = sumL 1
    prodElements' = prodL 1
    step' = stepL
    find' = findV
    assoc' = assocV
    accum' = accumV
    ccompare' = compareCV compareV
    cselect' = selectCV selectV
    scaleRecip = undefined -- cannot match
    divide = undefined
    arctan2' = undefined
    cmod' m x
        | m /= 0    = vectorMapValL ModVS m x
        | otherwise = error $ "cmod 0 on vector of size "++(show $ dim x)
    fromInt' = int2longV
    toInt'   = long2intV
    fromZ'   = id
    toZ'     = id



instance Container Vector Float
  where
    conj' = id
    size' = dim
    scale' = vectorMapValF Scale
    addConstant = vectorMapValF AddConstant
    add' = vectorZipF Add
    sub = vectorZipF Sub
    mul = vectorZipF Mul
    equal = (==)
    scalar' = V.singleton
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
    ccompare' = compareCV compareV
    cselect' = selectCV selectV
    scaleRecip = vectorMapValF Recip
    divide = vectorZipF Div
    arctan2' = vectorZipF ATan2
    cmod' = undefined
    fromInt' = int2floatV
    toInt'   = float2IntV
    fromZ'   = (single :: Vector R-> Vector Float) . fromZ'
    toZ'     = toZ' . double


instance Container Vector Double
  where
    conj' = id
    size' = dim
    scale' = vectorMapValR Scale
    addConstant = vectorMapValR AddConstant
    add' = vectorZipR Add
    sub = vectorZipR Sub
    mul = vectorZipR Mul
    equal = (==)
    scalar' = V.singleton
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
    ccompare' = compareCV compareV
    cselect' = selectCV selectV
    scaleRecip = vectorMapValR Recip
    divide = vectorZipR Div
    arctan2' = vectorZipR ATan2
    cmod' = undefined
    fromInt' = int2DoubleV
    toInt'   = double2IntV
    fromZ'   = long2DoubleV
    toZ'     = double2longV


instance Container Vector (Complex Double)
  where
    conj' = conjugateC
    size' = dim
    scale' = vectorMapValC Scale
    addConstant = vectorMapValC AddConstant
    add' = vectorZipC Add
    sub = vectorZipC Sub
    mul = vectorZipC Mul
    equal = (==)
    scalar' = V.singleton
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
    ccompare' = undefined -- cannot match
    cselect' = selectCV selectV
    scaleRecip = vectorMapValC Recip
    divide = vectorZipC Div
    arctan2' = vectorZipC ATan2
    cmod' = undefined
    fromInt' = complex . int2DoubleV
    toInt'   = toInt' . fst . fromComplex
    fromZ'   = complex . long2DoubleV
    toZ'     = toZ' . fst . fromComplex

instance Container Vector (Complex Float)
  where
    conj' = conjugateQ
    size' = dim
    scale' = vectorMapValQ Scale
    addConstant = vectorMapValQ AddConstant
    add' = vectorZipQ Add
    sub = vectorZipQ Sub
    mul = vectorZipQ Mul
    equal = (==)
    scalar' = V.singleton
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
    ccompare' = undefined -- cannot match
    cselect' = selectCV selectV
    scaleRecip = vectorMapValQ Recip
    divide = vectorZipQ Div
    arctan2' = vectorZipQ ATan2
    cmod' = undefined
    fromInt' = complex . int2floatV
    toInt'   = toInt' . fst . fromComplex
    fromZ' = complex . single . long2DoubleV
    toZ'   = toZ' . double . fst . fromComplex

---------------------------------------------------------------

instance (Num a, Element a, Container Vector a) => Container Matrix a
  where
    conj' = liftMatrix conj'
    size' = size
    scale' x = liftMatrix (scale' x)
    addConstant x = liftMatrix (addConstant x)
    add' = liftMatrix2 add'
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
    ccompare' = compareM
    cselect' = selectM
    scaleRecip x = liftMatrix (scaleRecip x)
    divide = liftMatrix2 divide
    arctan2' = liftMatrix2 arctan2'
    cmod' m x
        | m /= 0    = liftMatrix (cmod' m) x
        | otherwise = error $ "cmod 0 on matrix "++shSize x
    fromInt' = liftMatrix fromInt'
    toInt' = liftMatrix toInt'
    fromZ' = liftMatrix fromZ'
    toZ'   = liftMatrix toZ'


emptyErrorV msg f v =
    if dim v > 0
        then f v
        else error $ msg ++ " of empty Vector"

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


arctan2 :: (Fractional e, Container c e) => c e -> c e -> c e
arctan2 = arctan2'

-- | 'mod' for integer arrays
--
-- >>> cmod 3 (range 5)
-- fromList [0,1,2,0,1]
cmod :: (Integral e, Container c e) => e -> c e -> c e
cmod = cmod'

-- |
-- >>>fromInt ((2><2) [0..3]) :: Matrix (Complex Double)
-- (2><2)
-- [ 0.0 :+ 0.0, 1.0 :+ 0.0
-- , 2.0 :+ 0.0, 3.0 :+ 0.0 ]
--
fromInt :: (Container c e) => c I -> c e
fromInt = fromInt'

toInt :: (Container c e) => c e -> c I
toInt = toInt'

fromZ :: (Container c e) => c Z -> c e
fromZ = fromZ'

toZ :: (Container c e) => c e -> c Z
toZ = toZ'

-- | like 'fmap' (cannot implement instance Functor because of Element class constraint)
cmap :: (Element b, Container c e) => (e -> b) -> c e -> c b
cmap = cmap'

-- | generic indexing function
--
-- >>> vector [1,2,3] `atIndex` 1
-- 2.0
--
-- >>> matrix 3 [0..8] `atIndex` (2,0)
-- 6.0
--
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
  :: (Ord e, Container c e)
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
-- >>> let chop x = cond (abs x) 1E-6 0 0 x
--
cond
    :: (Ord e, Container c e, Container c x)
    => c e -- ^ a
    -> c e -- ^ b
    -> c x -- ^ l
    -> c x -- ^ e
    -> c x -- ^ g
    -> c x -- ^ result
cond a b l e g = cselect' (ccompare' a b) l e g


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

class Konst e d c | d -> c, c -> d
  where
    -- |
    -- >>> konst 7 3 :: Vector Float
    -- fromList [7.0,7.0,7.0]
    --
    -- >>> konst i (3::Int,4::Int)
    -- (3><4)
    --  [ 0.0 :+ 1.0, 0.0 :+ 1.0, 0.0 :+ 1.0, 0.0 :+ 1.0
    --  , 0.0 :+ 1.0, 0.0 :+ 1.0, 0.0 :+ 1.0, 0.0 :+ 1.0
    --  , 0.0 :+ 1.0, 0.0 :+ 1.0, 0.0 :+ 1.0, 0.0 :+ 1.0 ]
    --
    konst :: e -> d -> c e

instance Container Vector e => Konst e Int Vector
  where
    konst = konst'

instance (Num e, Container Vector e) => Konst e (Int,Int) Matrix
  where
    konst = konst'

--------------------------------------------------------------------------------

class ( Container Vector t
      , Container Matrix t
      , Konst t Int Vector
      , Konst t (Int,Int) Matrix
      , CTrans t
      , Product t
      , Additive (Vector t)
      , Additive (Matrix t)
      , Linear t Vector
      , Linear t Matrix
      ) => Numeric t

instance Numeric Double
instance Numeric (Complex Double)
instance Numeric Float
instance Numeric (Complex Float)
instance Numeric I
instance Numeric Z

--------------------------------------------------------------------------------

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
    norm2      :: Floating e => Vector e -> RealOf e
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

instance Product I where
    norm2      = undefined
    absSum     = emptyVal (sumElements . vectorMapI Abs)
    norm1      = absSum
    normInf    = emptyVal (maxElement . vectorMapI Abs)
    multiply   = emptyMul (multiplyI 1)

instance Product Z where
    norm2      = undefined
    absSum     = emptyVal (sumElements . vectorMapL Abs)
    norm1      = absSum
    normInf    = emptyVal (maxElement . vectorMapL Abs)
    multiply   = emptyMul (multiplyL 1)


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
              . chunksOf (cols a)
              . map (reshape (cols b))
              . toRows
              $ flatten a `outer` flatten b

-------------------------------------------------------------------


class Convert t where
    real    :: Complexable c => c (RealOf t) -> c t
    complex :: Complexable c => c t -> c (ComplexOf t)
    single  :: Complexable c => c t -> c (SingleOf t)
    double  :: Complexable c => c t -> c (DoubleOf t)
    toComplex   :: (Complexable c, RealElement t) => (c t, c t) -> c (Complex t)
    fromComplex :: (Complexable c, RealElement t) => c (Complex t) -> (c t, c t)


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

type instance RealOf I = I
type instance RealOf Z = Z

type ComplexOf x = Complex (RealOf x)

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

compareM a b = matrixFromVector RowMajor (rows a'') (cols a'') $ ccompare' a' b'
  where
    args@(a'':_) = conformMs [a,b]
    [a', b'] = map flatten args

compareCV f a b = f a' b'
  where
    [a', b'] = conformVs [a,b]

selectM c l e t = matrixFromVector RowMajor (rows a'') (cols a'') $ cselect' (toInt c') l' e' t'
  where
    args@(a'':_) = conformMs [fromInt c,l,e,t]
    [c', l', e', t'] = map flatten args

selectCV f c l e t = f (toInt c') l' e' t'
  where
    [c', l', e', t'] = conformVs [fromInt c,l,e,t]

--------------------------------------------------------------------------------

class CTrans t
  where
    ctrans :: Matrix t -> Matrix t
    ctrans = trans

instance CTrans Float
instance CTrans R
instance CTrans I
instance CTrans Z

instance CTrans C
  where
    ctrans = conj . trans

instance CTrans (Complex Float)
  where
    ctrans = conj . trans

class Transposable m mt | m -> mt, mt -> m
  where
    -- | conjugate transpose
    tr  :: m -> mt
    -- | transpose
    tr' :: m -> mt

instance (CTrans t, Container Vector t) => Transposable (Matrix t) (Matrix t)
  where
    tr  = ctrans
    tr' = trans

class Additive c
  where
    add    :: c -> c -> c

class Linear t c
  where
    scale  :: t -> c t -> c t


instance Container Vector t => Linear t Vector
  where
    scale = scale'

instance Container Matrix t => Linear t Matrix
  where
    scale = scale'

instance Container Vector t => Additive (Vector t)
  where
    add = add'

instance Container Matrix t => Additive (Matrix t)
  where
    add = add'


class Testable t
  where
    checkT   :: t -> (Bool, IO())
    ioCheckT :: t -> IO (Bool, IO())
    ioCheckT = return . checkT

--------------------------------------------------------------------------------

