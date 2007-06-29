-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Packed.Internal.Tensor
-- Copyright   :  (c) Alberto Ruiz 2007
-- License     :  GPL-style
--
-- Maintainer  :  Alberto Ruiz <aruiz@um.es>
-- Stability   :  provisional
-- Portability :  portable (uses FFI)
--
-- basic tensor operations
--
-----------------------------------------------------------------------------

module Data.Packed.Internal.Tensor where

import Data.Packed.Internal
import Foreign.Storable
import Data.List(sort,elemIndex,nub,foldl1',foldl')
import GSL.Vector
import Data.Packed.Matrix

data IdxType = Covariant | Contravariant deriving (Show,Eq)

type IdxName = String

data IdxDesc = IdxDesc { idxDim  :: Int,
                         idxType :: IdxType,
                         idxName :: IdxName } deriving Show

data Tensor t = T { dims   :: [IdxDesc]
                  , ten    :: Vector t
                  }

-- | tensor rank (number of indices)
rank :: Tensor t -> Int
rank = length . dims

instance (Show a,Storable a) => Show (Tensor a) where
    show T {dims = [], ten = t} = "scalar "++show (t `at` 0)
    show T {dims = ds, ten = t} = "("++shdims ds ++") "++ show (toList t)

-- | a nice description of the tensor structure
shdims :: [IdxDesc] -> String
shdims [] = ""
shdims [d] = shdim d 
shdims (d:ds) = shdim d ++ "><"++ shdims ds
shdim (IdxDesc n t name) = name ++ sym t ++"["++show n++"]"
    where sym Covariant     = "_"
          sym Contravariant = "^"

-- | express the tensor as a matrix with the given index in columns
findIdx :: (Field t) => IdxName -> Tensor t
        -> (([IdxDesc], [IdxDesc]), Matrix t)
findIdx name t = ((d1,d2),m) where
    (d1,d2) = span (\d -> idxName d /= name) (dims t)
    c = product (map idxDim d2)
    m = matrixFromVector RowMajor c (ten t)

-- | express the tensor as a matrix with the given index in rows
putFirstIdx :: (Field t) => String -> Tensor t -> ([IdxDesc], Matrix t)
putFirstIdx name t = (nd,m')
    where ((d1,d2),m) = findIdx name t
          m' = matrixFromVector RowMajor c $ cdat $ trans m
          nd = d2++d1
          c = dim (ten t) `div` (idxDim $ head d2)

-- | extracts a given part of a tensor
part :: (Field t) => Tensor t -> (IdxName, Int) -> Tensor t
part t (name,k) = if k<0 || k>=l
                    then error $ "part "++show (name,k)++" out of range" -- in "++show t
                    else T {dims = ds, ten = toRows m !! k}
    where (d:ds,m) = putFirstIdx name t
          l = idxDim d

-- | creates a list with all parts of a tensor along a given index
parts :: (Field t) => Tensor t -> IdxName -> [Tensor t]
parts t name = map f (toRows m)
    where (d:ds,m) = putFirstIdx name t
          l = idxDim d
          f t = T {dims=ds, ten=t}

-- | tensor product without without any contractions
rawProduct :: (Field t, Num t) => Tensor t -> Tensor t -> Tensor t
rawProduct (T d1 v1) (T d2 v2) = T (d1++d2) (outer' v1 v2)

-- | contraction of the product of two tensors 
contraction2 :: (Field t, Num t) => Tensor t -> IdxName -> Tensor t -> IdxName -> Tensor t
contraction2 t1 n1 t2 n2 =
    if compatIdx t1 n1 t2 n2
        then T (tail d1 ++ tail d2) (cdat m)
        else error "wrong contraction2"
  where (d1,m1) = putFirstIdx n1 t1
        (d2,m2) = putFirstIdx n2 t2
        m = multiply RowMajor (trans m1) m2

-- | contraction of a tensor along two given indices
contraction1 :: (Field t, Num t) => Tensor t -> IdxName -> IdxName -> Tensor t
contraction1 t name1 name2 =
    if compatIdx t name1 t name2
        then addT y
        else error $ "wrong contraction1: "++(shdims$dims$t)++" "++name1++" "++name2
    where d = dims (head y)
          x = (map (flip parts name2) (parts t name1))
          y = map head $ zipWith drop [0..] x

-- | contraction of a tensor along a repeated index
contraction1c :: (Field t, Num t) => Tensor t -> IdxName -> Tensor t
contraction1c t n = contraction1 renamed n' n
    where n' = n++"'" -- hmmm
          renamed = withIdx t auxnames
          auxnames = h ++ (n':r)
          (h,_:r) = break (==n) (map idxName (dims t))

-- | alternative and inefficient version of contraction2
contraction2' :: (Field t, Enum t, Num t) => Tensor t -> IdxName -> Tensor t -> IdxName -> Tensor t
contraction2' t1 n1 t2 n2 =
    if compatIdx t1 n1 t2 n2
        then contraction1 (rawProduct t1 t2) n1 n2
        else error "wrong contraction'"

-- | applies a sequence of contractions
contractions t pairs = foldl' contract1b t pairs
    where contract1b t (n1,n2) = contraction1 t n1 n2

-- | applies a sequence of contractions of same index
contractionsC t is = foldl' contraction1c t is


-- | applies a contraction on the first indices of the tensors
contractionF t1 t2 = contraction2 t1 n1 t2 n2
    where n1 = fn t1
          n2 = fn t2
          fn = idxName . head . dims

-- | computes all compatible contractions of the product of two tensors that would arise if the index names were equal
possibleContractions :: (Num t, Field t) => Tensor t -> Tensor t -> [Tensor t]
possibleContractions t1 t2 = [ contraction2 t1 n1 t2 n2 | n1 <- names t1, n2 <- names t2, compatIdx t1 n1 t2 n2 ]


liftTensor f (T d v) = T d (f v)

liftTensor2 f (T d1 v1) (T d2 v2) | compat d1 d2 = T d1 (f v1 v2)
                                  | otherwise = error "liftTensor2 with incompatible tensors"
    where compat a b = length a == length b


a |+| b = liftTensor2 add a b
addT l = foldl1' (|+|) l

-- | index transposition to a desired order. You can specify only a subset of the indices, which will be moved to the front of indices list
tridx :: (Field t) => [IdxName] -> Tensor t -> Tensor t
tridx [] t = t
tridx (name:rest) t = T (d:ds) (join ts) where
    ((_,d:_),_) = findIdx name t
    ps = map (tridx rest) (parts t name)
    ts = map ten ps
    ds = dims (head ps)

compatIdxAux :: IdxDesc -> IdxDesc -> Bool
compatIdxAux IdxDesc {idxDim = n1, idxType = t1}
             IdxDesc {idxDim = n2, idxType = t2}
    = t1 /= t2 && n1 == n2

compatIdx :: (Field t1, Field t) => Tensor t1 -> IdxName -> Tensor t -> IdxName -> Bool
compatIdx t1 n1 t2 n2 = compatIdxAux d1 d2 where
    d1 = head $ snd $ fst $ findIdx n1 t1
    d2 = head $ snd $ fst $ findIdx n2 t2

names :: Tensor t -> [IdxName]
names t = map idxName (dims t)


-- sent to Haskell-Cafe by Sebastian Sylvan
perms :: [t] -> [[t]]
perms [x] = [[x]]
perms xs = [y:ps | (y,ys) <- selections xs , ps <- perms ys]
selections []     = []
selections (x:xs) = (x,xs) : [(y,x:ys) | (y,ys) <- selections xs]

interchanges :: (Ord a) => [a] -> Int
interchanges ls = sum (map (count ls) ls)
    where count l p = length $ filter (>p) $ take pel l
              where Just pel = elemIndex p l

signature :: (Num t, Ord a) => [a] -> t
signature l | length (nub l) < length l =  0
            | even (interchanges l)     =  1
            | otherwise                 = -1

scalar x = T [] (fromList [x])
tensorFromVector tp v = T {dims = [IdxDesc (dim v) tp "1"], ten = v}
tensorFromMatrix tpr tpc m = T {dims = [IdxDesc (rows m) tpr "1",IdxDesc (cols m) tpc "2"]
                               , ten = cdat m}

tvector v = tensorFromVector Contravariant v
tcovector v = tensorFromVector Covariant v

antisym t = T (dims t) (ten (antisym' (withIdx t seqind)))
    where antisym' t = addT $ map (scsig . flip tridx t) (perms (names t))
          scsig t = scalar (signature (nms t)) `rawProduct` t
              where nms = map idxName . dims

norper t = rawProduct t (scalar (recip $ fromIntegral $ fact (rank t)))
antinorper t = rawProduct t (scalar (fromIntegral $ fact (rank t)))

wedge a b = antisym (rawProduct (norper a) (norper b))

normAT t = sqrt $ innerAT t t

innerAT t1 t2 = dot (ten t1) (ten t2) / fromIntegral (fact $ rank t1)

fact n = product [1..n]

seqind' = map return seqind
seqind = map show [1..]

leviCivita n = antisym $ foldl1 rawProduct $ zipWith withIdx auxbase seqind'
    where auxbase = map tcovector (toRows (ident n))

-- | obtains de dual of the exterior product of a list of X?
dualV vs = foldl' contractionF (leviCivita n) vs
    where n = idxDim . head . dims . head $ vs

-- | raises or lowers all the indices of a tensor (with euclidean metric)
raise (T d v) = T (map raise' d) v
    where raise' idx@IdxDesc {idxType = Covariant } = idx {idxType = Contravariant}
          raise' idx@IdxDesc {idxType = Contravariant } = idx {idxType = Covariant}
-- | raises or lowers all the indices of a tensor with a given an (inverse) metric
raiseWith = undefined

dualg f t = f (leviCivita n) `okContract` withIdx t seqind `rawProduct` x
    where n = idxDim . head . dims $ t
          x = scalar (recip $ fromIntegral $ fact (rank t))

-- | obtains the dual of a multivector
dual t = dualg id t

-- | obtains the dual of a multicovector (with euclidean metric)
codual t = dualg raise t

-- | shows only the relevant components of an antisymmetric tensor
niceAS t = filter ((/=0.0).fst) $ zip vals base
    where vals = map ((`at` 0).ten.foldl' partF t) (map (map pred) base)
          base = asBase r n
          r = length (dims t)
          n = idxDim . head . dims $ t
          partF t i = part t (name,i) where name = idxName . head . dims $ t
          asBase r n = filter (\x-> (x==nub x && x==sort x)) $ sequence $ replicate r [1..n]

-- | renames specified indices of a tensor (repeated indices get the same name)
idxRename (T d v) l = T (map (ir l) d) v
    where ir l i = case lookup (idxName i) l of
                       Nothing -> i
                       Just r  -> i {idxName = r}

-- | renames all the indices in the current order (repeated indices may get different names)
withIdx (T d v) l = T d' v
    where d' = zipWith f d l
          f i n = i {idxName=n}

desiredContractions2 t1 t2 = [ (n1,n2) | n1 <- names t1, n2 <- names t2, n1==n2]

desiredContractions1 t = [ n1 | (a,n1) <- x , (b,n2) <- x, a/=b, n1==n2]
    where x = zip [0..] (names t)

okContract t1 t2 = r where
    t1r = contractionsC t1 (desiredContractions1 t1)
    t2r = contractionsC t2 (desiredContractions1 t2)
    cs = desiredContractions2 t1r t2r
    r = case cs of
        [] -> rawProduct t1r t2r
        (n1,n2):as -> contractionsC (contraction2 t1r n1 t2r n2) (map fst as)
