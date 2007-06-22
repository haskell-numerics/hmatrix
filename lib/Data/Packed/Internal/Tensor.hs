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

import Data.Packed.Internal.Common
import Data.Packed.Internal.Vector
import Data.Packed.Internal.Matrix
import Foreign.Storable
import Data.List(sort,elemIndex,nub)

data IdxType = Covariant | Contravariant deriving (Show,Eq)

type IdxName = String

data IdxDesc = IdxDesc { idxDim  :: Int,
                         idxType :: IdxType,
                         idxName :: IdxName }

data Tensor t = T { dims   :: [IdxDesc]
                  , ten    :: Vector t
                  }

rank :: Tensor t -> Int
rank = length . dims

instance (Show a,Storable a) => Show (Tensor a) where
    show T {dims = [], ten = t} = "scalar "++show (t `at` 0)
    show T {dims = ds, ten = t} = "("++shdims ds ++") "++ show (toList t)


shdims :: [IdxDesc] -> String
shdims [IdxDesc n t name] = name ++ sym t ++"["++show n++"]"
    where sym Covariant     = "_"
          sym Contravariant = "^"
shdims (d:ds) = shdims [d] ++ "><"++ shdims ds


findIdx :: (Field t) => IdxName -> Tensor t
        -> (([IdxDesc], [IdxDesc]), Matrix t)
findIdx name t = ((d1,d2),m) where
    (d1,d2) = span (\d -> idxName d /= name) (dims t)
    c = product (map idxDim d2)
    m = matrixFromVector RowMajor c (ten t)

putFirstIdx :: (Field t) => String -> Tensor t -> ([IdxDesc], Matrix t)
putFirstIdx name t = (nd,m')
    where ((d1,d2),m) = findIdx name t
          m' = matrixFromVector RowMajor c $ cdat $ trans m
          nd = d2++d1
          c = dim (ten t) `div` (idxDim $ head d2)

part :: (Field t) => Tensor t -> (IdxName, Int) -> Tensor t
part t (name,k) = if k<0 || k>=l
                    then error $ "part "++show (name,k)++" out of range" -- in "++show t
                    else T {dims = ds, ten = toRows m !! k}
    where (d:ds,m) = putFirstIdx name t
          l = idxDim d

parts :: (Field t) => Tensor t -> IdxName -> [Tensor t]
parts t name = map f (toRows m)
    where (d:ds,m) = putFirstIdx name t
          l = idxDim d
          f t = T {dims=ds, ten=t}

concatRename :: [IdxDesc] -> [IdxDesc] -> [IdxDesc]
concatRename l1 l2 = l1 ++ map ren l2 where
    ren idx = if {- s `elem` fs -} True then idx {idxName = idxName idx ++ "'"} else idx
    fs = map idxName l1

prod :: (Field t, Num t) => Tensor t -> Tensor t -> Tensor t
prod (T d1 v1) (T d2 v2) = T (concatRename d1 d2) (outer' v1 v2)

contraction :: (Field t, Num t) => Tensor t -> IdxName -> Tensor t -> IdxName -> Tensor t
contraction t1 n1 t2 n2 =
    if compatIdx t1 n1 t2 n2
        then T (concatRename (tail d1) (tail d2)) (cdat m)
        else error "wrong contraction'"
  where (d1,m1) = putFirstIdx n1 t1
        (d2,m2) = putFirstIdx n2 t2
        m = multiply RowMajor (trans m1) m2

sumT :: (Storable t, Enum t, Num t) => [Tensor t] -> [t]
sumT ls = foldl (zipWith (+)) [0,0..] (map (toList.ten) ls)

contract1 :: (Num t, Enum t, Field t) => Tensor t -> IdxName -> IdxName -> Tensor t
contract1 t name1 name2 = T d $ fromList $ sumT y
    where d = dims (head y)
          x = (map (flip parts name2) (parts t name1))
          y = map head $ zipWith drop [0..] x

contraction' :: (Field t, Enum t, Num t) => Tensor t -> IdxName -> Tensor t -> IdxName -> Tensor t
contraction' t1 n1 t2 n2 =
    if compatIdx t1 n1 t2 n2
        then contract1 (prod t1 t2) n1 (n2++"'")
        else error "wrong contraction'"

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
names t = sort $ map idxName (dims t)

normal :: (Field t) => Tensor t -> Tensor t
normal t = tridx (names t) t

contractions :: (Num t, Field t) => Tensor t -> Tensor t -> [Tensor t]
contractions t1 t2 = [ contraction t1 n1 t2 n2 | n1 <- names t1, n2 <- names t2, compatIdx t1 n1 t2 n2 ]

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
