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
import Data.Packed.Internal.Vector
import Data.Packed.Internal.Matrix
import Foreign.Storable
import Data.List(sort,elemIndex,nub)

data IdxTp = Covariant | Contravariant deriving (Show,Eq)

data Tensor t = T { dims   :: [(Int,(IdxTp,String))]
                  , ten    :: Vector t
                  }

rank = length . dims

instance (Show a,Storable a) => Show (Tensor a) where
    show T {dims = [], ten = t} = "scalar "++show (t `at` 0)
    show T {dims = ds, ten = t} = "("++shdims ds ++") "++ show (toList t)


shdims [(n,(t,name))] = name ++ sym t ++"["++show n++"]"
    where sym Covariant     = "_"
          sym Contravariant = "^"
shdims (d:ds) = shdims [d] ++ "><"++ shdims ds



findIdx name t = ((d1,d2),m) where
    (d1,d2) = span (\(_,(_,n)) -> n /=name) (dims t)
    c = product (map fst d2)
    m = matrixFromVector RowMajor c (ten t)

putFirstIdx name t = (nd,m')
    where ((d1,d2),m) = findIdx name t
          m' = matrixFromVector RowMajor c $ cdat $ trans m
          nd = d2++d1
          c = dim (ten t) `div` (fst $ head d2)

part t (name,k) = if k<0 || k>=l
                    then error $ "part "++show (name,k)++" out of range in "++show t
                    else T {dims = ds, ten = toRows m !! k}
    where (d:ds,m) = putFirstIdx name t
          (l,_) = d

parts t name = map f (toRows m)
    where (d:ds,m) = putFirstIdx name t
          (l,_) = d
          f t = T {dims=ds, ten=t}

concatRename l1 l2 = l1 ++ map ren l2 where
    ren (n,(t,s)) = if {- s `elem` fs -} True then (n,(t,s++"'")) else (n,(t,s))
    fs = map (snd.snd) l1

prod (T d1 v1) (T d2 v2) = T (concatRename d1 d2) (outer v1 v2)

contraction t1 n1 t2 n2 =
    if compatIdx t1 n1 t2 n2
        then T (concatRename (tail d1) (tail d2)) (cdat m)
        else error "wrong contraction'"
  where (d1,m1) = putFirstIdx n1 t1
        (d2,m2) = putFirstIdx n2 t2
        m = multiply RowMajor (trans m1) m2

sumT ls = foldl (zipWith (+)) [0,0..] (map (toList.ten) ls)

contract1 t name1 name2 = T d $ fromList $ sumT y
    where d = dims (head y)
          x = (map (flip parts name2) (parts t name1))
          y = map head $ zipWith drop [0..] x

contraction' t1 n1 t2 n2 =
    if compatIdx t1 n1 t2 n2
        then contract1 (prod t1 t2) n1 (n2++"'")
        else error "wrong contraction'"

tridx [] t = t
tridx (name:rest) t = T (d:ds) (join ts) where
    ((_,d:_),_) = findIdx name t
    ps = map (tridx rest) (parts t name)
    ts = map ten ps
    ds = dims (head ps)

compatIdxAux (n1,(t1,_)) (n2, (t2,_)) = t1 /= t2 && n1 == n2

compatIdx t1 n1 t2 n2 = compatIdxAux d1 d2 where
    d1 = head $ snd $ fst $ findIdx n1 t1
    d2 = head $ snd $ fst $ findIdx n2 t2

names t = sort $ map (snd.snd) (dims t)

normal t = tridx (names t) t

contractions t1 t2 = [ contraction t1 n1 t2 n2 | n1 <- names t1, n2 <- names t2, compatIdx t1 n1 t2 n2 ]

-- sent to Haskell-Cafe by Sebastian Sylvan
perms [x] = [[x]]
perms xs = [y:ps | (y,ys) <- selections xs , ps <- perms ys]
selections []     = []
selections (x:xs) = (x,xs) : [(y,x:ys) | (y,ys) <- selections xs]


interchanges ls = sum (map (count ls) ls)
    where count l p = n
              where Just pel = elemIndex p l
                    n = length $ filter (>p) $ take pel l

signature l | length (nub l) < length l =  0
            | even (interchanges l)     =  1
            | otherwise                 = -1
