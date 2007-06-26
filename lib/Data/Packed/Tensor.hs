-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Packed.Tensor
-- Copyright   :  (c) Alberto Ruiz 2007
-- License     :  GPL-style
--
-- Maintainer  :  Alberto Ruiz <aruiz@um.es>
-- Stability   :  provisional
-- Portability :  portable
--
-- Tensors
--
-----------------------------------------------------------------------------

module Data.Packed.Tensor where

import Data.Packed.Matrix
import Data.Packed.Internal
import Complex
import Data.List(transpose,intersperse,sort,elemIndex,nub,foldl',foldl1')

scalar x = T [] (fromList [x])
tensorFromVector (tp,nm) v = T {dims = [IdxDesc (dim v) tp nm]
                                       , ten = v}
tensorFromMatrix (tpr,nmr) (tpc,nmc) m = T {dims = [IdxDesc (rows m) tpr nmr,IdxDesc (cols m) tpc nmc]
                                           , ten = cdat m}

scsig t = scalar (signature (nms t)) `prod` t
    where nms = map idxName . dims

antisym' t = addT $ map (scsig . flip tridx t) (perms (names t))


auxrename (T d v) = T d' v
    where d' = [IdxDesc n c (show (pos q)) | IdxDesc n c q <- d]
          pos n = i where Just i = elemIndex n nms
          nms = map idxName d

antisym t = T (dims t) (ten (antisym' (auxrename t)))


norper t = prod t (scalar (recip $ fromIntegral $ product [1 .. length (dims t)]))
antinorper t = prod t (scalar (fromIntegral $ product [1 .. length (dims t)]))


tvector n v = tensorFromVector (Contravariant,n) v
tcovector n v = tensorFromVector (Covariant,n) v

wedge a b = antisym (prod (norper a) (norper b))

a /\ b = wedge a b

a <*> b = normal $ prod a b

normAT t = sqrt $ innerAT t t

innerAT t1 t2 = dot (ten t1) (ten t2) / fromIntegral (fact $ length $ dims t1)

fact n = product [1..n]

leviCivita n = antisym $ foldl1 prod $ zipWith tcovector (map show [1,2..]) (toRows (ident n))

contractionF t1 t2 = contraction t1 n1 t2 n2
    where n1 = fn t1
          n2 = fn t2
          fn = idxName . head . dims


dualV vs = foldl' contractionF (leviCivita n) vs
    where n = idxDim . head . dims . head $ vs

raise (T d v) = T (map raise' d) v
    where raise' idx@IdxDesc {idxType = Covariant } = idx {idxType = Contravariant}
          raise' idx@IdxDesc {idxType = Contravariant } = idx {idxType = Covariant}

dualMV t = prod (foldl' contract1b (lc <*> t) ds) (scalar (recip $ fromIntegral $ fact (length ds)))
    where
        lc = leviCivita n
        nms1 = map idxName (dims lc)
        nms2 = map ((++"'").idxName) (dims t)
        ds = zip nms1 nms2
        n = idxDim . head . dims $ t

contract1b t (n1,n2) = contract1 t n1 n2

contractions t pairs = foldl' contract1b t pairs

asBase r n = filter (\x-> (x==nub x && x==sort x)) $ sequence $ replicate r [1..n]

partF t i = part t (name,i) where name = idxName . head . dims $ t

niceAS t = filter ((/=0.0).fst) $ zip vals base
    where vals = map ((`at` 0).ten.foldl' partF t) (map (map pred) base)
          base = asBase r n
          r = length (dims t)
          n = idxDim . head . dims $ t
