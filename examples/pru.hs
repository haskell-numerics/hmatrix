--{-# OPTIONS_GHC  #-}
--module Main where

import Data.Packed.Internal
import Data.Packed.Internal.Vector
import Data.Packed.Internal.Matrix
import Data.Packed.Internal.Tensor
import Data.Packed.Matrix
import GSL.Vector
import LAPACK

import Complex
import Numeric(showGFloat)
import Data.List(transpose,intersperse,sort,elemIndex,nub,foldl',foldl1')
import Foreign.Storable


vr = fromList [1..15::Double]
vc = fromList (map (\x->x :+ (x+1)) [1..15::Double])

mi = (2 >< 3) [1 .. 6::Int]
mz = (2 >< 3) [1,2,3,4,5,6:+(1::Double)]

ac = (2><3) [1 .. 6::Double]
bc = (3><4) [7 .. 18::Double]

af = (2>|<3) [1,4,2,5,3,6::Double]
bf = (3>|<4) [7,11,15,8,12,16,9,13,17,10,14,18::Double]


a |=| b = rows a == rows b &&
          cols a == cols b &&
          toList (cdat a) == toList (cdat b)

mulC a b = multiply RowMajor a b
mulF a b = multiply ColumnMajor a b

cc = mulC ac bf
cf = mulF af bc

r = mulC cc (trans cf)

rd = (2><2)
 [ 27736.0,  65356.0
 , 65356.0, 154006.0 ]



main = do
    print $ r |=| rd
    print $ foldl part t [("p",1),("q",0),("r",2)]
    print $ foldl part t [("p",1),("r",2),("q",0)]
    print $ foldl part t $ reverse [("p",1),("r",2),("q",0)]

t = T [(4,(Covariant,"p")),(2,(Covariant,"q")),(3,(Contravariant,"r"))] $ fromList [1..24::Double]



t1 = T [(4,(Covariant,"p")),(4,(Contravariant,"q")),(2,(Covariant,"r"))] $ fromList [1..32::Double]
t2 = T [(4,(Covariant,"p")),(4,(Contravariant,"q"))] $ fromList [1..16::Double]



addT ts = T (dims (head ts)) (fromList $ sumT ts)


delta i j | i==j      = 1
          | otherwise = 0

e i n = fromList [ delta k i | k <- [1..n]]

diagl = diag.fromList

scalar x = T [] (fromList [x])
tensorFromVector idx v = T {dims = [(dim v,idx)], ten = v}
tensorFromMatrix idxr idxc m = T {dims = [(rows m,idxr),(cols m,idxc)], ten = cdat m}

td = tensorFromMatrix (Contravariant,"i") (Covariant,"j") $ diagl [1..4] :: Tensor Double

tn = tensorFromMatrix (Contravariant,"i") (Covariant,"j") $ (2><3) [1..6] :: Tensor Double
tt = tensorFromMatrix (Contravariant,"i") (Covariant,"j") $ (2><3) [1..6] :: Tensor Double

tq = T [(3,(Covariant,"p")),(2,(Covariant,"q")),(2,(Covariant,"r"))] $ fromList [11 .. 22] :: Tensor Double

r1 = contraction tt "j" tq "p"
r1' = contraction' tt "j" tq "p"

pru = do
    mapM_ (putStrLn.shdims.dims.normal) (contractions t1 t2)
    let t1 = contraction tt "i" tq "q"
    print $ normal t1
    print $ foldl part t1 [("j",0),("p'",1),("r'",1)]
    let t2 = contraction' tt "i" tq "q"
    print $ normal t2
    print $ foldl part t2 [("j",0),("p'",1),("r'",1)]
    let t1 = contraction tq "q" tt "i"
    print $ normal t1
    print $ foldl part t1 [("j'",0),("p",1),("r",1)]
    let t2 = contraction' tq "q" tt "i"
    print $ normal t2
    print $ foldl part t2 [("j'",0),("p",1),("r",1)]

scsig t = scalar (signature (nms t)) `prod` t
    where nms = map (snd.snd) . dims

antisym' t = addT $ map (scsig . flip tridx t) (perms (names t))

{-
   where T d v = t
          t' = T d' v
          fixdim (T _ v) = T d v
          d' = [(n,(c,show (pos q))) | (n,(c,q)) <- d]
          pos n = i where Just i = elemIndex n nms
          nms = map (snd.snd) d
-}

auxrename (T d v) = T d' v
    where d' = [(n,(c,show (pos q))) | (n,(c,q)) <- d]
          pos n = i where Just i = elemIndex n nms
          nms = map (snd.snd) d

antisym t = T (dims t) (ten (antisym' (auxrename t)))


norper t = prod t (scalar (recip $ fromIntegral $ product [1 .. length (dims t)]))
antinorper t = prod t (scalar (fromIntegral $ product [1 .. length (dims t)]))


tvector n v = tensorFromVector (Contravariant,n) v
tcovector n v = tensorFromVector (Covariant,n) v

vector n v = tvector n (fromList v) :: Tensor Double

wedge a b = antisym (prod (norper a) (norper b))

a /\ b = wedge a b

a <*> b = normal $ prod a b

u = vector "p" [1,1,0]
v = vector "q" [0,1,1]
w = vector "r" [1,0,1]

uv = u /\ v
uw = u /\ w

normAT t = sqrt $ innerAT t t

innerAT t1 t2 = dot (ten t1) (ten t2) / fromIntegral (fact $ length $ dims t1)

det m = product $ toList s where (_,s,_) = svdR' m

fact n = product [1..n]

l1 = vector "p" [0,0,0,1]
l2 = vector "q" [1,0,0,1]
l3 = vector "r" [0,1,0,1]

leviCivita n = antisym $ foldl1 prod $ zipWith tcovector (map show [1..]) (toRows (ident n))

contractionF t1 t2 = contraction t1 n1 t2 n2
    where n1 = fn t1
          n2 = fn t2
          fn = snd . snd . head . dims


dual vs = foldl' contractionF (leviCivita n) vs
    where n = fst . head . dims . head $ vs


dual1 = foldl' contractionF (leviCivita 3) [u,v]
dual2 = foldl' contractionF (leviCivita 3) [u,v,w]


contract1b t (n1,n2) = contract1 t n1 n2

dual1' = prod (foldl' contract1b ((leviCivita 3) <*> (u /\ v)) [("1","p'"),("2'","q''")]) (scalar (recip $ fact 2))
dual2' = prod (foldl' contract1b ((leviCivita 3) <*> (u /\ v /\ w)) [("1","p'"),("2'","q''"),("3'","r''")]) (scalar (recip $ fact 3))