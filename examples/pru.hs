--{-# OPTIONS_GHC  #-}
--module Main where
{-
import Data.Packed.Internal
import Data.Packed.Internal.Vector
import Data.Packed.Internal.Matrix
import Data.Packed.Tensor
import Data.Packed.Matrix
import GSL.Vector
import LAPACK
import Data.List(foldl')

import Complex
import Numeric(showGFloat)

import Foreign.Storable
-}

import GSL
import Data.List(foldl', foldl1')
import Data.Packed.Internal.Tensor
import Data.Packed.Tensor
import Complex


main = do
    print $ foldl part t [("p",1),("q",0),("r",2)]
    print $ foldl part t [("p",1),("r",2),("q",0)]
    print $ foldl part t $ reverse [("p",1),("r",2),("q",0)]
    pru

t = T [IdxDesc 4 Covariant "p",IdxDesc 2 Covariant "q" ,IdxDesc 3 Contravariant "r"]
    $ fromList [1..24::Double]


t1 = T [IdxDesc 4 Covariant "p",IdxDesc 4 Contravariant "q" ,IdxDesc 2 Covariant "r"]
    $ fromList [1..32::Double]
t2 = T [IdxDesc 4 Covariant "p",IdxDesc 4 Contravariant "q"] 
    $ fromList [1..16::Double]

vector n v = tvector n (fromList v) :: Tensor Double

kron n = tensorFromMatrix (Contravariant,"k1") (Covariant,"k2") (ident n)

tensorFromTrans m = tensorFromMatrix (Contravariant,"1") (Covariant,"2") m

tam = (3><3) [1..9]
tbm = (3><3) [11..19]

ta = tensorFromMatrix (Contravariant,"a1") (Covariant,"a2") tam :: Tensor Double
tb = tensorFromMatrix (Contravariant,"b1") (Covariant,"b2") tbm :: Tensor Double

delta i j | i==j      = 1
          | otherwise = 0

e i n = fromList [ delta k i | k <- [1..n]]

diagl = diag.fromList

td = tensorFromMatrix (Contravariant,"i") (Covariant,"j") $ diagl [1..4] :: Tensor Double

tn = tensorFromMatrix (Contravariant,"i") (Covariant,"j") $ (2><3) [1..6] :: Tensor Double
tt = tensorFromMatrix (Contravariant,"i") (Covariant,"j") $ (2><3) [1..6] :: Tensor Double

tq = T  [IdxDesc 3 Covariant "p",IdxDesc 2 Covariant "q" ,IdxDesc 2 Covariant "r"]
    $ fromList [11 .. 22] :: Tensor Double

r1 = contraction tt "j" tq "p"
r1' = contraction' tt "j" tq "p"

pru = do
    mapM_ (putStrLn.shdims.dims.normal) (possibleContractions t1 t2)
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
    putStrLn "--------------------------------------------"
    print $ flatten $ tam <> tbm
    print $ contractions (ta <*> tb <*> kron 3) [("a2","k1'"),("b1'","k2'")]
    print $ contraction ta "a2" tb "b1"
    print $ normal $ contractions (ta <*> tb) [("a2","b1'")]
    print $ normal $ contraction' ta "a2" tb "b1"
    putStrLn "--------------------------------------------"
    print $ raise $ dualMV $ raise $ dualMV (x1/\x2) /\ dualV [x3,x4]
    putStrLn "--------------------------------------------"
    print $ foldl' contractionF (leviCivita 4) [y1,y2]
    print $ contractions (leviCivita 4 <*> (y1/\y2)) [("1","p'"),("2'","q''")] <*> (scalar (recip $ fact 2))
    print $ foldl' contractionF (leviCivita 4) [y1,y2,y3,y5]
    print $ contractions (leviCivita 4 <*> (y1/\y2/\y3/\y5)) [("1","p'"),("2'","q''"),("3'","r''"),("4'","t''")] <*> (scalar (recip $ fact 4))
    print $ dim $ ten $ leviCivita 4 <*> (y1/\y2/\y3/\y5)
    print $ innerAT (leviCivita 4) (y1/\y2/\y3/\y5)


y5 = vector "t" [0,1,-2,0]

u = vector "p" [1,1,0]
v = vector "q" [0,1,1]
w = vector "r" [1,0,1]

uv = u /\ v
uw = u /\ w


l1 = vector "p" [0,0,0,1]
l2 = vector "q" [1,0,0,1]
l3 = vector "r" [0,1,0,1]

dual1 = foldl' contractionF (leviCivita 3) [u,v]
dual2 = foldl' contractionF (leviCivita 3) [u,v,w]



dual1' = prod (foldl' contract1b ((leviCivita 3) <*> (u /\ v)) [("1","p'"),("2'","q''")]) (scalar (recip $ fact 2))
dual2' = prod (foldl' contract1b ((leviCivita 3) <*> (u /\ v /\ w)) [("1","p'"),("2'","q''"),("3'","r''")]) (scalar (recip $ fact 3))


x1 = vector "p" [0,0,1]
x2 = vector "q" [2,2,2]
x3 = vector "r" [-3,-1,-1]
x4 = vector "s" [12,0,3]


-- intersection of two lines :-)
-- > raise $ dualMV $ raise $ dualMV (x1/\x2) /\ dualV [x3,x4]
--(3'^[3]) [24.0,24.0,12.0]

y1 = vector "p" [0,0,0,1]
y2 = vector "q" [2,2,0,2]
y3 = vector "r" [-3,-1,0,-1]
y4 = vector "s" [12,0,0,3]

-- why not in R^4? 
-- > raise $ dualMV $ raise $ dualMV (y1/\y2) /\ dualV [y3,y4]
-- scalar 0.0
-- it seems that the sum of ranks must be greater than n :(


z1 = vector "p" [0,0,0,1]
z2 = vector "q" [1,0,0,1]
z3 = vector "r" [0,1,0,1]
z4 = vector "s" [0,0,1,1]
