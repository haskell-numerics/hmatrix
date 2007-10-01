{-# OPTIONS -fno-monomorphism-restriction #-}

import Static
import Numeric.LinearAlgebra


x = $(vec [1,2])

y = $(vec [5,7])

z a = vec [a,a]

w = $(vec [1,2,3])

cx = $(covec [1,2,3])


t3 = $(tdim 3)

crm33 = createml t3 t3 3 3

rot a = crm33 [a,0,0,0,a,0,0,0,1]

--q = x |+| y |+| $(z 5)

m = $(mat 2 3 [1..6])

n = $(mat 3 5 [1..15])

infixl 7 <*>
(<*>) = prod

r1 = m <*> n
r2 = strans (strans n <*> strans m)

--r' = prod n m
