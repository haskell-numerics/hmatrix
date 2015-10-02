-- vectorized boolean operations defined in terms of step or cond

{-# LANGUAGE FlexibleContexts #-}

import Numeric.LinearAlgebra

infix  4  .==., ./=., .<., .<=., .>=., .>.
infixr 3  .&&.
infixr 2  .||.

-- specialized for Int result
cond'
    :: (Element t, Ord t, Container c I, Container c t)
    => c t -> c t -> c I -> c I -> c I -> c I
cond' = cond

a .<.  b = cond' a b 1 0 0
a .<=. b = cond' a b 1 1 0
a .==. b = cond' a b 0 1 0
a ./=. b = cond' a b 1 0 1
a .>=. b = cond' a b 0 1 1
a .>.  b = cond' a b 0 0 1

a .&&. b  = step (a*b)
a .||. b  = step (a+b)
no a      = 1-a
xor a b   = a ./=. b
equiv a b = a .==. b
imp a b   = no a .||. b

taut x = minElement x == 1

minEvery a b = cond a b a a b
maxEvery a b = cond a b b b a

-- examples

clip a b x = cond y b y y b where y = cond x a a x x

eye n = ident n :: Matrix R

m = (3><4) [1..] :: Matrix R

p = fromList [0,0,1,1] :: Vector I
q = fromList [0,1,0,1] :: Vector I

main = do
    print $ find (>6) m
    disp 3 $ assoc (6,8) 7 $ zip (find (/=0) (eye 5)) [10..]
    disp 3 $ accum (eye 5) (+) [((0,2),3), ((3,1),7), ((1,1),1)]
    print $ m .>=. 10  .||.  m .<. 4
    (print . fromColumns) [p, q, p.&&.q, p .||.q, p `xor` q, p `equiv` q, p `imp` q]
    print $ taut $ (p `imp` q ) `equiv` (no q `imp` no p)
    print $ taut $ (xor p q) `equiv` (p .&&. no q .||. no p .&&. q)
    disp 3 $ clip 3 8 m
    print $ col [1..7] .<=. row [1..5]
    print $ cond (col [1..3]) (row [1..4]) m 50 (3*m)

