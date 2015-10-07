{-# LANGUAGE UnicodeSyntax
           , MultiParamTypeClasses
           , FunctionalDependencies
           , FlexibleInstances
           , FlexibleContexts
--           , OverlappingInstances
           , UndecidableInstances #-}

import Numeric.LinearAlgebra

class Scaling a b c | a b -> c where
 -- ^ 0x22C5	8901	DOT OPERATOR, scaling
 infixl 7 ⋅
 (⋅) :: a -> b -> c

instance (Num t) => Scaling t t t where
    (⋅) = (*)

instance Container Vector t => Scaling t (Vector t) (Vector t) where
    (⋅) = scale

instance Container Vector t => Scaling (Vector t) t (Vector t) where
    (⋅) = flip scale

instance (Num t, Container Vector t) => Scaling t (Matrix t) (Matrix t) where
    (⋅) = scale

instance (Num t, Container Vector t) => Scaling (Matrix t) t (Matrix t) where
    (⋅) = flip scale


class Mul a b c | a b -> c, a c -> b, b c -> a where
 -- ^ 0x00D7	215	MULTIPLICATION SIGN	×, contraction
 infixl 7 ×
 (×) :: a -> b -> c


-------



instance Product t => Mul (Vector t) (Vector t) t where
    (×) = udot

instance (Numeric t, Product t) => Mul (Matrix t) (Vector t) (Vector t) where
    (×) = (#>)

instance (Numeric t, Product t) => Mul (Vector t) (Matrix t) (Vector t) where
    (×) = (<#)

instance (Numeric t, Product t) => Mul (Matrix t) (Matrix t) (Matrix t) where
    (×) = (<>)


--instance Scaling a b c => Contraction a b c where
--    (×) = (⋅)

--------------------------------------------------------------------------------

class Outer a
  where
    infixl 7 ⊗
    -- | unicode 0x2297 8855 CIRCLED TIMES	⊗
    --
    -- vector outer product and matrix Kronecker product
    (⊗) :: Product t => a t -> a t -> Matrix t

instance Outer Vector where
    (⊗) = outer

instance Outer Matrix where
    (⊗) = kronecker

--------------------------------------------------------------------------------


v = 3 |> [1..] :: Vector Double

m = (3 >< 3) [1..] :: Matrix Double

s = 3 :: Double

a = s ⋅ v × m × m × v ⋅ s

--b = (v ⊗ m) ⊗ (v ⊗ m)

--c = v ⊗ m ⊗ v ⊗ m

d = s ⋅ (3 |> [10,20..] :: Vector Double)

u = fromList [3,0,5]
w = konst 1 (2,3) :: Matrix Double

main = do
    print $ (scale s v <# m) `udot` v
    print $ scale s v `udot` (m #> v)
    print $ s * ((v <# m) `udot` v)
    print $ s ⋅ v × m × v
    print a
--    print (b == c)
    print d
    print $ asColumn u ⊗ w
    print $ w ⊗ asColumn u

