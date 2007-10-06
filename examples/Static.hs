{-# OPTIONS_GHC -fglasgow-exts -fth -fallow-overlapping-instances -fallow-undecidable-instances #-}

module Static where

import Language.Haskell.TH
import Numeric.LinearAlgebra
import Foreign
import Language.Haskell.TH.Syntax
import Data.Packed.Internal(Vector(..),Matrix(..))

instance Lift Double where
  lift x = return (LitE (RationalL (toRational x)))

instance Lift (Ptr Double) where
    lift p = [e| p |]

instance Lift (ForeignPtr Double) where
    lift p = [e| p |]

instance (Lift a, Storable a, Lift (Ptr a), Lift (ForeignPtr a)) => Lift (Vector a ) where
    lift (V n fp p) = [e| V $(lift n) $(lift fp) $(lift p) |]

instance (Lift (Vector a)) => Lift (Matrix a) where
    lift (MC r c v vt) = [e| MC $(lift r) $(lift c) $(lift v) $(lift vt) |]
    lift (MF r c v vt) = [e| MF $(lift r) $(lift c) $(lift v) $(lift vt) |]

tdim :: Int -> ExpQ
tdim 0 = [| Z |]
tdim n = [| S $(tdim (n-1)) |]


data Z = Z deriving Show
data S a = S a deriving Show

class Dim a

instance Dim Z
instance Dim a => Dim (S a)

class Sum a b c | a b -> c -- , a c -> b, b c -> a

instance Sum Z a a
instance Sum a Z a
instance Sum a b c => Sum a (S b) (S c)

newtype SVec d t = SVec (Vector t) deriving Show
newtype SMat r c t = SMat (Matrix t) deriving Show

createl :: d -> [Double] -> SVec d Double
createl d l = SVec (fromList l)

createv :: Storable t => d -> Vector t -> SVec d t
createv d v = SVec v

--vec'' v = [|createv ($(tdim (dim v))) v|]

vec' :: [Double] -> ExpQ
vec' d = [| createl ($(tdim (length d))) d |]


createm :: (Dim r, Dim c) => r -> c -> (Matrix Double) -> SMat r c Double
createm _ _ m = SMat m

createml :: (Dim r, Dim c) => r -> c -> Int -> Int -> [Double] -> SMat r c Double
createml _ _ r c l = SMat ((r><c) l)

mat :: Int -> Int -> [Double] -> ExpQ
mat r c l = [| createml ($(tdim r)) ($(tdim c)) r c l |]

vec :: [Double] -> ExpQ
vec d = mat (length d) 1 d


mat' :: Matrix Double -> ExpQ
mat' m = [| createm ($(tdim (rows m))) ($(tdim (cols m))) m |]

covec :: [Double] -> ExpQ
covec d = mat 1 (length d) d

scalar :: SMat (S Z) (S Z) Double -> Double
scalar (SMat m) = flatten m @> 0

v = fromList [1..5] :: Vector Double
l = [1,1.5..5::Double]

k = [11..30::Int]

rawv (SVec v) = v
raw (SMat m) = m

liftStatic :: (Matrix a -> Matrix b -> Matrix c) -> SMat dr dc a -> SMat dr dc b -> SMat dr dc c
liftStatic f a b = SMat (f (raw a) (raw b))

a |+| b = liftStatic (+) a b

prod :: SMat r k Double -> SMat k c Double -> SMat r c Double
prod a b = SMat (raw a <> raw b)

strans :: SMat r c Double -> SMat c r Double
strans = SMat . trans . raw

sdot a b = scalar (prod a b)

jv :: (Field t, Sum r1 r2 r3) => SMat r1 c t -> SMat r2 c t -> SMat r3 c t
jv a b = SMat ((raw a) <-> (raw b))

-- curiously, we cannot easily fold jv because the matrics are not of the same type.

jh a b = strans (jv (strans a) (strans b))


homog :: (Field t) => SMat r c t -> SMat (S r) c t
homog m = SMat (raw m <-> constant 1 (cols (raw m)))

inhomog :: (Linear Vector t) => SMat (S (S r)) c t -> SMat r c t
inhomog (SMat m) = SMat (sm <> d) where
    sm = takeRows r' m
    d = diag $ 1 / (flatten $ dropRows r' m)
    r' = rows m -1


ht t vs = inhomog (t `prod` homog vs)

