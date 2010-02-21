{-# LANGUAGE ForeignFunctionInterface #-}

module Numeric.LinearProgramming(
    Optimization(..),
    Bound(..),
    (#),
    Coeffs(..),
    Solution(..),
    simplex,
) where

import Numeric.LinearAlgebra
import Data.Packed.Development
import Foreign(Ptr,unsafePerformIO)
import Foreign.C.Types(CInt)
import Data.List((\\),sortBy)
import Data.Function(on)

--import Debug.Trace
--debug x = trace (show x) x

-----------------------------------------------------

-- | Coefficient of a variable
(#) :: Double -> Int -> (Double,Int)
infixl 5 #
(#) = (,)

data Bound x =  x :<: Double 
             |  x :>: Double
             |  x :&: (Double,Double)
             |  x :==: Double
             |  Free x
             deriving Show

data Solution = Undefined
              | Feasible (Double, [Double])
              | Infeasible (Double, [Double])
              | NoFeasible
              | Optimal (Double, [Double])
              | Unbounded
              deriving Show
            
data Coeffs = Dense  [ Bound [Double] ]
            | Sparse [ Bound [(Double,Int)] ]

data Optimization = Maximize [Double]
                  | Minimize [Double]

simplex :: Optimization -> Coeffs -> [Bound Int] -> Solution

simplex opt (Dense constr) bnds = extract sg sol where
    sol = simplexDense (mkConstrD objfun constr) (mkBoundsD constr bnds)
    (sg, objfun) = case opt of
        Maximize x -> (1 ,x)
        Minimize x -> (-1, (map negate x))

simplex opt (Sparse constr) bnds = extract sg sol where
    sol = simplexSparse m n (mkConstrS objfun constr) (mkBoundsS constr bnds)
    n = length objfun
    m = length constr
    (sg, objfun) = case opt of
        Maximize x -> (1 ,x)
        Minimize x -> (-1, (map negate x))

extract :: Double -> Vector Double -> Solution
extract sg sol = r where
    z = sg * (sol@>1)
    v = toList $ subVector 2 (dim sol -2) sol
    r = case round(sol@>0)::Int of
          1 -> Undefined
          2 -> Feasible (z,v)
          3 -> Infeasible (z,v)
          4 -> NoFeasible
          5 -> Optimal (z,v)
          6 -> Unbounded
          _ -> error "simplex: solution type unknown"

-----------------------------------------------------

obj :: Bound t -> t
obj (x :<: _)  = x
obj (x :>: _)  = x
obj (x :&: _)  = x
obj (x :==: _) = x
obj (Free x)   = x

tb :: Bound t -> Double
tb (_ :<: _)  = glpUP
tb (_ :>: _)  = glpLO
tb (_ :&: _)  = glpDB
tb (_ :==: _) = glpFX
tb (Free _)   = glpFR

lb :: Bound t -> Double
lb (_ :<: _)     = 0
lb (_ :>: a)     = a
lb (_ :&: (a,_)) = a
lb (_ :==: a)    = a
lb (Free _)      = 0

ub :: Bound t -> Double
ub (_ :<: a)     = a
ub (_ :>: _)     = 0
ub (_ :&: (_,a)) = a
ub (_ :==: a)    = a
ub (Free _)      = 0

mkBound1 :: Bound t -> [Double]
mkBound1 b = [tb b, lb b, ub b]

mkBound2 :: Bound t -> (t, [Double])
mkBound2 b = (obj b, mkBound1 b)

mkBoundsD :: [Bound [a]] -> [Bound Int] -> Matrix Double
mkBoundsD b1 b2 = fromLists (cb++vb) where
    c = length (obj (head b1))
    gv = map obj b2
    rv = [1..c] \\ gv
    vb = map snd $ sortBy (compare `on` fst) $ map (mkBound2 . (:>: 0)) rv ++ map mkBound2 b2
    cb = map mkBound1 b1

mkConstrD :: [Double] -> [Bound [Double]] -> Matrix Double
mkConstrD f b1 = fromLists (f : map obj b1)

mkBoundsS :: [Bound [(Double, Int)]] -> [Bound Int] -> Matrix Double
mkBoundsS b1 b2 = fromLists (cb++vb) where
    c = maximum $ map snd $ concatMap obj b1
    gv = map obj b2
    rv = [1..c] \\ gv
    vb = map snd $ sortBy (compare `on` fst) $ map (mkBound2 . (:>: 0)) rv ++ map mkBound2 b2
    cb = map mkBound1 b1

mkConstrS :: [Double] -> [Bound [(Double, Int)]] -> Matrix Double
mkConstrS objfun constr = fromLists (ob ++ co) where
    ob = map (([0,0]++).return) objfun
    co = concat $ zipWith f [1::Int ..] (map obj constr)
    f k = map (g k)
    g k (c,v) = [fromIntegral k, fromIntegral v,c]

-----------------------------------------------------

foreign import ccall "c_simplex_dense" c_simplex_dense
    :: CInt -> CInt -> Ptr Double    -- coeffs
    -> CInt -> CInt -> Ptr Double    -- bounds
    -> CInt -> Ptr Double            -- result
    -> IO CInt                       -- exit code

simplexDense :: Matrix Double -> Matrix Double -> Vector Double
simplexDense c b = unsafePerformIO $ do
    s <- createVector (2+cols c)
    app3 c_simplex_dense mat (cmat c) mat (cmat b) vec s "c_simplex_dense"
    return s

foreign import ccall "c_simplex_sparse" c_simplex_sparse
    :: CInt -> CInt                  -- rows and cols
    -> CInt -> CInt -> Ptr Double    -- coeffs
    -> CInt -> CInt -> Ptr Double    -- bounds
    -> CInt -> Ptr Double            -- result
    -> IO CInt                       -- exit code

simplexSparse :: Int -> Int -> Matrix Double -> Matrix Double -> Vector Double
simplexSparse m n c b = unsafePerformIO $ do
    s <- createVector (2+n)
    let fi = fromIntegral
    app3 (c_simplex_sparse (fi m) (fi n)) mat (cmat c) mat (cmat b) vec s "c_simplex_sparse"
    return s

glpFR, glpLO, glpUP, glpDB, glpFX :: Double
glpFR = 0
glpLO = 1
glpUP = 2
glpDB = 3
glpFX = 4

{- Raw format of coeffs

simplexDense

((3+1) >< 3)
  [ 10, 6, 4
  ,  1, 1, 1
  , 10, 4, 5
  ,  2, 2, 6 :: Double]

simplexSparse

(12><3)
 [ 0.0, 0.0, 10.0
 , 0.0, 0.0,  6.0
 , 0.0, 0.0,  4.0
 , 1.0, 1.0,  1.0
 , 1.0, 2.0,  1.0
 , 1.0, 3.0,  1.0
 , 2.0, 1.0, 10.0
 , 2.0, 2.0,  4.0
 , 2.0, 3.0,  5.0
 , 3.0, 1.0,  2.0
 , 3.0, 2.0,  2.0
 , 3.0, 3.0,  6.0 ]
  
bounds = (6><3)
  [ glpUP,0,100
  , glpUP,0,600
  , glpUP,0,300
  , glpLO,0,0
  , glpLO,0,0
  , glpLO,0,0 ]
  
-}

