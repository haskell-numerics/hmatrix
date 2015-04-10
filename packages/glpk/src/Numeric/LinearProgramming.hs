{-# LANGUAGE ForeignFunctionInterface #-}

{- |
Module      :  Numeric.LinearProgramming
Copyright   :  (c) Alberto Ruiz 2010
License     :  GPL

Maintainer  :  Alberto Ruiz
Stability   :  provisional

This module provides an interface to the standard simplex algorithm.

For example, the following LP problem


maximize 4 x_1 - 3 x_2 + 2 x_3
subject to

2 x_1 +   x_2 <= 10

  x_2 + 5 x_3 <= 20

and

x_i >= 0


can be solved as follows:

@
import Numeric.LinearProgramming

prob = Maximize [4, -3, 2]

constr1 = Sparse [ [2\#1, 1\#2] :<=: 10
                 , [1\#2, 5\#3] :<=: 20
                 ]
@

>>> simplex prob constr1 []
Optimal (28.0,[5.0,0.0,4.0])


The coefficients of the constraint matrix can also be given in dense format:

@
constr2 = Dense [ [2,1,0] :<=: 10
                , [0,1,5] :<=: 20
                ]
@

Note that when using sparse constraints, coefficients cannot appear more than once in each constraint. You can alternatively use General which will automatically sum any duplicate coefficients when necessary.

@
constr3 = General [ [1\#1, 1\#1, 1\#2] :<=: 10
                  , [1\#2, 5\#3] :<=: 20
                  ]
@

By default all variables are bounded as @x_i >= 0@, but this can be
changed:

>>> simplex prob constr2 [ 2 :>=: 1, 3 :&: (2,7)]
Optimal (22.6,[4.5,1.0,3.8])

>>> simplex prob constr2 [Free 2]
Unbounded

The given bound for a variable completely replaces the default,
so @0 <= x_i <= b@ must be explicitly given as @i :&: (0,b)@.
Multiple bounds for a variable are not allowed, instead of
@[i :>=: a, i:<=: b]@ use @i :&: (a,b)@.

-}

module Numeric.LinearProgramming(
    simplex,
    exact,
    sparseOfGeneral,
    Optimization(..),
    Constraints(..),
    Bounds,
    Bound(..),
    (#),
    Solution(..)
) where

import Numeric.LinearAlgebra.HMatrix
import Numeric.LinearAlgebra.Devel hiding (Dense)
import Foreign(Ptr)
import System.IO.Unsafe(unsafePerformIO)
import Foreign.C.Types
import Data.List((\\),sortBy,nub)
import Data.Function(on)
import qualified Data.Map.Strict as Map

--import Debug.Trace
--debug x = trace (show x) x

-----------------------------------------------------

-- | Coefficient of a variable for a sparse and general representations of constraints.
(#) :: Double -> Int -> (Double,Int)
infixl 5 #
(#) = (,)

data Bound x =  x :<=: Double
             |  x :>=: Double
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

data Constraints = Dense   [ Bound [Double] ]
                 | Sparse  [ Bound [(Double,Int)] ]
                 | General [ Bound [(Double,Int)] ]

data Optimization = Maximize [Double]
                  | Minimize [Double]

type Bounds = [Bound Int]

-- | Convert a system of General constraints to one with unique coefficients. 
sparseOfGeneral :: Constraints -> Constraints
sparseOfGeneral (General cs) =
    Sparse $ map (\bl -> 
                      let cl = obj bl in
                      let cl_unique = foldr (\(c,t) m -> Map.insertWith (+) t c m) Map.empty cl in
                      withObj bl (Map.foldrWithKey' (\t c l -> (c#t) : l) [] cl_unique)) cs
sparseOfGeneral _ = error "sparseOfGeneral: a general system of constraints was expected"

simplex :: Optimization -> Constraints -> Bounds -> Solution

simplex opt (Dense   []) bnds = simplex opt (Sparse []) bnds
simplex opt (Sparse  []) bnds = simplex opt (Sparse [Free [0#1]]) bnds
simplex opt (General []) bnds = simplex opt (Sparse [Free [0#1]]) bnds

simplex opt (Dense constr) bnds = extract sg sol where
    sol = simplexSparse m n (mkConstrD sz objfun constr) (mkBounds sz constr bnds)
    n = length objfun
    m = length constr
    (sz, sg, objfun) = adapt opt

simplex opt (Sparse constr) bnds = extract sg sol where
    sol = simplexSparse m n (mkConstrS sz objfun constr) (mkBounds sz constr bnds)
    n = length objfun
    m = length constr
    (sz, sg, objfun) = adapt opt

simplex opt constr@(General _) bnds = simplex opt (sparseOfGeneral constr) bnds

-- | Simplex method with exact internal arithmetic. See glp_exact in glpk documentation for more information.
exact :: Optimization -> Constraints -> Bounds -> Solution

exact opt (Dense   []) bnds = exact opt (Sparse []) bnds
exact opt (Sparse  []) bnds = exact opt (Sparse [Free [0#1]]) bnds
exact opt (General []) bnds = exact opt (Sparse [Free [0#1]]) bnds

exact opt (Dense constr) bnds = extract sg sol where
    sol = exactSparse m n (mkConstrD sz objfun constr) (mkBounds sz constr bnds)
    n = length objfun
    m = length constr
    (sz, sg, objfun) = adapt opt

exact opt (Sparse constr) bnds = extract sg sol where
    sol = exactSparse m n (mkConstrS sz objfun constr) (mkBounds sz constr bnds)
    n = length objfun
    m = length constr
    (sz, sg, objfun) = adapt opt

exact opt constr@(General _) bnds = exact opt (sparseOfGeneral constr) bnds

adapt :: Optimization -> (Int, Double, [Double])
adapt opt = case opt of
    Maximize x -> (sz x, 1 ,x)
    Minimize x -> (sz x, -1, (map negate x))
  where
    sz x | null x = error "simplex: objective function with zero variables"
         | otherwise = length x

extract :: Double -> Vector Double -> Solution
extract sg sol = r where
    z = sg * (sol!1)
    v = toList $ subVector 2 (size sol -2) sol
    r = case round(sol!0)::Int of
          1 -> Undefined
          2 -> Feasible (z,v)
          3 -> Infeasible (z,v)
          4 -> NoFeasible
          5 -> Optimal (z,v)
          6 -> Unbounded
          _ -> error "simplex: solution type unknown"

-----------------------------------------------------

obj :: Bound t -> t
obj (x :<=: _)  = x
obj (x :>=: _)  = x
obj (x :&: _)  = x
obj (x :==: _) = x
obj (Free x)   = x

withObj :: Bound t -> t -> Bound t
withObj (_ :<=: b) x = (x :<=: b)
withObj (_ :>=: b) x = (x :>=: b)
withObj (_ :&: b) x = (x :&: b)
withObj (_ :==: b) x = (x :==: b)
withObj (Free _) x = Free x

tb :: Bound t -> Double
tb (_ :<=: _)  = glpUP
tb (_ :>=: _)  = glpLO
tb (_ :&: _)  = glpDB
tb (_ :==: _) = glpFX
tb (Free _)   = glpFR

lb :: Bound t -> Double
lb (_ :<=: _)     = 0
lb (_ :>=: a)     = a
lb (_ :&: (a,_)) = a
lb (_ :==: a)    = a
lb (Free _)      = 0

ub :: Bound t -> Double
ub (_ :<=: a)     = a
ub (_ :>=: _)     = 0
ub (_ :&: (_,a)) = a
ub (_ :==: a)    = a
ub (Free _)      = 0

mkBound1 :: Bound t -> [Double]
mkBound1 b = [tb b, lb b, ub b]

mkBound2 :: Bound t -> (t, [Double])
mkBound2 b = (obj b, mkBound1 b)

mkBounds :: Int -> [Bound [a]] -> [Bound Int] -> Matrix Double
mkBounds n b1 b2 = fromLists (cb++vb) where
    gv' = map obj b2
    gv | nub gv' == gv' = gv'
       | otherwise = error $ "simplex: duplicate bounds for vars " ++ show (gv'\\nub gv')
    rv | null gv || minimum gv >= 0 && maximum gv <= n = [1..n] \\ gv
       | otherwise = error $ "simplex: bounds: variables "++show gv++" not in 1.."++show n
    vb = map snd $ sortBy (compare `on` fst) $ map (mkBound2 . (:>=: 0)) rv ++ map mkBound2 b2
    cb = map mkBound1 b1

mkConstrD :: Int -> [Double] -> [Bound [Double]] -> Matrix Double
mkConstrD n f b1 | ok = fromLists (ob ++ co)
                 | otherwise = error $ "simplex: dense constraints require "++show n
                                     ++" variables, given " ++ show ls
    where
       cs = map obj b1
       ls = map length cs
       ok = all (==n) ls
       den = fromLists cs
       ob = map (([0,0]++).return) f
       co = [[fromIntegral i, fromIntegral j,den `atIndex` (i-1,j-1)]| i<-[1 ..rows den], j<-[1 .. cols den]]

mkConstrS :: Int -> [Double] -> [Bound [(Double, Int)]] -> Matrix Double
mkConstrS n objfun b1 = fromLists (ob ++ co) where
    ob = map (([0,0]++).return) objfun
    co = concat $ zipWith f [1::Int ..] cs
    cs = map obj b1
    f k = map (g k)
    g k (c,v) | v >=1 && v<= n = [fromIntegral k, fromIntegral v,c]
              | otherwise = error $ "simplex: sparse constraints: variable "++show v++" not in 1.."++show n

-----------------------------------------------------

foreign import ccall unsafe "c_simplex_sparse" c_simplex_sparse
    :: CInt -> CInt                  -- rows and cols
    -> CInt -> CInt -> Ptr Double    -- coeffs
    -> CInt -> CInt -> Ptr Double    -- bounds
    -> CInt -> Ptr Double            -- result
    -> IO CInt                       -- exit code

simplexSparse :: Int -> Int -> Matrix Double -> Matrix Double -> Vector Double
simplexSparse m n c b = unsafePerformIO $ do
    s <- createVector (2+n)
    app3 (c_simplex_sparse (fi m) (fi n)) mat (cmat c) mat (cmat b) vec s "c_simplex_sparse"
    return s

foreign import ccall unsafe "c_exact_sparse" c_exact_sparse
    :: CInt -> CInt                  -- rows and cols
    -> CInt -> CInt -> Ptr Double    -- coeffs
    -> CInt -> CInt -> Ptr Double    -- bounds
    -> CInt -> Ptr Double            -- result
    -> IO CInt                       -- exit code

exactSparse :: Int -> Int -> Matrix Double -> Matrix Double -> Vector Double
exactSparse m n c b = unsafePerformIO $ do
    s <- createVector (2+n)
    app3 (c_exact_sparse (fi m) (fi n)) mat (cmat c) mat (cmat b) vec s "c_exact_sparse"
    return s

glpFR, glpLO, glpUP, glpDB, glpFX :: Double
glpFR = 0
glpLO = 1
glpUP = 2
glpDB = 3
glpFX = 4

{- Raw format of coeffs

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

