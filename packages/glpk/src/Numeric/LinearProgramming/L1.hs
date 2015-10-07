{- |
Module      :  Numeric.LinearProgramming.L1
Copyright   :  (c) Alberto Ruiz 2011-14
Stability   :  provisional

Linear system solvers in the L_1 norm using linear programming.

-}
-----------------------------------------------------------------------------

module Numeric.LinearProgramming.L1 (
    l1Solve, l1SolveGT,
    l1SolveO, lInfSolveO,
    l1SolveU,
) where

import Numeric.LinearAlgebra.HMatrix
import Numeric.LinearProgramming

-- | L_inf solution of overconstrained system Ax=b.
--
-- @argmin_x ||Ax-b||_inf@
lInfSolveO :: Matrix Double -> Vector Double -> Vector Double
lInfSolveO a b = fromList (take n x)
  where
    n = cols a
    as = toRows a
    bs = toList b
    c1 = zipWith (mk (1)) as bs
    c2 = zipWith (mk (-1)) as bs
    mk sign a_i b_i = (zipWith (#) (toList (scale sign a_i)) [1..] ++ [-1#(n+1)]) :<=: (sign * b_i)
    p = Sparse (c1++c2)
    Optimal (_j,x) = simplex (Minimize (replicate n 0 ++ [1])) p (map Free [1..(n+1)])

--------------------------------------------------------------------------------

-- | L_1 solution of overconstrained system Ax=b.
--
-- @argmin_x ||Ax-b||_1@
l1SolveO :: Matrix Double -> Vector Double -> Vector Double
l1SolveO a b = fromList (take n x)
  where
    n = cols a
    m = rows a
    as = toRows a
    bs = toList b
    ks = [1..]
    c1 = zipWith3 (mk (1)) as bs ks
    c2 = zipWith3 (mk (-1)) as bs ks
    mk sign a_i b_i k = (zipWith (#) (toList (scale sign a_i)) [1..] ++ [-1#(k+n)]) :<=: (sign * b_i)
    p = Sparse (c1++c2)
    Optimal (_j,x) = simplex (Minimize (replicate n 0 ++ replicate m 1)) p (map Free [1..(n+m)])

--------------------------------------------------------------------------------

-- | L1 solution of underconstrained linear system Ax=b.
--
-- @argmin_x ||x||_1 such that Ax=b@
l1SolveU :: Matrix Double -> Vector Double -> Vector Double
l1SolveU a y = fromList (take n x)
  where
    n = cols a
    c1 = map (\k ->  [ 1#k, -1#k+n] :<=: 0) [1..n]
    c2 = map (\k ->  [-1#k, -1#k+n] :<=: 0) [1..n]
    c3 = zipWith (:==:) (map sp $ toRows a) (toList y)
    sp v = zipWith (#) (toList v) [1..]
    p = Sparse (c1 ++ c2 ++ c3)
    Optimal (_j,x) = simplex (Minimize (replicate n 0 ++ replicate n 1)) p (map Free [1..(2*n)])

--------------------------------------------------------------------------------
-- | Solution in the L_1 norm, with L_1 regularization, of a linear system @Ax=b@.
--
-- @argmin_x  λ||x||_1 + ||Ax-b||_1@
l1Solve
    :: Double        -- ^ λ
    -> Matrix Double -- ^ A
    -> Vector Double -- ^ b
    -> Vector Double -- ^ x
l1Solve λ a b = fromList (take n x)
  where
    n = cols a
    m = rows a
    as = toRows a
    bs = toList b
    c1Res = zipWith3 (mkR (1)) as bs [1..m]
    c2Res = zipWith3 (mkR (-1)) as bs [1..m]
    mkR sign a_i b_i k = (zipWith (#) (toList (scale sign a_i)) [1..] ++ [-1#(k+2*n)]) :<=: (sign * b_i)
    c1Sol = map (\k ->  [ 1#k, -1#k+n] :<=: 0) [1..n]
    c2Sol = map (\k ->  [-1#k, -1#k+n] :<=: 0) [1..n]
    p = Sparse (c1Res++c2Res++c1Sol++c2Sol)
    cost = replicate n 0 ++ replicate n λ ++ replicate m 1
    Optimal (_j,x) = simplex (Minimize cost) p (map Free [1..(2*n+m)])

--------------------------------------------------------------------------------

-- | Solution in the L_1 norm, with L_1 regularization, of a system of linear inequalities @Ax>=b@.
--
-- @argmin_x  λ||x||_1 + ||step(b-Ax)||_1@
l1SolveGT
    :: Double        -- ^ λ
    -> Matrix Double -- ^ A
    -> Vector Double -- ^ b
    -> Vector Double -- ^ x
l1SolveGT λ a b = fromList (take n x)
  where
    n = cols a
    m = rows a
    as = toRows a
    bs = toList b
    cRes = zipWith3 mkR as bs [1..m]
    mkR a_i b_i k = (zipWith (#) (toList a_i) [1..] ++ [1#(k+2*n)]) :>=: (b_i)
    c1Sol = map (\k ->  [ 1#k, -1#k+n] :<=: 0) [1..n]
    c2Sol = map (\k ->  [-1#k, -1#k+n] :<=: 0) [1..n]
    p = Sparse (cRes++c1Sol++c2Sol)
    cost = replicate n 0 ++ replicate n λ ++ replicate m 1
    Optimal (_j,x) = simplex (Minimize cost) p (map Free [1..(2*n)])

--------------------------------------------------------------------------------


