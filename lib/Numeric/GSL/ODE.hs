{- |
Module      :  Numeric.GSL.ODE
Copyright   :  (c) Alberto Ruiz 2010
License     :  GPL

Maintainer  :  Alberto Ruiz (aruiz at um dot es)
Stability   :  provisional
Portability :  uses ffi

Solution of ordinary differential equation (ODE) initial value problems.

<http://www.gnu.org/software/gsl/manual/html_node/Ordinary-Differential-Equations.html>

A simple example:

@import Numeric.GSL
import Numeric.LinearAlgebra
import Graphics.Plot

xdot t [x,v] = [v, -0.95*x - 0.1*v]

ts = linspace 100 (0,20)

sol = odeSolve xdot [10,0] ts

main = mplot (ts : toColumns sol)@

-}
-----------------------------------------------------------------------------

module Numeric.GSL.ODE (
    odeSolve, odeSolveV, ODEMethod(..)
) where

import Data.Packed.Internal
import Data.Packed.Matrix
import Foreign
import Foreign.C.Types(CInt)
import Numeric.GSL.Internal

-------------------------------------------------------------------------

-- | Stepping functions
data ODEMethod = RK2 -- ^ Embedded Runge-Kutta (2, 3) method.
               | RK4 -- ^ 4th order (classical) Runge-Kutta. The error estimate is obtained by halving the step-size. For more efficient estimate of the error, use 'RKf45'.
               | RKf45 -- ^ Embedded Runge-Kutta-Fehlberg (4, 5) method. This method is a good general-purpose integrator.
               | RKck -- ^ Embedded Runge-Kutta Cash-Karp (4, 5) method.
               | RK8pd -- ^ Embedded Runge-Kutta Prince-Dormand (8,9) method.
               | RK2imp -- ^ Implicit 2nd order Runge-Kutta at Gaussian points.
               | RK4imp -- ^ Implicit 4th order Runge-Kutta at Gaussian points.
               | BSimp -- ^ Implicit Bulirsch-Stoer method of Bader and Deuflhard. This algorithm requires the Jacobian.
               | Gear1 -- ^ M=1 implicit Gear method.
               | Gear2 -- ^ M=2 implicit Gear method.
               deriving (Enum,Eq,Show,Bounded)

-- | A version of 'odeSolveV' with reasonable default parameters and system of equations defined using lists.
odeSolve
    :: (Double -> [Double] -> [Double])        -- ^ xdot(t,x)
    -> [Double]        -- ^ initial conditions
    -> Vector Double   -- ^ desired solution times
    -> Matrix Double   -- ^ solution
odeSolve xdot xi ts = odeSolveV RKf45 hi epsAbs epsRel (l2v xdot) Nothing (fromList xi) ts
    where hi = (ts@>1 - ts@>0)/100
          epsAbs = 1.49012e-08
          epsRel = 1.49012e-08
          l2v f = \t -> fromList  . f t . toList
          l2m f = \t -> fromLists . f t . toList

-- | Evolution of the system with adaptive step-size control.
odeSolveV
    :: ODEMethod
    -> Double -- ^ initial step size
    -> Double -- ^ absolute tolerance for the state vector
    -> Double -- ^ relative tolerance for the state vector
    -> (Double -> Vector Double -> Vector Double)   -- ^ xdot(t,x)
    -> Maybe (Double -> Vector Double -> Matrix Double)   -- ^ optional jacobian
    -> Vector Double     -- ^ initial conditions
    -> Vector Double     -- ^ desired solution times
    -> Matrix Double     -- ^ solution
odeSolveV method h epsAbs epsRel f mbjac xiv ts = unsafePerformIO $ do
    let n   = dim xiv
    fp <- mkDoubleVecVecfun (\t -> aux_vTov (checkdim1 n . f t))
    jp <- case mbjac of
        Just jac -> mkDoubleVecMatfun (\t -> aux_vTom (checkdim2 n . jac t))
        Nothing  -> return nullFunPtr
    sol <- withVector xiv $ \xiv' ->
            withVector (checkTimes ts) $ \ts' ->
             createMIO (dim ts) n
              (ode_c (fi (fromEnum method)) h epsAbs epsRel fp jp // xiv' // ts' )
              "ode"
    freeHaskellFunPtr fp
    return sol

foreign import ccall "ode"
    ode_c :: CInt -> Double -> Double -> Double -> FunPtr (Double -> TVV) -> FunPtr (Double -> TVM) -> TVVM

-------------------------------------------------------

checkdim1 n v
    | dim v == n = v
    | otherwise = error $ "Error: "++ show n
                        ++ " components expected in the result of the function supplied to odeSolve"

checkdim2 n m
    | rows m == n && cols m == n = m
    | otherwise = error $ "Error: "++ show n ++ "x" ++ show n
                        ++ " Jacobian expected in odeSolve"

checkTimes ts | dim ts > 1 && all (>0) (zipWith subtract ts' (tail ts')) = ts
              | otherwise = error "odeSolve requires increasing times"
    where ts' = toList ts