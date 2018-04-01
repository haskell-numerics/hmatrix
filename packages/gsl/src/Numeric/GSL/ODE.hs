{-# LANGUAGE FlexibleContexts #-}

{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# OPTIONS_GHC -fno-warn-unused-top-binds #-}

{- |
Module      :  Numeric.GSL.ODE
Copyright   :  (c) Alberto Ruiz 2010
License     :  GPL
Maintainer  :  Alberto Ruiz
Stability   :  provisional

Solution of ordinary differential equation (ODE) initial value problems.

<http://www.gnu.org/software/gsl/manual/html_node/Ordinary-Differential-Equations.html>

A simple example:

@
import Numeric.GSL.ODE
import Numeric.LinearAlgebra
import Graphics.Plot(mplot)

xdot t [x,v] = [v, -0.95*x - 0.1*v]

ts = linspace 100 (0,20 :: Double)

sol = odeSolve xdot [10,0] ts

main = mplot (ts : toColumns sol)
@

-}
-----------------------------------------------------------------------------

module Numeric.GSL.ODE (
    odeSolve, odeSolveV, odeSolveVWith, ODEMethod(..), Jacobian, StepControl(..)
) where

import Numeric.LinearAlgebra.HMatrix
import Numeric.GSL.Internal

import Foreign.Ptr(FunPtr, nullFunPtr, freeHaskellFunPtr)
import Foreign.C.Types
import System.IO.Unsafe(unsafePerformIO)

-------------------------------------------------------------------------

type TVV   = TV (TV Res)
type TVM   = TV (TM Res)
type TVVM  = TV (TV (TM Res))
type TVVVM = TV (TV (TV (TM Res)))

type Jacobian = Double -> Vector Double -> Matrix Double

-- | Stepping functions
data ODEMethod = RK2 -- ^ Embedded Runge-Kutta (2, 3) method.
               | RK4 -- ^ 4th order (classical) Runge-Kutta. The error estimate is obtained by halving the step-size. For more efficient estimate of the error, use the embedded methods.
               | RKf45 -- ^ Embedded Runge-Kutta-Fehlberg (4, 5) method. This method is a good general-purpose integrator.
               | RKck -- ^ Embedded Runge-Kutta Cash-Karp (4, 5) method.
               | RK8pd -- ^ Embedded Runge-Kutta Prince-Dormand (8,9) method.
               | RK2imp Jacobian -- ^ Implicit 2nd order Runge-Kutta at Gaussian points.
               | RK4imp Jacobian -- ^ Implicit 4th order Runge-Kutta at Gaussian points.
               | BSimp Jacobian -- ^ Implicit Bulirsch-Stoer method of Bader and Deuflhard. The method is generally suitable for stiff problems.
               | RK1imp Jacobian -- ^ Implicit Gaussian first order Runge-Kutta. Also known as implicit Euler or backward Euler method. Error estimation is carried out by the step doubling method.
               | MSAdams -- ^ A variable-coefficient linear multistep Adams method in Nordsieck form. This stepper uses explicit Adams-Bashforth (predictor) and implicit Adams-Moulton (corrector) methods in P(EC)^m functional iteration mode. Method order varies dynamically between 1 and 12. 
               | MSBDF Jacobian -- ^ A variable-coefficient linear multistep backward differentiation formula (BDF) method in Nordsieck form. This stepper uses the explicit BDF formula as predictor and implicit BDF formula as corrector. A modified Newton iteration method is used to solve the system of non-linear equations. Method order varies dynamically between 1 and 5. The method is generally suitable for stiff problems.

-- | Adaptive step-size control functions
data StepControl = X     Double Double -- ^ abs. and rel. tolerance for x(t)
                 | X'    Double Double -- ^ abs. and rel. tolerance for x'(t)
                 | XX'   Double Double Double Double -- ^ include both via rel. tolerance scaling factors a_x, a_x'
                 | ScXX' Double Double Double Double (Vector Double) -- ^ scale abs. tolerance of x(t) components

-- | A version of 'odeSolveV' with reasonable default parameters and system of equations defined using lists.
odeSolve
    :: (Double -> [Double] -> [Double])        -- ^ x'(t,x)
    -> [Double]        -- ^ initial conditions
    -> Vector Double   -- ^ desired solution times
    -> Matrix Double   -- ^ solution
odeSolve xdot xi ts = odeSolveV RKf45 hi epsAbs epsRel (l2v xdot) (fromList xi) ts
    where hi = (ts!1 - ts!0)/100
          epsAbs = 1.49012e-08
          epsRel = epsAbs
          l2v f  = \t -> fromList . f t . toList

-- | A version of 'odeSolveVWith' with reasonable default step control.
odeSolveV
    :: ODEMethod
    -> Double            -- ^ initial step size
    -> Double            -- ^ absolute tolerance for the state vector
    -> Double            -- ^ relative tolerance for the state vector
    -> (Double -> Vector Double -> Vector Double)   -- ^ x'(t,x)
    -> Vector Double     -- ^ initial conditions
    -> Vector Double     -- ^ desired solution times
    -> Matrix Double     -- ^ solution
odeSolveV meth hi epsAbs epsRel = odeSolveVWith meth (XX' epsAbs epsRel 1 1) hi

-- | Evolution of the system with adaptive step-size control.
odeSolveVWith
    :: ODEMethod
    -> StepControl
    -> Double            -- ^ initial step size
    -> (Double -> Vector Double -> Vector Double)   -- ^ x'(t,x)
    -> Vector Double     -- ^ initial conditions
    -> Vector Double     -- ^ desired solution times
    -> Matrix Double     -- ^ solution
odeSolveVWith method control = odeSolveVWith' m mbj c epsAbs epsRel aX aX' mbsc
    where (m, mbj) = case method of
              RK2        -> (0 , Nothing )
              RK4        -> (1 , Nothing )
              RKf45      -> (2 , Nothing )
              RKck       -> (3 , Nothing )
              RK8pd      -> (4 , Nothing )
              RK2imp jac -> (5 , Just jac)
              RK4imp jac -> (6 , Just jac)
              BSimp  jac -> (7 , Just jac)
              RK1imp jac -> (8 , Just jac)
              MSAdams    -> (9 , Nothing )
              MSBDF  jac -> (10, Just jac)
          (c, epsAbs, epsRel, aX, aX', mbsc) = case control of
              X     ea er           -> (0, ea, er, 1 , 0  , Nothing)
              X'    ea er           -> (0, ea, er, 0 , 1  , Nothing)
              XX'   ea er ax ax'    -> (0, ea, er, ax, ax', Nothing)
              ScXX' ea er ax ax' sc -> (1, ea, er, ax, ax', Just sc)

odeSolveVWith'
    :: CInt     -- ^ stepping function
    -> Maybe (Double -> Vector Double -> Matrix Double)   -- ^ optional jacobian
    -> CInt     -- ^ step-size control function
    -> Double   -- ^ absolute tolerance for step-size control
    -> Double   -- ^ relative tolerance for step-size control
    -> Double   -- ^ scaling factor for relative tolerance of x(t)
    -> Double   -- ^ scaling factor for relative tolerance of x'(t)
    -> Maybe (Vector Double)    -- ^ optional scaling for absolute error
    -> Double   -- ^ initial step size
    -> (Double -> Vector Double -> Vector Double)        -- ^ x'(t,x)
    -> Vector Double  -- ^ initial conditions
    -> Vector Double  -- ^ desired solution times
    -> Matrix Double  -- ^ solution
odeSolveVWith' method mbjac control epsAbs epsRel aX aX' mbsc h f xiv ts =
    unsafePerformIO $ do
        let n  = size xiv
            sc = case mbsc of
                Just scv -> checkdim1 n scv
                Nothing  -> xiv
        fp <- mkDoubleVecVecfun (\t -> aux_vTov (checkdim1 n . f t))
        jp <- case mbjac of
            Just jac -> mkDoubleVecMatfun (\t -> aux_vTom (checkdim2 n . jac t))
            Nothing  -> return nullFunPtr
        sol <- vec sc $ \sc' -> vec xiv $ \xiv' ->
            vec (checkTimes ts) $ \ts' -> createMIO (size ts) n
                (ode_c method control h epsAbs epsRel aX aX' fp jp
                // sc' // xiv' // ts' )
                "ode"
        freeHaskellFunPtr fp
        if (jp /= nullFunPtr) then freeHaskellFunPtr jp else pure ()
        return sol

foreign import ccall safe "ode"
    ode_c :: CInt -> CInt -> Double
          -> Double -> Double -> Double -> Double
          -> FunPtr (Double -> TVV) -> FunPtr (Double -> TVM) -> TVVVM

-------------------------------------------------------

checkdim1 n v
    | size v == n = v
    | otherwise = error $ "Error: "++ show n
                        ++ " components expected in the result of the function supplied to odeSolve"

checkdim2 n m
    | rows m == n && cols m == n = m
    | otherwise = error $ "Error: "++ show n ++ "x" ++ show n
                        ++ " Jacobian expected in odeSolve"

checkTimes ts | size ts > 1 && all (>0) (zipWith subtract ts' (tail ts')) = ts
              | otherwise = error "odeSolve requires increasing times"
    where ts' = toList ts
