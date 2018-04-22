{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

{-# LANGUAGE ViewPatterns #-}

import           Numeric.Sundials.ARKode.ODE
import           Numeric.LinearAlgebra
import           Graphics.Plot

vanderpol mu = do
    let xdot nu _t [x,v] = [v, -x + nu * v * (1-x*x)]
        xdot _ _ _ = error "vanderpol RHS not defined"
        ts = linspace 1000 (0,50)
        sol = toColumns $ odeSolve (xdot mu) [1,0] ts
    mplot (ts : sol)
    mplot sol


harmonic w d = do
    let xdot u dd _t [x,v] = [v, a*x + b*v] where a = -u*u; b = -2*dd*u
        xdot _ _ _ _ = error "harmonic RHS not defined"
        ts = linspace 100 (0,20)
        sol = odeSolve (xdot w d) [1,0] ts
    mplot (ts : toColumns sol)


kepler v a = mplot (take 2 $ toColumns sol) where
    xdot _t [x,y,vx,vy] = [vx,vy,x*k,y*k]
        where g=1
              k=(-g)*(x*x+y*y)**(-1.5)
    xdot _ _ = error "kepler RHS not defined"
    ts = linspace 100 (0,30)
    sol = odeSolve xdot [4, 0, v * cos (a*degree), v * sin (a*degree)] ts
    degree = pi/180


main = do
    vanderpol 2
    harmonic 1 0
    harmonic 1 0.1
    kepler 0.3 60
    kepler 0.4 70
    vanderpol' 2

-- example of odeSolveV with jacobian
vanderpol' mu = do
    let xdot nu _t (toList->[x,v]) = fromList [v, -x + nu * v * (1-x*x)]
        xdot _ _ _ = error "vanderpol' RHS not defined"
        jac _ (toList->[x,v]) = (2><2) [ 0          ,          1
                                       , -1-2*x*v*mu, mu*(1-x**2) ]
        jac _ _ = error "vanderpol' Jacobian not defined"
        ts = linspace 1000 (0,50)
        hi = pure $ (ts!1 - ts!0) / 100.0
        sol = toColumns $ odeSolveV (SDIRK_5_3_4 jac) hi 1E-8 1E-8 (xdot mu) (fromList [1,0]) ts
    mplot sol


