module Main where

import           Numeric.Sundials.Arkode.ODE
import           Numeric.LinearAlgebra

import           Plots as P
import qualified Diagrams.Prelude as D
import           Diagrams.Backend.Rasterific

import           Control.Lens

brusselator :: a -> [Double] -> [Double]
brusselator _t x = [ a - (w + 1) * u + v * u^(2::Int)
                   , w * u - v * u^(2::Int)
                   , (b - w) / eps - w * u
                   ]
  where
    a = 1.0
    b = 3.5
    eps = 5.0e-6
    u = x !! 0
    v = x !! 1
    w = x !! 2

stiffish :: Double -> [Double] -> [Double]
stiffish t v = [ lamda * u + 1.0 / (1.0 + t * t) - lamda * atan t ]
  where
    lamda = -100.0
    u = v !! 0

lSaxis :: [[Double]] -> P.Axis B D.V2 Double
lSaxis xs = P.r2Axis &~ do
  let ts = xs!!0
      us = xs!!1
      vs = xs!!2
      ws = xs!!3
  P.linePlot' $ zip ts us
  P.linePlot' $ zip ts vs
  P.linePlot' $ zip ts ws

kSaxis :: [(Double, Double)] -> P.Axis B D.V2 Double
kSaxis xs = P.r2Axis &~ do
  P.linePlot' xs

main :: IO ()
main = do
  do let res = odeSolve brusselator [1.2, 3.1, 3.0] (fromList [0.0, 0.1 .. 10.0])
     putStrLn $ show res
     renderRasterific "diagrams/brusselator.png"
                      (D.dims2D 500.0 500.0)
                      (renderAxis $ lSaxis $ [0.0, 0.1 .. 10.0]:(toLists $ tr res))

  do let res = odeSolve stiffish [0.0] (fromList [0.0, 0.1 .. 10.0])
     putStrLn $ show res
     renderRasterific "diagrams/stiffish.png"
                      (D.dims2D 500.0 500.0)
                      (renderAxis $ kSaxis $ zip [0.0, 0.1 .. 10.0] (concat $ toLists res))
