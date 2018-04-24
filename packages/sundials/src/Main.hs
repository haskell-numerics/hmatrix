{-# OPTIONS_GHC -Wall #-}

import qualified Numeric.Sundials.ARKode.ODE as ARK
import qualified Numeric.Sundials.CVode.ODE  as CV
import           Numeric.LinearAlgebra

import           Plots as P
import qualified Diagrams.Prelude as D
import           Diagrams.Backend.Rasterific

import           Control.Lens

import           Test.Hspec


lorenz :: Double -> [Double] -> [Double]
lorenz _t u = [ sigma * (y - x)
              , x * (rho - z) - y
              , x * y - beta * z
              ]
  where
    rho = 28.0
    sigma = 10.0
    beta = 8.0 / 3.0
    x = u !! 0
    y = u !! 1
    z = u !! 2

_lorenzJac :: Double -> Vector Double -> Matrix Double
_lorenzJac _t u = (3><3) [ (-sigma), rho - z, y
                        , sigma   , -1.0   , x
                        , 0.0     , (-x)   , (-beta)
                        ]
  where
    rho = 28.0
    sigma = 10.0
    beta = 8.0 / 3.0
    x = u ! 0
    y = u ! 1
    z = u ! 2

brusselator :: Double -> [Double] -> [Double]
brusselator _t x = [ a - (w + 1) * u + v * u * u
                   , w * u - v * u * u
                   , (b - w) / eps - w * u
                   ]
  where
    a = 1.0
    b = 3.5
    eps = 5.0e-6
    u = x !! 0
    v = x !! 1
    w = x !! 2

_brussJac :: Double -> Vector Double -> Matrix Double
_brussJac _t x = (3><3) [ (-(w + 1.0)) + 2.0 * u * v, w - 2.0 * u * v, (-w)
                       , u * u                     , (-(u * u))     , 0.0
                       , (-u)                      , u              , (-1.0) / eps - u
                       ]
  where
    y = toList x
    u = y !! 0
    v = y !! 1
    w = y !! 2
    eps = 5.0e-6

stiffish :: Double -> [Double] -> [Double]
stiffish t v = [ lamda * u + 1.0 / (1.0 + t * t) - lamda * atan t ]
  where
    lamda = -100.0
    u = v !! 0

stiffishV :: Double -> Vector Double -> Vector Double
stiffishV t v = fromList [ lamda * u + 1.0 / (1.0 + t * t) - lamda * atan t ]
  where
    lamda = -100.0
    u = v ! 0

_stiffJac :: Double -> Vector Double -> Matrix Double
_stiffJac _t _v = (1><1) [ lamda ]
  where
    lamda = -100.0

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

  let res1 = ARK.odeSolve brusselator [1.2, 3.1, 3.0] (fromList [0.0, 0.1 .. 10.0])
  renderRasterific "diagrams/brusselator.png"
                   (D.dims2D 500.0 500.0)
                   (renderAxis $ lSaxis $ [0.0, 0.1 .. 10.0]:(toLists $ tr res1))

  let res1a = ARK.odeSolve brusselator [1.2, 3.1, 3.0] (fromList [0.0, 0.1 .. 10.0])
  renderRasterific "diagrams/brusselatorA.png"
                   (D.dims2D 500.0 500.0)
                   (renderAxis $ lSaxis $ [0.0, 0.1 .. 10.0]:(toLists $ tr res1a))

  let res2 = ARK.odeSolve stiffish [0.0] (fromList [0.0, 0.1 .. 10.0])
  renderRasterific "diagrams/stiffish.png"
                   (D.dims2D 500.0 500.0)
                   (renderAxis $ kSaxis $ zip [0.0, 0.1 .. 10.0] (concat $ toLists res2))

  let res2a = ARK.odeSolveV (ARK.SDIRK_5_3_4') Nothing 1e-6 1e-10 stiffishV (fromList [0.0]) (fromList [0.0, 0.1 .. 10.0])

  let res2b = ARK.odeSolveV (ARK.TRBDF2_3_3_2') Nothing 1e-6 1e-10 stiffishV (fromList [0.0]) (fromList [0.0, 0.1 .. 10.0])

  let maxDiffA = maximum $ map abs $
                 zipWith (-) ((toLists $ tr res2a)!!0) ((toLists $ tr res2b)!!0)

  let res2c = CV.odeSolveV (CV.BDF) Nothing 1e-6 1e-10 stiffishV (fromList [0.0]) (fromList [0.0, 0.1 .. 10.0])

  let maxDiffB = maximum $ map abs $
                 zipWith (-) ((toLists $ tr res2a)!!0) ((toLists $ tr res2c)!!0)

  let maxDiffC = maximum $ map abs $
                 zipWith (-) ((toLists $ tr res2b)!!0) ((toLists $ tr res2c)!!0)

  hspec $ describe "Compare results" $ do
    it "for SDIRK_5_3_4' and TRBDF2_3_3_2'" $ maxDiffA < 1.0e-6
    it "for SDIRK_5_3_4' and BDF" $ maxDiffB < 1.0e-6
    it "for TRBDF2_3_3_2' and BDF" $ maxDiffC < 1.0e-6

  let res3 = ARK.odeSolve lorenz [-5.0, -5.0, 1.0] (fromList [0.0, 0.01 .. 10.0])

  renderRasterific "diagrams/lorenz.png"
                   (D.dims2D 500.0 500.0)
                   (renderAxis $ kSaxis $ zip ((toLists $ tr res3)!!0) ((toLists $ tr res3)!!1))

  renderRasterific "diagrams/lorenz1.png"
                   (D.dims2D 500.0 500.0)
                   (renderAxis $ kSaxis $ zip ((toLists $ tr res3)!!0) ((toLists $ tr res3)!!2))

  renderRasterific "diagrams/lorenz2.png"
                   (D.dims2D 500.0 500.0)
                   (renderAxis $ kSaxis $ zip ((toLists $ tr res3)!!1) ((toLists $ tr res3)!!2))
