{-# OPTIONS_GHC -Wall #-}

import           Numeric.Sundials.ARKode.ODE
import           Numeric.LinearAlgebra

import           Plots as P
import qualified Diagrams.Prelude as D
import           Diagrams.Backend.Rasterific

import           Control.Lens
import           Data.List (intercalate)

import           Text.PrettyPrint.HughesPJClass

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

stiffJac :: Double -> Vector Double -> Matrix Double
stiffJac _t _v = (1><1) [ lamda ]
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

butcherTableauTex :: ButcherTable -> String
butcherTableauTex (ButcherTable m c b b2) =
  render $
  vcat [ text ("\n\\begin{array}{c|" ++ (concat $ replicate n "c") ++ "}")
       , us
       , text "\\hline"
       , text bs <+> text "\\\\"
       , text b2s <+> text "\\\\"
       , text "\\end{array}"
       ]
  where
    n = rows m
    rs = toLists m
    ss = map (\r -> intercalate " & " $ map show r) rs
    ts = zipWith (\i r -> show i ++ " & " ++ r) (toList c) ss
    us = vcat $ map (\r -> text r <+> text "\\\\") ts
    bs  = " & " ++ (intercalate " & " $ map show $ toList b)
    b2s = " & " ++ (intercalate " & " $ map show $ toList b2)

main :: IO ()
main = do

  let res = butcherTable (SDIRK_2_1_2 undefined)
  putStrLn $ show res
  putStrLn $ butcherTableauTex res

  let resA = butcherTable (KVAERNO_4_2_3 undefined)
  putStrLn $ show resA
  putStrLn $ butcherTableauTex resA

  let resB = butcherTable (SDIRK_5_3_4 undefined)
  putStrLn $ show resB
  putStrLn $ butcherTableauTex resB

  let resC = butcherTable (FEHLBERG_6_4_5 undefined)
  putStrLn $ show resC
  putStrLn $ butcherTableauTex resC

  let res1 = odeSolve brusselator [1.2, 3.1, 3.0] (fromList [0.0, 0.1 .. 10.0])
  renderRasterific "diagrams/brusselator.png"
                   (D.dims2D 500.0 500.0)
                   (renderAxis $ lSaxis $ [0.0, 0.1 .. 10.0]:(toLists $ tr res1))

  let res1a = odeSolve brusselator [1.2, 3.1, 3.0] (fromList [0.0, 0.1 .. 10.0])
  renderRasterific "diagrams/brusselatorA.png"
                   (D.dims2D 500.0 500.0)
                   (renderAxis $ lSaxis $ [0.0, 0.1 .. 10.0]:(toLists $ tr res1a))

  let res2 = odeSolve stiffish [0.0] (fromList [0.0, 0.1 .. 10.0])
  putStrLn $ show res2
  renderRasterific "diagrams/stiffish.png"
                   (D.dims2D 500.0 500.0)
                   (renderAxis $ kSaxis $ zip [0.0, 0.1 .. 10.0] (concat $ toLists res2))

  let res2a = odeSolveV (SDIRK_5_3_4 stiffJac) Nothing 1e-3 1e-6 stiffishV (fromList [0.0]) (fromList [0.0, 0.1 .. 10.0])
  putStrLn "Lower tolerances"
  putStrLn $ show res2a

  let res3 = odeSolve lorenz [-5.0, -5.0, 1.0] (fromList [0.0, 0.01 .. 10.0])
  putStrLn $ show $ last ((toLists $ tr res3)!!0)

  let res3A = odeSolve lorenz [-5.0, -5.0, 1.0] (fromList [0.0, 0.01 .. 10.0])
  putStrLn $ show $ last ((toLists $ tr res3A)!!0)

  renderRasterific "diagrams/lorenz.png"
                   (D.dims2D 500.0 500.0)
                   (renderAxis $ kSaxis $ zip ((toLists $ tr res3)!!0) ((toLists $ tr res3A)!!1))

  let res3a = odeSolve lorenz [-5.0, -5.0, 1.0] (fromList [0.0, 0.01 .. 10.0])
  renderRasterific "diagrams/lorenzA.png"
                   (D.dims2D 500.0 500.0)
                   (renderAxis $ kSaxis $ zip ((toLists $ tr res3)!!0) ((toLists $ tr res3a)!!1))

  renderRasterific "diagrams/lorenz1.png"
                   (D.dims2D 500.0 500.0)
                   (renderAxis $ kSaxis $ zip ((toLists $ tr res3)!!0) ((toLists $ tr res3)!!2))

  renderRasterific "diagrams/lorenz2.png"
                   (D.dims2D 500.0 500.0)
                   (renderAxis $ kSaxis $ zip ((toLists $ tr res3)!!1) ((toLists $ tr res3)!!2))
