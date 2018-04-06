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

lorenzJac :: Double -> Vector Double -> Matrix Double
lorenzJac _t u = (3><3) [ (-sigma), rho - z, y
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

brussJac :: Double -> Vector Double -> Matrix Double
brussJac _t x = (3><3) [ (-(w + 1.0)) + 2.0 * u * v, w - 2.0 * u * v, (-w)
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

butcherTableauTex :: (Show a, Element a) => Matrix a -> String
butcherTableauTex m = render $
                    vcat [ text ("\n\\begin{array}{c|" ++ (concat $ replicate n "c") ++ "}")
                         , us
                         , text "\\end{array}"
                         ]
  where
    n = rows m
    rs = toLists m
    ss = map (\r -> intercalate " & " $ map show r) rs
    ts = zipWith (\i r -> "c_" ++ show i ++ " & " ++ r) [1..n] ss
    us = vcat $ map (\r -> text r <+> text "\\\\") ts

main :: IO ()
main = do
  -- $$
  -- \begin{array}{c|cccc}
  -- c_1    & a_{11} & a_{12}& \dots & a_{1s}\\
  -- c_2    & a_{21} & a_{22}& \dots & a_{2s}\\
  -- \vdots & \vdots & \vdots& \ddots& \vdots\\
  -- c_s    & a_{s1} & a_{s2}& \dots & a_{ss} \\
  -- \hline
  --        & b_1    & b_2   & \dots & b_s\\
  --        & b^*_1  & b^*_2 & \dots & b^*_s\\
  -- \end{array}
  -- $$

  -- let res = btGet (SDIRK_2_1_2 undefined)
  -- putStrLn $ show res
  -- putStrLn $ butcherTableauTex res

  -- let res = btGet (KVAERNO_4_2_3 undefined)
  -- putStrLn $ show res
  -- putStrLn $ butcherTableauTex res

  -- let res = btGet (SDIRK_5_3_4 undefined)
  -- putStrLn $ show res
  -- putStrLn $ butcherTableauTex res

  let res1 = odeSolve' (SDIRK_5_3_4 brussJac) brusselator [1.2, 3.1, 3.0] (fromList [0.0, 0.1 .. 10.0])
  renderRasterific "diagrams/brusselator.png"
                   (D.dims2D 500.0 500.0)
                   (renderAxis $ lSaxis $ [0.0, 0.1 .. 10.0]:(toLists $ tr res1))

  let res1a = odeSolve' (SDIRK_5_3_4') brusselator [1.2, 3.1, 3.0] (fromList [0.0, 0.1 .. 10.0])
  renderRasterific "diagrams/brusselatorA.png"
                   (D.dims2D 500.0 500.0)
                   (renderAxis $ lSaxis $ [0.0, 0.1 .. 10.0]:(toLists $ tr res1a))

  let res2 = odeSolve' (SDIRK_5_3_4 stiffJac) stiffish [0.0] (fromList [0.0, 0.1 .. 10.0])
  putStrLn $ show res2
  renderRasterific "diagrams/stiffish.png"
                   (D.dims2D 500.0 500.0)
                   (renderAxis $ kSaxis $ zip [0.0, 0.1 .. 10.0] (concat $ toLists res2))

  let res3 = odeSolve' (SDIRK_5_3_4 lorenzJac) lorenz [-5.0, -5.0, 1.0] (fromList [0.0, 0.01 .. 10.0])
  putStrLn $ show $ last ((toLists $ tr res3)!!0)

  let res3 = odeSolve' (SDIRK_5_3_4') lorenz [-5.0, -5.0, 1.0] (fromList [0.0, 0.01 .. 10.0])
  putStrLn $ show $ last ((toLists $ tr res3)!!0)

  renderRasterific "diagrams/lorenz.png"
                   (D.dims2D 500.0 500.0)
                   (renderAxis $ kSaxis $ zip ((toLists $ tr res3)!!0) ((toLists $ tr res3)!!1))

  let res3a = odeSolve' (SDIRK_5_3_4') lorenz [-5.0, -5.0, 1.0] (fromList [0.0, 0.01 .. 10.0])
  renderRasterific "diagrams/lorenzA.png"
                   (D.dims2D 500.0 500.0)
                   (renderAxis $ kSaxis $ zip ((toLists $ tr res3)!!0) ((toLists $ tr res3a)!!1))

  renderRasterific "diagrams/lorenz1.png"
                   (D.dims2D 500.0 500.0)
                   (renderAxis $ kSaxis $ zip ((toLists $ tr res3)!!0) ((toLists $ tr res3)!!2))

  renderRasterific "diagrams/lorenz2.png"
                   (D.dims2D 500.0 500.0)
                   (renderAxis $ kSaxis $ zip ((toLists $ tr res3)!!1) ((toLists $ tr res3)!!2))
