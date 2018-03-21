{-# OPTIONS_GHC -Wall #-}

import qualified Data.Vector.Storable as V
import           Numeric.Sundials.Arkode.ODE

brusselator :: Double -> V.Vector Double -> V.Vector Double
brusselator _t x = V.fromList [ a - (w + 1) * u + v * u^2
                              , w * u - v * u^2
                              , (b - w) / eps - w * u
                              ]
  where
    a = 1.0
    b = 3.5
    eps = 5.0e-6
    u = x V.! 0
    v = x V.! 1
    w = x V.! 2

main :: IO ()
main = do
  let res = solveOde brusselator (V.fromList [1.2, 3.1, 3.0]) (V.fromList [0.0, 1.0 .. 10.0])
  putStrLn $ show res
