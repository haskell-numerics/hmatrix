{-# OPTIONS_GHC -Wall #-}

import           Numeric.Sundials.ARKode.ODE
import           Numeric.LinearAlgebra

import           Data.List (intercalate)

import           Text.PrettyPrint.HughesPJClass


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
