#!/usr/bin/env runhaskell

import Data.List(isPrefixOf)
import System(getArgs)

rep (c,r) [] = []
rep (c,r) f@(x:xs) 
  | c `isPrefixOf` f = r ++ rep (c,r) (drop (length c) f)
  | otherwise        = x:(rep (c,r) xs)

main = do
    args <- getArgs
    let [p',r'] = map (rep ("\\n","\n")) args
    interact $ rep (p',r')
