-- |
-- Module      :  Internal.Tools
-- Copyright   :  (c) Alberto Ruiz 2007-15
-- License     :  BSD3
-- Maintainer  :  Alberto Ruiz
-- Stability   :  provisional
--

module Internal.Tools where

import Data.List(transpose,intersperse)
import Foreign.C.Types(CInt)
import Data.List.Split

type I = CInt

splitEvery :: Int -> [e] -> [[e]]
splitEvery = chunksOf

-- | postfix function application (@flip ($)@)
(//) :: x -> (x -> y) -> y
infixl 0 //
(//) = flip ($)

-- | specialized fromIntegral
fi :: Int -> CInt
fi = fromIntegral

-- | specialized fromIntegral
ti :: CInt -> Int
ti = fromIntegral

-- | obtains the common value of a property of a list
common :: (Eq a) => (b->a) -> [b] -> Maybe a
common f = commonval . map f
  where
    commonval :: (Eq a) => [a] -> Maybe a
    commonval [] = Nothing
    commonval [a] = Just a
    commonval (a:b:xs) = if a==b then commonval (b:xs) else Nothing

-- | common value with \"adaptable\" 1
compatdim :: [Int] -> Maybe Int
compatdim [] = Nothing
compatdim [a] = Just a
compatdim (a:b:xs)
    | a==b = compatdim (b:xs)
    | a==1 = compatdim (b:xs)
    | b==1 = compatdim (a:xs)
    | otherwise = Nothing

-- | Formatting tool
table :: String -> [[String]] -> String
table sep as = unlines . map unwords' $ transpose mtp
  where 
    mt = transpose as
    longs = map (maximum . map length) mt
    mtp = zipWith (\a b -> map (pad a) b) longs mt
    pad n str = replicate (n - length str) ' ' ++ str
    unwords' = concat . intersperse sep

