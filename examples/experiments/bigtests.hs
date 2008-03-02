module Main where

import Numeric.LinearAlgebra
import Numeric.LinearAlgebra.Tests
import System.Random(randomRs,mkStdGen)
import Test.HUnit hiding (test)
import System(getArgs)


pseudorandomR seed (n,m) = reshape m $ fromList $ take (n*m) $ randomRs (-100,100) $ mkStdGen seed

pseudorandomC seed (n,m) = toComplex (pseudorandomR seed (n,m), pseudorandomR (seed+1) (n,m))

bigmat = m + trans m
    where m = pseudorandomR 18 (1000,1000) :: Matrix Double
bigmatc = mc + ctrans mc
    where mc = pseudorandomC 19 (1000,1000) :: Matrix (Complex Double)

utest str b = TestCase $ assertBool str b

feye n = flipud (ident n) :: Matrix Double

infixl 4 |~|
a |~| b = dist a b < 10^^(-10)

dist a b = r
    where norm = pnorm Infinity
          na = norm a
          nb = norm b
          nab = norm (a-b)
          mx = max na nb
          mn = min na nb
          r = if mn < eps
                then mx
                else nab/mx

square m = rows m == cols m

unitary m = square m && m <> ctrans m |~| ident (rows m)

eigProp m = complex m <> v |~| v <> diag s
    where (s, v) = eig m

eigSHProp m = m <> v |~| v <> real (diag s)
              && unitary v
              && m |~| v <> real (diag s) <> ctrans v
    where (s, v) = eigSH m

bigtests = do
    putStrLn "--------- big matrices -----"
    runTestTT $ TestList
     [ utest "eigS" $ eigSHProp bigmat
     , utest "eigH" $ eigSHProp bigmatc
     , utest "eigR" $ eigProp   bigmat
     , utest "eigC" $ eigProp   bigmatc
     , utest "det"  $ det (feye 1000) == 1 && det (feye 1002) == -1
     ]
    return ()

main = bigtests
