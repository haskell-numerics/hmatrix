module Main where

import Numeric.LinearAlgebra
import Numeric.LinearAlgebra.Tests
import System.Random(randomRs,mkStdGen)
import Test.HUnit hiding (test)
import System(getArgs)


pseudorandomR seed (n,m) = reshape m $ fromList $ take (n*m) $ randomRs (-100,100) $ mkStdGen seed

pseudorandomC seed (n,m) = toComplex (pseudorandomR seed (n,m), pseudorandomR (seed+1) (n,m))

bigmat = m + trans m :: RM
    where m = pseudorandomR 18 (1000,1000)
bigmatc = mc + ctrans mc ::CM
    where mc = pseudorandomC 19 (1000,1000)

utest str b = TestCase $ assertBool str b

feye n = flipud (ident n) :: Matrix Double

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

main = do
    args <- getArgs
    if "--big" `elem` args
        then bigtests
        else runTests 20
