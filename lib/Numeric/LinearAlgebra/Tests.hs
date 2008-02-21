-----------------------------------------------------------------------------
{- |
Module      :  Numeric.LinearAlgebra.Tests
Copyright   :  (c) Alberto Ruiz 2007
License     :  GPL-style

Maintainer  :  Alberto Ruiz (aruiz at um dot es)
Stability   :  provisional
Portability :  portable

Some tests.

-}

module Numeric.LinearAlgebra.Tests(
  module Numeric.LinearAlgebra.Tests.Instances,
  module Numeric.LinearAlgebra.Tests.Properties,
  qCheck,RM,CM, rM,cM, rHer,cHer,rRot,cRot,rSq,cSq,
    runTests
--, runBigTests
) where

import Numeric.LinearAlgebra
import Numeric.LinearAlgebra.Tests.Instances
import Numeric.LinearAlgebra.Tests.Properties
import Test.QuickCheck
import Debug.Trace

qCheck n = check defaultConfig {configSize = const n}

debug x = trace (show x) x

type RM = Matrix Double
type CM = Matrix (Complex Double)

rM m = m :: RM
cM m = m :: CM

rHer (Her m) = m :: RM
cHer (Her m) = m :: CM

rRot (Rot m) = m :: RM
cRot (Rot m) = m :: CM

rSq  (Sq m)  = m :: RM
cSq  (Sq m)  = m :: CM

rWC (WC m) = m :: RM
cWC (WC m) = m :: CM

-- | It runs all the tests.
runTests :: Int  -- ^ maximum dimension
         -> IO ()
runTests n = do
    qCheck n (hermitian . rHer)
    qCheck n (hermitian . cHer)
    qCheck n (unitary . rRot)
    qCheck n (unitary . cRot)
    qCheck n (wellCond . rWC)
    qCheck n (wellCond . cWC)
    --------------------------------
    qCheck n (luTest . rM)
    qCheck n (luTest . cM)

-- | Some additional tests on big matrices. They take a few minutes.
runBigTests :: IO ()
runBigTests = undefined
