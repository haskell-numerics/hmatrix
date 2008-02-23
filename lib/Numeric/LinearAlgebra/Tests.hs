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
  qCheck, runTests
--, runBigTests
) where

import Numeric.LinearAlgebra
import Numeric.LinearAlgebra.Tests.Instances
import Numeric.LinearAlgebra.Tests.Properties
import Test.QuickCheck
import Numeric.GSL(setErrorHandlerOff)

qCheck n = check defaultConfig {configSize = const n}

-- | It runs all the tests.
runTests :: Int  -- ^ maximum dimension
         -> IO ()
runTests n = do
    setErrorHandlerOff
    let test p = qCheck n p
    test (luProp    . rM)
    test (luProp    . cM)
    test (invProp   . rSqWC)
    test (invProp   . cSqWC)
    test (pinvProp  . rM)
    test (pinvProp  . cM)
    test (detProp   . cSqWC)
    test (svdProp1  . rM)
    test (svdProp1  . cM)
    test (svdProp2  . rM)
    test (svdProp2  . cM)
    test (eigSHProp . rHer)
    test (eigSHProp . cHer)
    test (eigProp   . rSq)
    test (eigProp   . cSq)
    test (nullspaceProp . rM)
    test (nullspaceProp . cM)
    test (qrProp     . rM)
    test (qrProp     . cM)
    test (hessProp   . rSq)
    test (hessProp   . cSq)
    test (schurProp2 . rSq)
    test (schurProp1 . cSq)
    test (cholProp   . rPosDef)
    test (cholProp   . cPosDef)

-- | Some additional tests on big matrices. They take a few minutes.
runBigTests :: IO ()
runBigTests = undefined
