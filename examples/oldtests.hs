import Test.HUnit
import LinearAlgebra
import GSL hiding (exp)
import System.Random(randomRs,mkStdGen)

realMatrix = fromLists :: [[Double]] -> Matrix Double
realVector = fromList ::  [Double] -> Vector Double



infixl 2 =~=
a =~= b = pnorm PNorm1 (flatten (a - b)) < 1E-6

randomMatrix seed (n,m) = reshape m $ realVector $ take (n*m) $ randomRs (-100,100) $ mkStdGen seed 

randomMatrixC seed (n,m) = toComplex (randomMatrix seed (n,m), randomMatrix (seed+1) (n,m))

besselTest = do
    let (r,e) = bessel_J0_e 5.0
    let expected = -0.17759677131433830434739701
    assertBool "bessel_J0_e" ( abs (r-expected) < e ) 

exponentialTest = do
    let (v,e,err) = exp_e10_e 30.0
    let expected = exp 30.0
    assertBool "exp_e10_e" ( abs (v*10^e - expected) < 4E-2 ) 

disp m = putStrLn (format " " show m)

ms = realMatrix [[1,2,3]
                ,[-4,1,7]]

ms' = randomMatrix 27 (50,100)

ms'' = toComplex (randomMatrix 100 (50,100),randomMatrix 101 (50,100))

fullsvdTest method mat msg = do
    let (u,s,vt) = method mat
    assertBool msg (u <> s <> trans vt =~= mat)

svdg' m = (u, diag s, v) where (u,s,v) = svdg m

full_svd_Rd = svdRdd

--------------------------------------------------------------------

mcu = toComplex (randomMatrix 33 (20,20),randomMatrix 34 (20,20))

mcur = randomMatrix 35 (40,40)

-- eigenvectors are columns
eigTest method m msg = do
    let (s,v) = method m
    assertBool msg $ m <> v =~= v <> diag s

bigmat = m + trans m where m = randomMatrix 18 (1000,1000)
bigmatc = mc + conjTrans mc where mc = toComplex(m,m)
                                  m = randomMatrix 19 (1000,1000)

--------------------------------------------------------------------

invTest msg m = do
    assertBool msg $ m <> inv m =~= ident (rows m)

invComplexTest msg m = do
    assertBool msg $ m <> invC m =~= identC (rows m)

invC m = linearSolveC m (identC (rows m))

identC = comp . ident

--------------------------------------------------------------------

pinvTest f msg m = do
    assertBool msg $ m <> f m <> m =~= m

pinvC m = linearSolveLSC m (identC (rows m))

pinvSVDR m = linearSolveSVDR Nothing m (ident (rows m))

pinvSVDC m = linearSolveSVDC Nothing m (identC (rows m))

--------------------------------------------------------------------


tests = TestList [
      TestCase $ besselTest
    , TestCase $ exponentialTest
    , TestCase $ invTest "inv 100x100" (randomMatrix 18 (100,100))
    , TestCase $ invComplexTest "complex inv 100x100" (randomMatrixC 18 (100,100))
    , TestCase $ pinvTest (pinvTolg 1) "pinvg 100x50" (randomMatrix 18 (100,50))
    , TestCase $ pinvTest pinv "pinv 100x50" (randomMatrix 18 (100,50))
    , TestCase $ pinvTest pinv "pinv 50x100" (randomMatrix 18 (50,100))
    , TestCase $ pinvTest pinvSVDR "pinvSVDR 100x50" (randomMatrix 18 (100,50))
    , TestCase $ pinvTest pinvSVDR "pinvSVDR 50x100" (randomMatrix 18 (50,100))
    , TestCase $ pinvTest pinvC "pinvC 100x50" (randomMatrixC 18 (100,50))
    , TestCase $ pinvTest pinvC "pinvC 50x100" (randomMatrixC 18 (50,100))
    , TestCase $ pinvTest pinvSVDC "pinvSVDC 100x50" (randomMatrixC 18 (100,50))
    , TestCase $ pinvTest pinvSVDC "pinvSVDC 50x100" (randomMatrixC 18 (50,100))
    , TestCase $ eigTest eigC mcu "eigC" 
    , TestCase $ eigTest eigR mcur "eigR"
    , TestCase $ eigTest eigS (mcur+trans mcur) "eigS"
    , TestCase $ eigTest eigSg (mcur+trans mcur) "eigSg"
    , TestCase $ eigTest eigH (mcu+ (conjTrans) mcu) "eigH"
    , TestCase $ eigTest eigHg (mcu+ (conjTrans) mcu) "eigHg"
    , TestCase $ fullsvdTest svdg' ms "GSL svd small"
    , TestCase $ fullsvdTest svdR ms "fullsvdR small"
    , TestCase $ fullsvdTest svdR (trans ms) "fullsvdR small"
    , TestCase $ fullsvdTest svdR ms' "fullsvdR"
    , TestCase $ fullsvdTest svdR (trans ms') "fullsvdR"
    , TestCase $ fullsvdTest full_svd_Rd ms' "fullsvdRd"
    , TestCase $ fullsvdTest full_svd_Rd (trans ms') "fullsvdRd"
    , TestCase $ fullsvdTest svdC ms'' "fullsvdC"
    , TestCase $ fullsvdTest svdC (trans ms'') "fullsvdC"
    , TestCase $ eigTest eigS bigmat "big eigS"
    , TestCase $ eigTest eigH bigmatc "big eigH"
    , TestCase $ eigTest eigR bigmat "big eigR"
    ]

main = runTestTT tests
