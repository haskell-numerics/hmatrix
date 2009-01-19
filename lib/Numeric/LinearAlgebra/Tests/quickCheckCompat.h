#ifndef MIN_VERSION_QuickCheck
#define MIN_VERSION_QuickCheck(A,B,C) 1
#endif

#if MIN_VERSION_QuickCheck(2,0,0)
import Test.QuickCheck(Arbitrary,arbitrary,coarbitrary,choose,vector
                      ,sized,classify,Testable,Property

                      ,quickCheckWith,maxSize,stdArgs,shrink)

#else
import Test.QuickCheck(Arbitrary,arbitrary,coarbitrary,choose,vector
                      ,sized,classify,Testable,Property

                      ,check,configSize,defaultConfig,trivial)
#endif



#if MIN_VERSION_QuickCheck(2,0,0)
trivial :: Testable a => Bool -> a -> Property
trivial = (`classify` "trivial")
#else
#endif


-- define qCheck, which used to be in Tests.hs
#if MIN_VERSION_QuickCheck(2,0,0)
qCheck n = quickCheckWith stdArgs {maxSize = n}
#else
qCheck n = check defaultConfig {configSize = const n}
#endif

