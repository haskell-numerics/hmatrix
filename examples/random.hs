import System.Random.MWC
import qualified System.Random.MWC.Distributions as D
import Numeric.LinearAlgebra
import Numeric.LinearAlgebra.Devel
  
rvec :: Vector Double
rvec = runSTVector $ do
    v <- newUndefinedVector 10
    g <- initialize (fromList [4, 8, 15, 16, 23, 42])
    mapM_ (\k -> writeVector v k =<< D.standard g) [0..9]
    return v


main = do
    v <- withSystemRandom . asGenST $ \gen -> uniformVector gen 20
    print (v :: Vector Double)

    g <- initialize (fromList [4, 8, 15, 16, 23, 42])
    x <- uniform g :: IO Double
    print x
    print =<< (uniform g :: IO Double)
    print =<< (uniformVector g 20 :: IO (Vector Double))

