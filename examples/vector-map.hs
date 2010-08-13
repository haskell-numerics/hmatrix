-- use of vectorMapM
--

-------------------------------------------

import Data.Packed.Vector
import Numeric.LinearAlgebra.Interface

import Control.Monad.State

-------------------------------------------

-- an instance of MonadIO, a monad transformer
type VectorMonadT = StateT Int IO

v :: Vector Int
v = fromList $ take 10 [0..]

test1 :: Vector Int -> IO (Vector Int)
test1 = do
        mapVectorM (\x -> do
                          putStr $ (show x) ++ " "
                          return (x + 1))
                            
-- we can have an arbitrary monad AND do IO
addInitialM :: Vector Int -> VectorMonadT ()
addInitialM = mapVectorM_ (\x -> do
                                 i <- get
                                 liftIO $ putStr $ (show $ x + i) ++ " "
                                 put $ x + i
                          )

-------------------------------------------
main = do
       v' <- test1 v
       putStrLn ""
       putStrLn $ show v'
       evalStateT (addInitialM v) 0
       putStrLn ""
       return ()

