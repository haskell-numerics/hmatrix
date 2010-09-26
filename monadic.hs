-- monadic computations
-- (contributed by Vivian McPhail)

import Numeric.Container
import Control.Monad.State
import Control.Monad.Maybe

-------------------------------------------

-- an instance of MonadIO, a monad transformer
type VectorMonadT = StateT Int IO

v :: Vector Int
v = 10 |> [0..]

test1 :: Vector Int -> IO (Vector Int)
test1 = mapVectorM $ \x -> do
    putStr $ (show x) ++ " "
    return (x + 1)

-- we can have an arbitrary monad AND do IO
addInitialM :: Vector Int -> VectorMonadT ()
addInitialM = mapVectorM_ $ \x -> do
    i <- get
    liftIO $ putStr $ (show $ x + i) ++ " "
    put $ x + i

-- sum the values of the even indiced elements
sumEvens :: Vector Int -> Int
sumEvens = foldVectorWithIndex (\x a b -> if x `mod` 2 == 0 then a + b else b) 0

-- sum and print running total of evens
sumEvensAndPrint :: Vector Int -> VectorMonadT ()
sumEvensAndPrint = mapVectorWithIndexM_ $ \ i x -> do
    when (i `mod` 2 == 0) $ do
        v <- get
        put $ v + x
        v' <- get
        liftIO $ putStr $ (show v') ++ " "


indexPlusSum :: Vector Int -> VectorMonadT ()
indexPlusSum v' = do
    let f i x = do
            s <- get
            let inc = x+s
            liftIO $ putStr $ show (i,inc) ++ " "
            put inc
            return inc
    v <- mapVectorWithIndexM f v'
    liftIO $ do
        putStrLn ""
        putStrLn $ show v

-------------------------------------------

-- short circuit
monoStep :: Double -> MaybeT (State Double) ()
monoStep d = do
    dp <- get
    when (d < dp) (fail "negative difference")
    put d
{-# INLINE monoStep #-}

isMonotoneIncreasing :: Vector Double -> Bool
isMonotoneIncreasing v =
    let res = evalState (runMaybeT $ (mapVectorM_ monoStep v)) (v @> 0)
     in case res of
        Nothing -> False
        Just _  -> True

w = fromList ([1..10]++[10,9..1]) :: Vector Double

-------------------------------------------

main = do
    v' <- test1 v
    putStrLn ""
    putStrLn $ show v'
    evalStateT (addInitialM v) 0
    putStrLn ""
    putStrLn $ show (sumEvens v)
    evalStateT (sumEvensAndPrint v) 0
    putStrLn ""
    evalStateT (indexPlusSum v) 0
    putStrLn "-----------------------"
    print $ isMonotoneIncreasing w
    print $ isMonotoneIncreasing (subVector 0 7 w)
    print $ successive_ (>) v
    print $ successive_ (>) w
