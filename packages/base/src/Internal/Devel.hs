{-# LANGUAGE TypeOperators            #-}

-- |
-- Module      :  Internal.Devel
-- Copyright   :  (c) Alberto Ruiz 2007-15
-- License     :  BSD3
-- Maintainer  :  Alberto Ruiz
-- Stability   :  provisional
--

module Internal.Devel where


import Control.Monad ( when )
import Foreign.C.Types ( CInt )
--import Foreign.Storable.Complex ()
import Foreign.Ptr(Ptr)
import Control.Exception as E ( SomeException, catch )


-- | postfix function application (@flip ($)@)
(//) :: x -> (x -> y) -> y
infixl 0 //
(//) = flip ($)

-- hmm..
ww2 w1 o1 w2 o2 f = w1 o1 $ w2 o2 . f
ww3 w1 o1 w2 o2 w3 o3 f = w1 o1 $ ww2 w2 o2 w3 o3 . f
ww4 w1 o1 w2 o2 w3 o3 w4 o4 f = w1 o1 $ ww3 w2 o2 w3 o3 w4 o4 . f
ww5 w1 o1 w2 o2 w3 o3 w4 o4 w5 o5 f = w1 o1 $ ww4 w2 o2 w3 o3 w4 o4 w5 o5 . f
ww6 w1 o1 w2 o2 w3 o3 w4 o4 w5 o5 w6 o6 f = w1 o1 $ ww5 w2 o2 w3 o3 w4 o4 w5 o5 w6 o6 . f
ww7 w1 o1 w2 o2 w3 o3 w4 o4 w5 o5 w6 o6 w7 o7 f = w1 o1 $ ww6 w2 o2 w3 o3 w4 o4 w5 o5 w6 o6 w7 o7 . f
ww8 w1 o1 w2 o2 w3 o3 w4 o4 w5 o5 w6 o6 w7 o7 w8 o8 f = w1 o1 $ ww7 w2 o2 w3 o3 w4 o4 w5 o5 w6 o6 w7 o7 w8 o8 . f
ww9 w1 o1 w2 o2 w3 o3 w4 o4 w5 o5 w6 o6 w7 o7 w8 o8 w9 o9 f = w1 o1 $ ww8 w2 o2 w3 o3 w4 o4 w5 o5 w6 o6 w7 o7 w8 o8 w9 o9 . f
ww10 w1 o1 w2 o2 w3 o3 w4 o4 w5 o5 w6 o6 w7 o7 w8 o8 w9 o9 w10 o10 f = w1 o1 $ ww9 w2 o2 w3 o3 w4 o4 w5 o5 w6 o6 w7 o7 w8 o8 w9 o9 w10 o10 . f

type Adapt f t r = t -> ((f -> r) -> IO()) -> IO()

type Adapt1 f t1 = Adapt f t1 (IO CInt) -> t1 -> String -> IO()
type Adapt2 f t1 r1 t2 = Adapt f t1 r1 -> t1 -> Adapt1 r1 t2
type Adapt3 f t1 r1 t2 r2 t3 = Adapt f t1 r1 -> t1 -> Adapt2 r1 t2 r2 t3
type Adapt4 f t1 r1 t2 r2 t3 r3 t4 = Adapt f t1 r1 -> t1 -> Adapt3 r1 t2 r2 t3 r3 t4
type Adapt5 f t1 r1 t2 r2 t3 r3 t4 r4 t5 = Adapt f t1 r1 -> t1 -> Adapt4 r1 t2 r2 t3 r3 t4 r4 t5
type Adapt6 f t1 r1 t2 r2 t3 r3 t4 r4 t5 r5 t6 = Adapt f t1 r1 -> t1 -> Adapt5 r1 t2 r2 t3 r3 t4 r4 t5 r5 t6
type Adapt7 f t1 r1 t2 r2 t3 r3 t4 r4 t5 r5 t6 r6 t7 = Adapt f t1 r1 -> t1 -> Adapt6 r1 t2 r2 t3 r3 t4 r4 t5 r5 t6 r6 t7
type Adapt8 f t1 r1 t2 r2 t3 r3 t4 r4 t5 r5 t6 r6 t7 r7 t8 = Adapt f t1 r1 -> t1 -> Adapt7 r1 t2 r2 t3 r3 t4 r4 t5 r5 t6 r6 t7 r7 t8
type Adapt9 f t1 r1 t2 r2 t3 r3 t4 r4 t5 r5 t6 r6 t7 r7 t8 r8 t9 = Adapt f t1 r1 -> t1 -> Adapt8 r1 t2 r2 t3 r3 t4 r4 t5 r5 t6 r6 t7 r7 t8 r8 t9
type Adapt10 f t1 r1 t2 r2 t3 r3 t4 r4 t5 r5 t6 r6 t7 r7 t8 r8 t9 r9 t10 = Adapt f t1 r1 -> t1 -> Adapt9 r1 t2 r2 t3 r3 t4 r4 t5 r5 t6 r6 t7 r7 t8 r8 t9 r9 t10

app1 :: f -> Adapt1 f t1
app2 :: f -> Adapt2 f t1 r1 t2
app3 :: f -> Adapt3 f t1 r1 t2 r2 t3
app4 :: f -> Adapt4 f t1 r1 t2 r2 t3 r3 t4
app5 :: f -> Adapt5 f t1 r1 t2 r2 t3 r3 t4 r4 t5
app6 :: f -> Adapt6 f t1 r1 t2 r2 t3 r3 t4 r4 t5 r5 t6
app7 :: f -> Adapt7 f t1 r1 t2 r2 t3 r3 t4 r4 t5 r5 t6 r6 t7
app8 :: f -> Adapt8 f t1 r1 t2 r2 t3 r3 t4 r4 t5 r5 t6 r6 t7 r7 t8
app9 :: f -> Adapt9 f t1 r1 t2 r2 t3 r3 t4 r4 t5 r5 t6 r6 t7 r7 t8 r8 t9
app10 :: f -> Adapt10 f t1 r1 t2 r2 t3 r3 t4 r4 t5 r5 t6 r6 t7 r7 t8 r8 t9 r9 t10

app1 f w1 o1 s = w1 o1 $ \a1 -> f // a1 // check s
app2 f w1 o1 w2 o2 s = ww2 w1 o1 w2 o2 $ \a1 a2 -> f // a1 // a2 // check s
app3 f w1 o1 w2 o2 w3 o3 s = ww3 w1 o1 w2 o2 w3 o3 $
     \a1 a2 a3 -> f // a1 // a2 // a3 // check s
app4 f w1 o1 w2 o2 w3 o3 w4 o4 s = ww4 w1 o1 w2 o2 w3 o3 w4 o4 $
     \a1 a2 a3 a4 -> f // a1 // a2 // a3 // a4 // check s
app5 f w1 o1 w2 o2 w3 o3 w4 o4 w5 o5 s = ww5 w1 o1 w2 o2 w3 o3 w4 o4 w5 o5 $
     \a1 a2 a3 a4 a5 -> f // a1 // a2 // a3 // a4 // a5 // check s
app6 f w1 o1 w2 o2 w3 o3 w4 o4 w5 o5 w6 o6 s = ww6 w1 o1 w2 o2 w3 o3 w4 o4 w5 o5 w6 o6 $
     \a1 a2 a3 a4 a5 a6 -> f // a1 // a2 // a3 // a4 // a5 // a6 // check s
app7 f w1 o1 w2 o2 w3 o3 w4 o4 w5 o5 w6 o6 w7 o7 s = ww7 w1 o1 w2 o2 w3 o3 w4 o4 w5 o5 w6 o6 w7 o7 $
     \a1 a2 a3 a4 a5 a6 a7 -> f // a1 // a2 // a3 // a4 // a5 // a6 // a7 // check s
app8 f w1 o1 w2 o2 w3 o3 w4 o4 w5 o5 w6 o6 w7 o7 w8 o8 s = ww8 w1 o1 w2 o2 w3 o3 w4 o4 w5 o5 w6 o6 w7 o7 w8 o8 $
     \a1 a2 a3 a4 a5 a6 a7 a8 -> f // a1 // a2 // a3 // a4 // a5 // a6 // a7 // a8 // check s
app9 f w1 o1 w2 o2 w3 o3 w4 o4 w5 o5 w6 o6 w7 o7 w8 o8 w9 o9 s = ww9 w1 o1 w2 o2 w3 o3 w4 o4 w5 o5 w6 o6 w7 o7 w8 o8 w9 o9 $
     \a1 a2 a3 a4 a5 a6 a7 a8 a9 -> f // a1 // a2 // a3 // a4 // a5 // a6 // a7 // a8 // a9 // check s
app10 f w1 o1 w2 o2 w3 o3 w4 o4 w5 o5 w6 o6 w7 o7 w8 o8 w9 o9 w10 o10 s = ww10 w1 o1 w2 o2 w3 o3 w4 o4 w5 o5 w6 o6 w7 o7 w8 o8 w9 o9 w10 o10 $
     \a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 -> f // a1 // a2 // a3 // a4 // a5 // a6 // a7 // a8 // a9 // a10 // check s



-- GSL error codes are <= 1024
-- | error codes for the auxiliary functions required by the wrappers
errorCode :: CInt -> String
errorCode 2000 = "bad size"
errorCode 2001 = "bad function code"
errorCode 2002 = "memory problem"
errorCode 2003 = "bad file"
errorCode 2004 = "singular"
errorCode 2005 = "didn't converge"
errorCode 2006 = "the input matrix is not positive definite"
errorCode 2007 = "not yet supported in this OS"
errorCode n    = "code "++show n


-- | clear the fpu
foreign import ccall unsafe "asm_finit" finit :: IO ()

-- | check the error code
check :: String -> IO CInt -> IO ()
check msg f = do
--  finit
    err <- f
    when (err/=0) $ error (msg++": "++errorCode err)
    return ()

-- | Error capture and conversion to Maybe
mbCatch :: IO x -> IO (Maybe x)
mbCatch act = E.catch (Just `fmap` act) f
    where f :: SomeException -> IO (Maybe x)
          f _ = return Nothing

--------------------------------------------------------------------------------

type CM b r = CInt -> CInt -> Ptr b -> r
type CV b r = CInt -> Ptr b -> r
type OM b r = CInt -> CInt -> CInt -> CInt -> Ptr b -> r

type CIdxs r = CV CInt r
type Ok = IO CInt

infixr 5 :>, ::>, ..>
type (:>)  t r = CV t r
type (::>) t r = OM t r
type (..>) t r = CM t r


