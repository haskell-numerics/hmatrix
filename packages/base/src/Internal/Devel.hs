{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}

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
import Internal.Vector(Vector,avec)
import Foreign.Storable(Storable)

-- | postfix function application (@flip ($)@)
(//) :: x -> (x -> y) -> y
infixl 0 //
(//) = flip ($)


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


-- | postfix error code check
infixl 0 #|
(#|) = flip check

-- | Error capture and conversion to Maybe
mbCatch :: IO x -> IO (Maybe x)
mbCatch act = E.catch (Just `fmap` act) f
    where f :: SomeException -> IO (Maybe x)
          f _ = return Nothing

--------------------------------------------------------------------------------

type CV b r = CInt -> Ptr b -> r
type OM b r = CInt -> CInt -> CInt -> CInt -> Ptr b -> r

type CIdxs r = CV CInt r
type Ok = IO CInt

infixr 5 :>, ::>
type (:>)  t r = CV t r
type (::>) t r = OM t r

class TransArray c
  where
    type Trans c b
    apply      :: (Trans c b) -> c -> b
    infixl 1 `apply`

instance Storable t => TransArray (Vector t)
  where
    type Trans (Vector t) b    = CInt -> Ptr t -> b
    apply = avec
    {-# INLINE apply #-}

