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
import           Control.Exception (SomeException, SomeAsyncException (..))
import qualified Control.Exception as Exception
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
(#|) :: IO CInt -> String -> IO ()
(#|) = flip check

-- | Error capture and conversion to Maybe
mbCatch :: IO x -> IO (Maybe x)
mbCatch act =
  hush <$>
    Exception.tryJust
      (\e -> if isSyncException e then Just e else Nothing)
      act

  where
    hush :: Either a b -> Maybe b
    hush = either (const Nothing) Just

    isSyncException :: SomeException -> Bool
    isSyncException e =
      case Exception.fromException e of
        Just (SomeAsyncException _) -> False
        Nothing -> True

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

class TransArray c
  where
    type Trans c b
    type TransRaw c b
    apply      :: c -> (b -> IO r) -> (Trans c b) -> IO r
    applyRaw   :: c -> (b -> IO r) -> (TransRaw c b) -> IO r
    infixl 1 `apply`, `applyRaw`

instance Storable t => TransArray (Vector t)
  where
    type Trans (Vector t) b    = CInt -> Ptr t -> b
    type TransRaw (Vector t) b = CInt -> Ptr t -> b
    apply = avec
    {-# INLINE apply #-}
    applyRaw = avec
    {-# INLINE applyRaw #-}
