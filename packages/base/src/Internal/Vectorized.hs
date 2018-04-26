{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Numeric.Vectorized
-- Copyright   :  (c) Alberto Ruiz 2007-15
-- License     :  BSD3
-- Maintainer  :  Alberto Ruiz
-- Stability   :  provisional
--
-- Low level interface to vector operations.
--
-----------------------------------------------------------------------------

module Internal.Vectorized where

import Internal.Vector
import Internal.Devel
import Data.Complex
import Foreign.Marshal.Alloc(free,malloc)
import Foreign.Marshal.Array(newArray,copyArray)
import Foreign.Ptr(Ptr)
import Foreign.Storable(peek,Storable)
import Foreign.C.Types
import Foreign.C.String
import System.IO.Unsafe(unsafePerformIO)
import Control.Monad(when)

infixr 1 #
(#) :: TransArray c => c -> (b -> IO r) -> TransRaw c b -> IO r
a # b = applyRaw a b
{-# INLINE (#) #-}

(#!) :: (TransArray c, TransArray c1) => c1 -> c -> TransRaw c1 (TransRaw c (IO r)) -> IO r
a #! b = a # b # id
{-# INLINE (#!) #-}

fromei :: Enum a => a -> CInt
fromei x = fromIntegral (fromEnum x) :: CInt

data FunCodeV = Sin
              | Cos
              | Tan
              | Abs
              | ASin
              | ACos
              | ATan
              | Sinh
              | Cosh
              | Tanh
              | ASinh
              | ACosh
              | ATanh
              | Exp
              | Log
              | Sign
              | Sqrt
              deriving Enum

data FunCodeSV = Scale
               | Recip
               | AddConstant
               | Negate
               | PowSV
               | PowVS
               | ModSV
               | ModVS
               deriving Enum

data FunCodeVV = Add
               | Sub
               | Mul
               | Div
               | Pow
               | ATan2
               | Mod
               deriving Enum

data FunCodeS = Norm2
              | AbsSum
              | MaxIdx
              | Max
              | MinIdx
              | Min
              deriving Enum

------------------------------------------------------------------

-- | sum of elements
sumF :: Vector Float -> Float
sumF = sumg c_sumF

-- | sum of elements
sumR :: Vector Double -> Double
sumR = sumg c_sumR

-- | sum of elements
sumQ :: Vector (Complex Float) -> Complex Float
sumQ = sumg c_sumQ

-- | sum of elements
sumC :: Vector (Complex Double) -> Complex Double
sumC = sumg c_sumC

sumI :: ( TransRaw c (CInt -> Ptr a -> IO CInt) ~ (CInt -> Ptr I -> I :> Ok)
        , TransArray c
        , Storable a
        )
     => I -> c -> a
sumI m = sumg (c_sumI m)

sumL :: ( TransRaw c (CInt -> Ptr a -> IO CInt) ~ (CInt -> Ptr Z -> Z :> Ok)
        , TransArray c
        , Storable a
        ) => Z -> c -> a
sumL m = sumg (c_sumL m)

sumg :: (TransArray c, Storable a) => TransRaw c (CInt -> Ptr a -> IO CInt) -> c -> a
sumg f x = unsafePerformIO $ do
    r <- createVector 1
    (x #! r) f #| "sum"
    return $ r @> 0

type TVV t = t :> t :> Ok

foreign import ccall unsafe "sumF" c_sumF :: TVV Float
foreign import ccall unsafe "sumR" c_sumR :: TVV Double
foreign import ccall unsafe "sumQ" c_sumQ :: TVV (Complex Float)
foreign import ccall unsafe "sumC" c_sumC :: TVV (Complex Double)
foreign import ccall unsafe "sumI" c_sumI :: I -> TVV I
foreign import ccall unsafe "sumL" c_sumL :: Z -> TVV Z

-- | product of elements
prodF :: Vector Float -> Float
prodF = prodg c_prodF

-- | product of elements
prodR :: Vector Double -> Double
prodR = prodg c_prodR

-- | product of elements
prodQ :: Vector (Complex Float) -> Complex Float
prodQ = prodg c_prodQ

-- | product of elements
prodC :: Vector (Complex Double) -> Complex Double
prodC = prodg c_prodC

prodI :: I-> Vector I -> I
prodI = prodg . c_prodI

prodL :: Z-> Vector Z -> Z
prodL = prodg . c_prodL

prodg :: (TransArray c, Storable a)
      => TransRaw c (CInt -> Ptr a -> IO CInt) -> c -> a
prodg f x = unsafePerformIO $ do
    r <- createVector 1
    (x #! r) f #| "prod"
    return $ r @> 0


foreign import ccall unsafe "prodF" c_prodF :: TVV Float
foreign import ccall unsafe "prodR" c_prodR :: TVV Double
foreign import ccall unsafe "prodQ" c_prodQ :: TVV (Complex Float)
foreign import ccall unsafe "prodC" c_prodC :: TVV (Complex Double)
foreign import ccall unsafe "prodI" c_prodI :: I -> TVV I
foreign import ccall unsafe "prodL" c_prodL :: Z -> TVV Z

------------------------------------------------------------------

toScalarAux :: (Enum a, TransArray c, Storable a1)
            => (CInt -> TransRaw c (CInt -> Ptr a1 -> IO CInt)) -> a -> c -> a1
toScalarAux fun code v = unsafePerformIO $ do
    r <- createVector 1
    (v #! r) (fun (fromei code)) #|"toScalarAux"
    return (r @> 0)


vectorMapAux :: (Enum a, Storable t, Storable a1)
             => (CInt -> CInt -> Ptr t -> CInt -> Ptr a1 -> IO CInt)
             -> a -> Vector t -> Vector a1
vectorMapAux fun code v = unsafePerformIO $ do
    r <- createVector (dim v)
    (v #! r) (fun (fromei code)) #|"vectorMapAux"
    return r

vectorMapValAux :: (Enum a, Storable a2, Storable t, Storable a1)
                => (CInt -> Ptr a2 -> CInt -> Ptr t -> CInt -> Ptr a1 -> IO CInt)
                -> a -> a2 -> Vector t -> Vector a1
vectorMapValAux fun code val v = unsafePerformIO $ do
    r <- createVector (dim v)
    pval <- newArray [val]
    (v #! r) (fun (fromei code) pval) #|"vectorMapValAux"
    free pval
    return r

vectorZipAux :: (Enum a, TransArray c, Storable t, Storable a1)
             => (CInt -> CInt -> Ptr t -> TransRaw c (CInt -> Ptr a1 -> IO CInt))
             -> a -> Vector t -> c -> Vector a1
vectorZipAux fun code u v = unsafePerformIO $ do
    r <- createVector (dim u)
    (u # v #! r) (fun (fromei code)) #|"vectorZipAux"
    return r

---------------------------------------------------------------------

-- | obtains different functions of a vector: norm1, norm2, max, min, posmax, posmin, etc.
toScalarR :: FunCodeS -> Vector Double -> Double
toScalarR oper =  toScalarAux c_toScalarR (fromei oper)

foreign import ccall unsafe "toScalarR" c_toScalarR :: CInt -> TVV Double

-- | obtains different functions of a vector: norm1, norm2, max, min, posmax, posmin, etc.
toScalarF :: FunCodeS -> Vector Float -> Float
toScalarF oper =  toScalarAux c_toScalarF (fromei oper)

foreign import ccall unsafe "toScalarF" c_toScalarF :: CInt -> TVV Float

-- | obtains different functions of a vector: only norm1, norm2
toScalarC :: FunCodeS -> Vector (Complex Double) -> Double
toScalarC oper =  toScalarAux c_toScalarC (fromei oper)

foreign import ccall unsafe "toScalarC" c_toScalarC :: CInt -> Complex Double :> Double :> Ok

-- | obtains different functions of a vector: only norm1, norm2
toScalarQ :: FunCodeS -> Vector (Complex Float) -> Float
toScalarQ oper =  toScalarAux c_toScalarQ (fromei oper)

foreign import ccall unsafe "toScalarQ" c_toScalarQ :: CInt -> Complex Float :> Float :> Ok

-- | obtains different functions of a vector: norm1, norm2, max, min, posmax, posmin, etc.
toScalarI :: FunCodeS -> Vector CInt -> CInt
toScalarI oper =  toScalarAux c_toScalarI (fromei oper)

foreign import ccall unsafe "toScalarI" c_toScalarI :: CInt -> TVV CInt

-- | obtains different functions of a vector: norm1, norm2, max, min, posmax, posmin, etc.
toScalarL :: FunCodeS -> Vector Z -> Z
toScalarL oper =  toScalarAux c_toScalarL (fromei oper)

foreign import ccall unsafe "toScalarL" c_toScalarL :: CInt -> TVV Z


------------------------------------------------------------------

-- | map of real vectors with given function
vectorMapR :: FunCodeV -> Vector Double -> Vector Double
vectorMapR = vectorMapAux c_vectorMapR

foreign import ccall unsafe "mapR" c_vectorMapR :: CInt -> TVV Double

-- | map of complex vectors with given function
vectorMapC :: FunCodeV -> Vector (Complex Double) -> Vector (Complex Double)
vectorMapC oper = vectorMapAux c_vectorMapC (fromei oper)

foreign import ccall unsafe "mapC" c_vectorMapC :: CInt -> TVV (Complex Double)

-- | map of real vectors with given function
vectorMapF :: FunCodeV -> Vector Float -> Vector Float
vectorMapF = vectorMapAux c_vectorMapF

foreign import ccall unsafe "mapF" c_vectorMapF :: CInt -> TVV Float

-- | map of real vectors with given function
vectorMapQ :: FunCodeV -> Vector (Complex Float) -> Vector (Complex Float)
vectorMapQ = vectorMapAux c_vectorMapQ

foreign import ccall unsafe "mapQ" c_vectorMapQ :: CInt -> TVV (Complex Float)

-- | map of real vectors with given function
vectorMapI :: FunCodeV -> Vector CInt -> Vector CInt
vectorMapI = vectorMapAux c_vectorMapI

foreign import ccall unsafe "mapI" c_vectorMapI :: CInt -> TVV CInt

-- | map of real vectors with given function
vectorMapL :: FunCodeV -> Vector Z -> Vector Z
vectorMapL = vectorMapAux c_vectorMapL

foreign import ccall unsafe "mapL" c_vectorMapL :: CInt -> TVV Z

-------------------------------------------------------------------

-- | map of real vectors with given function
vectorMapValR :: FunCodeSV -> Double -> Vector Double -> Vector Double
vectorMapValR oper = vectorMapValAux c_vectorMapValR (fromei oper)

foreign import ccall unsafe "mapValR" c_vectorMapValR :: CInt -> Ptr Double -> TVV Double

-- | map of complex vectors with given function
vectorMapValC :: FunCodeSV -> Complex Double -> Vector (Complex Double) -> Vector (Complex Double)
vectorMapValC = vectorMapValAux c_vectorMapValC

foreign import ccall unsafe "mapValC" c_vectorMapValC :: CInt -> Ptr (Complex Double) -> TVV (Complex Double)

-- | map of real vectors with given function
vectorMapValF :: FunCodeSV -> Float -> Vector Float -> Vector Float
vectorMapValF oper = vectorMapValAux c_vectorMapValF (fromei oper)

foreign import ccall unsafe "mapValF" c_vectorMapValF :: CInt -> Ptr Float -> TVV Float

-- | map of complex vectors with given function
vectorMapValQ :: FunCodeSV -> Complex Float -> Vector (Complex Float) -> Vector (Complex Float)
vectorMapValQ oper = vectorMapValAux c_vectorMapValQ (fromei oper)

foreign import ccall unsafe "mapValQ" c_vectorMapValQ :: CInt -> Ptr (Complex Float) -> TVV (Complex Float)

-- | map of real vectors with given function
vectorMapValI :: FunCodeSV -> CInt -> Vector CInt -> Vector CInt
vectorMapValI oper = vectorMapValAux c_vectorMapValI (fromei oper)

foreign import ccall unsafe "mapValI" c_vectorMapValI :: CInt -> Ptr CInt -> TVV CInt

-- | map of real vectors with given function
vectorMapValL :: FunCodeSV -> Z -> Vector Z -> Vector Z
vectorMapValL oper = vectorMapValAux c_vectorMapValL (fromei oper)

foreign import ccall unsafe "mapValL" c_vectorMapValL :: CInt -> Ptr Z -> TVV Z


-------------------------------------------------------------------

type TVVV t = t :> t :> t :> Ok

-- | elementwise operation on real vectors
vectorZipR :: FunCodeVV -> Vector Double -> Vector Double -> Vector Double
vectorZipR = vectorZipAux c_vectorZipR

foreign import ccall unsafe "zipR" c_vectorZipR :: CInt -> TVVV Double

-- | elementwise operation on complex vectors
vectorZipC :: FunCodeVV -> Vector (Complex Double) -> Vector (Complex Double) -> Vector (Complex Double)
vectorZipC = vectorZipAux c_vectorZipC

foreign import ccall unsafe "zipC" c_vectorZipC :: CInt -> TVVV (Complex Double)

-- | elementwise operation on real vectors
vectorZipF :: FunCodeVV -> Vector Float -> Vector Float -> Vector Float
vectorZipF = vectorZipAux c_vectorZipF

foreign import ccall unsafe "zipF" c_vectorZipF :: CInt -> TVVV Float

-- | elementwise operation on complex vectors
vectorZipQ :: FunCodeVV -> Vector (Complex Float) -> Vector (Complex Float) -> Vector (Complex Float)
vectorZipQ = vectorZipAux c_vectorZipQ

foreign import ccall unsafe "zipQ" c_vectorZipQ :: CInt -> TVVV (Complex Float)

-- | elementwise operation on CInt vectors
vectorZipI :: FunCodeVV -> Vector CInt -> Vector CInt -> Vector CInt
vectorZipI = vectorZipAux c_vectorZipI

foreign import ccall unsafe "zipI" c_vectorZipI :: CInt -> TVVV CInt

-- | elementwise operation on CInt vectors
vectorZipL :: FunCodeVV -> Vector Z -> Vector Z -> Vector Z
vectorZipL = vectorZipAux c_vectorZipL

foreign import ccall unsafe "zipL" c_vectorZipL :: CInt -> TVVV Z

--------------------------------------------------------------------------------

foreign import ccall unsafe "vectorScan" c_vectorScan
    :: CString -> Ptr CInt -> Ptr (Ptr Double) -> IO CInt

vectorScan :: FilePath -> IO (Vector Double)
vectorScan s = do
    pp <- malloc
    pn <- malloc
    cs <- newCString s
    ok <- c_vectorScan cs pn pp
    when (not (ok == 0)) $
        error ("vectorScan: file \"" ++ s ++"\" not found")
    n <- fromIntegral <$> peek pn
    p <- peek pp
    v <- createVector n
    free pn
    free cs
    unsafeWith v $ \pv -> copyArray pv p n
    free p
    free pp
    return v

--------------------------------------------------------------------------------

type Seed = Int

data RandDist = Uniform  -- ^ uniform distribution in [0,1)
              | Gaussian -- ^ normal distribution with mean zero and standard deviation one
              deriving Enum

-- | Obtains a vector of pseudorandom elements (use randomIO to get a random seed).
randomVector :: Seed
             -> RandDist -- ^ distribution
             -> Int      -- ^ vector size
             -> Vector Double
randomVector seed dist n = unsafePerformIO $ do
    r <- createVector n
    (r # id) (c_random_vector (fi seed) ((fi.fromEnum) dist)) #|"randomVector"
    return r

foreign import ccall unsafe "random_vector" c_random_vector :: CInt -> CInt -> Double :> Ok

--------------------------------------------------------------------------------

roundVector :: Vector Double -> Vector Double
roundVector v = unsafePerformIO $ do
    r <- createVector (dim v)
    (v #! r) c_round_vector #|"roundVector"
    return r

foreign import ccall unsafe "round_vector" c_round_vector :: TVV Double

--------------------------------------------------------------------------------

-- |
-- >>> range 5
-- [0,1,2,3,4]
-- it :: Vector I
--
range :: Int -> Vector I
range n = unsafePerformIO $ do
    r <- createVector n
    (r # id) c_range_vector #|"range"
    return r

foreign import ccall unsafe "range_vector" c_range_vector :: CInt :> Ok


float2DoubleV :: Vector Float -> Vector Double
float2DoubleV = tog c_float2double

double2FloatV :: Vector Double -> Vector Float
double2FloatV = tog c_double2float

double2IntV :: Vector Double -> Vector CInt
double2IntV = tog c_double2int

int2DoubleV :: Vector CInt -> Vector Double
int2DoubleV = tog c_int2double

double2longV :: Vector Double -> Vector Z
double2longV = tog c_double2long

long2DoubleV :: Vector Z -> Vector Double
long2DoubleV = tog c_long2double


float2IntV :: Vector Float -> Vector CInt
float2IntV = tog c_float2int

int2floatV :: Vector CInt -> Vector Float
int2floatV = tog c_int2float

int2longV :: Vector I -> Vector Z
int2longV = tog c_int2long

long2intV :: Vector Z -> Vector I
long2intV = tog c_long2int


tog :: (Storable t, Storable a)
    => (CInt -> Ptr t -> CInt -> Ptr a -> IO CInt) -> Vector t -> Vector a
tog f v = unsafePerformIO $ do
    r <- createVector (dim v)
    (v #! r) f #|"tog"
    return r

foreign import ccall unsafe "float2double" c_float2double :: Float :> Double :> Ok
foreign import ccall unsafe "double2float" c_double2float :: Double :> Float :> Ok
foreign import ccall unsafe "int2double"   c_int2double   :: CInt :> Double :> Ok
foreign import ccall unsafe "double2int"   c_double2int   :: Double :> CInt :> Ok
foreign import ccall unsafe "long2double"  c_long2double   :: Z :> Double :> Ok
foreign import ccall unsafe "double2long"  c_double2long   :: Double :> Z :> Ok
foreign import ccall unsafe "int2float"    c_int2float    :: CInt :> Float :> Ok
foreign import ccall unsafe "float2int"    c_float2int    :: Float :> CInt :> Ok
foreign import ccall unsafe "int2long"    c_int2long    :: I :> Z :> Ok
foreign import ccall unsafe "long2int"    c_long2int    :: Z :> I :> Ok


---------------------------------------------------------------

stepg :: (Storable t, Storable a)
      => (CInt -> Ptr t -> CInt -> Ptr a -> IO CInt) -> Vector t -> Vector a
stepg f v = unsafePerformIO $ do
    r <- createVector (dim v)
    (v #! r) f #|"step"
    return r

stepD :: Vector Double -> Vector Double
stepD = stepg c_stepD

stepF :: Vector Float -> Vector Float
stepF = stepg c_stepF

stepI :: Vector CInt -> Vector CInt
stepI = stepg c_stepI

stepL :: Vector Z -> Vector Z
stepL = stepg c_stepL


foreign import ccall unsafe "stepF" c_stepF :: TVV Float
foreign import ccall unsafe "stepD" c_stepD :: TVV Double
foreign import ccall unsafe "stepI" c_stepI :: TVV CInt
foreign import ccall unsafe "stepL" c_stepL :: TVV Z

--------------------------------------------------------------------------------

conjugateAux :: (Storable t, Storable a)
             => (CInt -> Ptr t -> CInt -> Ptr a -> IO CInt) -> Vector t -> Vector a
conjugateAux fun x = unsafePerformIO $ do
    v <- createVector (dim x)
    (x #! v) fun #|"conjugateAux"
    return v

conjugateQ :: Vector (Complex Float) -> Vector (Complex Float)
conjugateQ = conjugateAux c_conjugateQ
foreign import ccall unsafe "conjugateQ" c_conjugateQ :: TVV (Complex Float)

conjugateC :: Vector (Complex Double) -> Vector (Complex Double)
conjugateC = conjugateAux c_conjugateC
foreign import ccall unsafe "conjugateC" c_conjugateC :: TVV (Complex Double)

--------------------------------------------------------------------------------

cloneVector :: Storable t => Vector t -> IO (Vector t)
cloneVector v = do
        let n = dim v
        r <- createVector n
        let f _ s _ d =  copyArray d s n >> return 0
        (v #! r) f #|"cloneVector"
        return r

--------------------------------------------------------------------------------

constantAux :: (Storable a1, Storable a)
            => (Ptr a1 -> CInt -> Ptr a -> IO CInt) -> a1 -> Int -> Vector a
constantAux fun x n = unsafePerformIO $ do
    v <- createVector n
    px <- newArray [x]
    (v # id) (fun px) #|"constantAux"
    free px
    return v

type TConst t = Ptr t -> t :> Ok

foreign import ccall unsafe "constantF" cconstantF :: TConst Float
foreign import ccall unsafe "constantR" cconstantR :: TConst Double
foreign import ccall unsafe "constantQ" cconstantQ :: TConst (Complex Float)
foreign import ccall unsafe "constantC" cconstantC :: TConst (Complex Double)
foreign import ccall unsafe "constantI" cconstantI :: TConst CInt
foreign import ccall unsafe "constantL" cconstantL :: TConst Z

----------------------------------------------------------------------
