{- |
Module      :  Numeric.GSL.Interpolation
Copyright   :  (c) Matthew Peddie 2015
License     :  GPL
Maintainer  :  Alberto Ruiz
Stability   :  provisional

Simulated annealing routines.

<https://www.gnu.org/software/gsl/manual/html_node/Simulated-Annealing.html#Simulated-Annealing>

Here is a translation of the simple example given in
<https://www.gnu.org/software/gsl/manual/html_node/Trivial-example.html#Trivial-example the GSL manual>:

> import Numeric.GSL.SimulatedAnnealing
> import Numeric.LinearAlgebra.HMatrix
>
> main = print $ simanSolve 0 1 exampleParams 15.5 exampleE exampleM exampleS (Just show)
>
> exampleParams = SimulatedAnnealingParams 200 1000 1.0 1.0 0.008 1.003 2.0e-6
>
> exampleE x = exp (-(x - 1)**2) * sin (8 * x)
>
> exampleM x y = abs $ x - y
>
> exampleS rands stepSize current = (rands ! 0) * 2 * stepSize - stepSize + current

The manual states:

>     The first example, in one dimensional Cartesian space, sets up an
>     energy function which is a damped sine wave; this has many local
>     minima, but only one global minimum, somewhere between 1.0 and
>     1.5. The initial guess given is 15.5, which is several local minima
>     away from the global minimum.

This global minimum is around 1.36.

-}
{-# OPTIONS_GHC -Wall #-}

module Numeric.GSL.SimulatedAnnealing (
  -- * Searching for minima
  simanSolve
  -- * Configuring the annealing process
  , SimulatedAnnealingParams(..)
  ) where

import Numeric.GSL.Internal
import Numeric.LinearAlgebra.HMatrix hiding(step)

import Data.Vector.Storable(generateM)
import Foreign.Storable(Storable(..))
import Foreign.Marshal.Utils(with)
import Foreign.Ptr(Ptr, FunPtr, nullFunPtr)
import Foreign.StablePtr(StablePtr, newStablePtr, deRefStablePtr, freeStablePtr)
import Foreign.C.Types
import System.IO.Unsafe(unsafePerformIO)

import System.IO (hFlush, stdout)

import Data.IORef (IORef, newIORef, writeIORef, readIORef, modifyIORef')

-- | 'SimulatedAnnealingParams' is a translation of the
-- @gsl_siman_params_t@ structure documented in
-- <https://www.gnu.org/software/gsl/manual/html_node/Simulated-Annealing-functions.html#Simulated-Annealing-functions the GSL manual>,
-- which controls the simulated annealing algorithm.
--
-- The annealing process is parameterized by the Boltzmann
-- distribution and the /cooling schedule/.  For more details, see
-- <https://www.gnu.org/software/gsl/manual/html_node/Simulated-Annealing-algorithm.html#Simulated-Annealing-algorithm the relevant section of the manual>.
data SimulatedAnnealingParams = SimulatedAnnealingParams {
  n_tries :: CInt  -- ^ The number of points to try for each step.
  , iters_fixed_T :: CInt  -- ^ The number of iterations at each temperature
  , step_size :: Double    -- ^ The maximum step size in the random walk
  , boltzmann_k :: Double  -- ^ Boltzmann distribution parameter
  , cooling_t_initial :: Double -- ^ Initial temperature
  , cooling_mu_t :: Double      -- ^ Cooling rate parameter
  , cooling_t_min :: Double     -- ^ Final temperature
  } deriving (Eq, Show, Read)

instance Storable SimulatedAnnealingParams where
  sizeOf p = sizeOf (n_tries p) +
             sizeOf (iters_fixed_T p) +
             sizeOf (step_size p) +
             sizeOf (boltzmann_k p) +
             sizeOf (cooling_t_initial p) +
             sizeOf (cooling_mu_t p) +
             sizeOf (cooling_t_min p)
  -- TODO(MP): is this safe?
  alignment p = alignment (step_size p)
  -- TODO(MP): Is there a more automatic way to write these?
  peek ptr = SimulatedAnnealingParams <$>
             peekByteOff ptr 0 <*>
             peekByteOff ptr i <*>
             peekByteOff ptr (2*i) <*>
             peekByteOff ptr (2*i + d) <*>
             peekByteOff ptr (2*i + 2*d) <*>
             peekByteOff ptr (2*i + 3*d) <*>
             peekByteOff ptr (2*i + 4*d)
    where
      i = sizeOf (0 :: CInt)
      d = sizeOf (0 :: Double)
  poke ptr sap = do
    pokeByteOff ptr 0 (n_tries sap)
    pokeByteOff ptr i (iters_fixed_T sap)
    pokeByteOff ptr (2*i) (step_size sap)
    pokeByteOff ptr (2*i + d) (boltzmann_k sap)
    pokeByteOff ptr (2*i + 2*d) (cooling_t_initial sap)
    pokeByteOff ptr (2*i + 3*d) (cooling_mu_t sap)
    pokeByteOff ptr (2*i + 4*d) (cooling_t_min sap)
    where
      i = sizeOf (0 :: CInt)
      d = sizeOf (0 :: Double)

-- We use a StablePtr to an IORef so that we can keep hold of
-- StablePtr values but mutate their contents.  A simple 'StablePtr a'
-- won't work, since we'd have no way to write 'copyConfig'.
type P a = StablePtr (IORef a)

copyConfig :: P a -> P a -> IO ()
copyConfig src' dest' = do
  dest <- deRefStablePtr dest'
  src <- deRefStablePtr src'
  readIORef src >>= writeIORef dest

copyConstructConfig :: P a -> IO (P a)
copyConstructConfig x = do
  conf <- deRefRead x
  newconf <- newIORef conf
  newStablePtr newconf

destroyConfig :: P a -> IO ()
destroyConfig p = do
  freeStablePtr p

deRefRead :: P a -> IO a
deRefRead p = deRefStablePtr p >>= readIORef

wrapEnergy :: (a -> Double) -> P a -> Double
wrapEnergy f p = unsafePerformIO $ f <$> deRefRead p

wrapMetric :: (a -> a -> Double) -> P a -> P a -> Double
wrapMetric f x y = unsafePerformIO $ f <$> deRefRead x <*> deRefRead y

wrapStep :: Int
         -> (Vector Double -> Double -> a -> a)
         -> GSLRNG
         -> P a
         -> Double
         -> IO ()
wrapStep nrand f (GSLRNG rng) confptr stepSize = do
  v <- generateM nrand (\_ -> gslRngUniform rng)
  conf <- deRefStablePtr confptr
  modifyIORef' conf $ f v stepSize

wrapPrint :: (a -> String) -> P a -> IO ()
wrapPrint pf ptr = deRefRead ptr >>= putStr . pf >> hFlush stdout

foreign import ccall safe "wrapper"
  mkEnergyFun :: (P a -> Double) -> IO (FunPtr (P a -> Double))

foreign import ccall safe "wrapper"
  mkMetricFun :: (P a -> P a -> Double) -> IO (FunPtr (P a -> P a -> Double))

foreign import ccall safe "wrapper"
  mkStepFun :: (GSLRNG -> P a -> Double -> IO ())
            -> IO (FunPtr (GSLRNG -> P a -> Double -> IO ()))

foreign import ccall safe "wrapper"
  mkCopyFun :: (P a -> P a -> IO ()) -> IO (FunPtr (P a -> P a -> IO ()))

foreign import ccall safe "wrapper"
  mkCopyConstructorFun :: (P a -> IO (P a)) -> IO (FunPtr (P a -> IO (P a)))

foreign import ccall safe "wrapper"
  mkDestructFun :: (P a -> IO ()) -> IO (FunPtr (P a -> IO ()))

newtype GSLRNG = GSLRNG (Ptr GSLRNG)

foreign import ccall safe "gsl_rng.h gsl_rng_uniform"
  gslRngUniform :: Ptr GSLRNG -> IO Double

foreign import ccall safe "gsl-aux.h siman"
  siman :: CInt     -- ^ RNG seed (for repeatability)
        -> Ptr SimulatedAnnealingParams    -- ^ params
        -> P a                             -- ^ Configuration
        -> FunPtr (P a -> Double)          -- ^ Energy functional
        -> FunPtr (P a -> P a -> Double) -- ^ Metric definition
        -> FunPtr (GSLRNG -> P a -> Double -> IO ())  -- ^ Step evaluation
        -> FunPtr (P a -> P a -> IO ())  -- ^ Copy config
        -> FunPtr (P a -> IO (P a))      -- ^ Copy constructor for config
        -> FunPtr (P a -> IO ())           -- ^ Destructor for config
        -> FunPtr (P a -> IO ())           -- ^ Print function
        -> IO CInt

-- |
-- Calling
--
-- > simanSolve seed nrand params x0 e m step print
--
-- performs a simulated annealing search through a given space. So
-- that any configuration type may be used, the space is specified by
-- providing the functions @e@ (the energy functional) and @m@ (the
-- metric definition).  @x0@ is the initial configuration of the
-- system.  The simulated annealing steps are generated using the
-- user-provided function @step@, which should randomly construct a
-- new system configuration.
--
-- If 'Nothing' is passed instead of a printing function, no
-- incremental output will be generated.  Otherwise, the GSL-formatted
-- output, including the configuration description the user function
-- generates, will be printed to stdout.
--
-- Each time the step function is called, it is supplied with a random
-- vector containing @nrand@ 'Double' values, uniformly distributed in
-- @[0, 1)@.  It should use these values to generate its new
-- configuration.
simanSolve :: Int   -- ^ Seed for the random number generator
           -> Int   -- ^ @nrand@, the number of random 'Double's the
                    -- step function requires
           -> SimulatedAnnealingParams  -- ^ Parameters to configure the solver
           -> a                    -- ^ Initial configuration @x0@
           -> (a -> Double)        -- ^ Energy functional @e@
           -> (a -> a -> Double)   -- ^ Metric definition @m@
           -> (Vector Double -> Double -> a -> a)  -- ^ Stepping function @step@
           -> Maybe (a -> String)  -- ^ Optional printing function
           -> a          -- ^ Best configuration the solver has found
simanSolve seed nrand params conf e m step printfun =
  unsafePerformIO $ with params $ \paramptr -> do
    ewrap <- mkEnergyFun $ wrapEnergy e
    mwrap <- mkMetricFun $ wrapMetric m
    stepwrap <- mkStepFun $ wrapStep nrand step
    confptr <- newIORef conf >>= newStablePtr
    cpwrap <- mkCopyFun copyConfig
    ccwrap <- mkCopyConstructorFun copyConstructConfig
    dwrap <- mkDestructFun destroyConfig
    pwrap <- case printfun of
      Nothing -> return nullFunPtr
      Just pf -> mkDestructFun $ wrapPrint pf
    siman (fromIntegral seed)
      paramptr confptr
      ewrap mwrap stepwrap cpwrap ccwrap dwrap pwrap // check "siman"
    result <- deRefRead confptr
    freeStablePtr confptr
    return result
