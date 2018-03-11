{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE MultiWayIf #-}
import           Data.Coerce (coerce)
import           Data.Monoid ((<>))
import qualified Data.Vector.Storable as V
import qualified Data.Vector.Storable.Mutable as VM
import           Foreign.C.Types
import           Foreign.ForeignPtr (newForeignPtr_)
import           Foreign.Ptr (Ptr)
import           Foreign.Storable (Storable)
-- import qualified Graphics.Rendering.Chart.Backend.Cairo as Chart
-- import qualified Graphics.Rendering.Chart.Easy as Chart
import qualified Language.C.Inline as C
import qualified Language.C.Inline.Unsafe as CU
import           System.IO.Unsafe (unsafePerformIO)

-- #if __GLASGOW_HASKELL__ < 710
-- import           Data.Functor ((<$>))
-- #endif

C.context (C.baseCtx <> C.vecCtx <> C.funCtx)

C.include "<gsl/gsl_errno.h>"
C.include "<gsl/gsl_matrix.h>"
C.include "<gsl/gsl_odeiv2.h>"

-- | Solves a system of ODEs.  Every 'V.Vector' involved must be of the
-- same size.
{-# NOINLINE solveOdeC #-}
solveOdeC
  :: (CDouble -> V.Vector CDouble -> V.Vector CDouble)
  -- ^ ODE to Solve
  -> CDouble
  -- ^ Start
  -> V.Vector CDouble
  -- ^ Solution at start point
  -> CDouble
  -- ^ End
  -> Either String (V.Vector CDouble)
  -- ^ Solution at end point, or error.
solveOdeC fun x0 f0 xend = unsafePerformIO $ do
  let dim = V.length f0
  let dim_c = fromIntegral dim -- This is in CInt
  -- Convert the function to something of the right type to C.
  let funIO x y f _ptr = do
        -- Convert the pointer we get from C (y) to a vector, and then
        -- apply the user-supplied function.
        fImm <- fun x <$> vectorFromC dim y
        -- Fill in the provided pointer with the resulting vector.
        vectorToC fImm dim f
        -- Unsafe since the function will be called many times.
        [CU.exp| int{ GSL_SUCCESS } |]
  -- Create a mutable vector from the initial solution.  This will be
  -- passed to the ODE solving function provided by GSL, and will
  -- contain the final solution.
  fMut <- V.thaw f0
  res <- [C.block| int {
      gsl_odeiv2_system sys = {
        $fun:(int (* funIO) (double t, const double y[], double dydt[], void * params)),
        // The ODE to solve, converted to function pointer using the `fun`
        // anti-quoter
        NULL,                   // We don't provide a Jacobian
        $(int dim_c),           // The dimension
        NULL                    // We don't need the parameter pointer
      };
      // Create the driver, using some sensible values for the stepping
      // function and the tolerances
      gsl_odeiv2_driver *d = gsl_odeiv2_driver_alloc_y_new (
        &sys, gsl_odeiv2_step_rk8pd, 1e-6, 1e-6, 0.0);
      // Finally, apply the driver.
      int status = gsl_odeiv2_driver_apply(
        d, &$(double x0), $(double xend), $vec-ptr:(double *fMut));
      // Free the driver
      gsl_odeiv2_driver_free(d);
      return status;
    } |]
  -- Check the error code
  maxSteps <- [C.exp| int{ GSL_EMAXITER } |]
  smallStep <- [C.exp| int{ GSL_ENOPROG } |]
  good <- [C.exp| int{ GSL_SUCCESS } |]
  if | res == good -> Right <$> V.freeze fMut
     | res == maxSteps -> return $ Left "Too many steps"
     | res == smallStep -> return $ Left "Step size dropped below minimum allowed size"
     | otherwise -> return $ Left $ "Unknown error code " ++ show res

solveOde
  :: (Double -> V.Vector Double -> V.Vector Double)
  -- ^ ODE to Solve
  -> Double
  -- ^ Start
  -> V.Vector Double
  -- ^ Solution at start point
  -> Double
  -- ^ End
  -> Either String (V.Vector Double)
  -- ^ Solution at end point, or error.
solveOde fun x0 f0 xend =
  coerce $ solveOdeC (coerce fun) (coerce x0) (coerce f0) (coerce xend)

lorenz
  :: Double
  -- ^ Starting point
  -> V.Vector Double
  -- ^ Solution at starting point
  -> Double
  -- ^ End point
  -> Either String (V.Vector Double)
lorenz x0 f0 xend = solveOde fun x0 f0 xend
  where
    sigma = 10.0;
    _R = 28.0;
    b = 8.0 / 3.0;

    fun _x y =
      let y0 = y V.! 0
          y1 = y V.! 1
          y2 = y V.! 2
      in V.fromList
           [ sigma * ( y1 - y0 )
           , _R * y0 - y1 - y0 * y2
           , -b * y2 + y0 * y1
           ]

main :: IO ()
main = undefined
-- main = Chart.toFile Chart.def "lorenz.png" $ do
--     Chart.layout_title Chart..= "Lorenz"
--     Chart.plot $ Chart.line "curve" [pts]
--   where
--     pts = [(f V.! 0, f V.! 2) | (_x, f) <- go 0 (V.fromList [10.0 , 1.0 , 1.0])]

--     go x f | x > 40 =
--       [(x, f)]
--     go x f =
--       let x' = x + 0.01
--           Right f' = lorenz x f x'
--       in (x, f) : go x' f'

-- Utils

vectorFromC :: Storable a => Int -> Ptr a -> IO (V.Vector a)
vectorFromC len ptr = do
  ptr' <- newForeignPtr_ ptr
  V.freeze $ VM.unsafeFromForeignPtr0 ptr' len

vectorToC :: Storable a => V.Vector a -> Int -> Ptr a -> IO ()
vectorToC vec len ptr = do
  ptr' <- newForeignPtr_ ptr
  V.copy (VM.unsafeFromForeignPtr0 ptr' len) vec
