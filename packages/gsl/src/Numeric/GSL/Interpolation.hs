{-# LANGUAGE MagicHash, UnboxedTuples #-}

{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

{- |
Module      :  Numeric.GSL.Interpolation
Copyright   :  (c) Matthew Peddie 2015
License     :  GPL
Maintainer  :  Alberto Ruiz
Stability   :  provisional

Interpolation routines.

<https://www.gnu.org/software/gsl/manual/html_node/Interpolation.html#Interpolation>

The GSL routines @gsl_spline_eval@ and friends are used, but in spite
of the names, they are not restricted to spline interpolation.  The
functions in this module will work for any 'InterpolationMethod'.

-}


module Numeric.GSL.Interpolation (
  -- * Interpolation methods
  InterpolationMethod(..)
  -- * Evaluation of interpolated functions
  , evaluate
  , evaluateV
    -- * Evaluation of derivatives of interpolated functions
  , evaluateDerivative
  , evaluateDerivative2
  , evaluateDerivativeV
  , evaluateDerivative2V
    -- * Evaluation of integrals of interpolated functions
  , evaluateIntegral
  , evaluateIntegralV
) where

import Numeric.LinearAlgebra(Vector, fromList, size, Numeric)
import Foreign.C.Types
import Foreign.Marshal.Alloc(alloca)
import Foreign.Ptr(Ptr)
import Foreign.Storable(peek)
import Numeric.GSL.Internal
import System.IO.Unsafe(unsafePerformIO)

-- FIXME
import qualified Data.Vector.Storable as S
import GHC.Base (IO(..), realWorld#)

data InterpolationMethod = Linear
                         | Polynomial
                         | CSpline
                         | CSplinePeriodic
                         | Akima
                         | AkimaPeriodic
                         deriving (Eq, Show, Read)

methodToInt :: Integral a => InterpolationMethod -> a
methodToInt Linear = 0
methodToInt Polynomial = 1
methodToInt CSpline = 2
methodToInt CSplinePeriodic = 3
methodToInt Akima = 4
methodToInt AkimaPeriodic = 5

dim :: Numeric t => Vector t -> Int
dim = size

-- FIXME
appVector f x = unsafeInlinePerformIO (S.unsafeWith x (return . f))

unsafeInlinePerformIO (IO f) = case f realWorld# of
    (# _, x #) -> x

applyCFun hsname cname fun mth xs ys x
  | dim xs /= dim ys = error $
                         "Error: Vectors of unequal sizes " ++
                         show (dim xs) ++ " and " ++ show (dim ys) ++
                         " supplied to " ++ hsname
  | otherwise = unsafePerformIO $
      flip appVector xs $ \xs' ->
       flip appVector ys $ \ys' ->
        alloca $ \y' -> do
          fun xs' ys'
            (fromIntegral $ dim xs) x
            (methodToInt mth) y' // check cname
          peek y'

foreign import ccall safe "spline_eval" c_spline_eval
  :: Ptr Double -> Ptr Double -> CUInt -> Double -> CInt -> Ptr Double -> IO CInt

--------------------------------------------------------------------
{- | Evaluate a function by interpolating within the given dataset.  For
example:

>>> let xs = vector [1..10]
>>> let ys = vector $ map (**2) [1..10]
>>> evaluateV CSpline xs ys 2.2
4.818867924528303

To successfully @evaluateV xs ys x@, the vectors of corresponding
domain-range values @xs@ and @ys@ must have identical lengths, and
@xs@ must be monotonically increasing.  The evaluation point @x@ must
lie between the smallest and largest values in @xs@.

-}
evaluateV :: InterpolationMethod  -- ^ What method to use to interpolate
             -> Vector Double     -- ^ Data points sampling the domain of the function
             -> Vector Double     -- ^ Data points sampling the range of the function
             -> Double            -- ^ Point at which to evaluate the function
             -> Double            -- ^ Interpolated result
evaluateV = applyCFun "evaluateV" "spline_eval" c_spline_eval

{- | Evaluate a function by interpolating within the given dataset.  For
example:

>>> let xs = [1..10]
>>> let ys map (**2) [1..10]
>>> evaluate Akima (zip xs ys) 2.2
4.840000000000001

To successfully @evaluate points x@, the domain (@x@) values in
@points@ must be monotonically increasing.  The evaluation point @x@
must lie between the smallest and largest values in the sampled
domain.

-}
evaluate :: InterpolationMethod    -- ^ What method to use to interpolate
            -> [(Double, Double)]  -- ^ (domain, range) values sampling the function
            -> Double              -- ^ Point at which to evaluate the function
            -> Double              -- ^ Interpolated result
evaluate mth pts =
  applyCFun "evaluate" "spline_eval" c_spline_eval
  mth (fromList xs) (fromList ys)
  where
    (xs, ys) = unzip pts

foreign import ccall safe "spline_eval_deriv" c_spline_eval_deriv
  :: Ptr Double -> Ptr Double -> CUInt -> Double -> CInt -> Ptr Double -> IO CInt

{- | Evaluate the derivative of a function by interpolating within the
given dataset.  For example:

>>> let xs = vector [1..10]
>>> let ys = vector $ map (**2) [1..10]
>>> evaluateDerivativeV CSpline xs ys 2.2
4.338867924528302

To successfully @evaluateDerivativeV xs ys x@, the vectors of
corresponding domain-range values @xs@ and @ys@ must have identical
lengths, and @xs@ must be monotonically increasing.  The interpolation
point @x@ must lie between the smallest and largest values in @xs@.

-}
evaluateDerivativeV :: InterpolationMethod  -- ^ What method to use to interpolate
                       -> Vector Double     -- ^ Data points @xs@ sampling the domain of the function
                       -> Vector Double     -- ^ Data points @ys@ sampling the range of the function
                       -> Double            -- ^ Point @x@ at which to evaluate the derivative
                       -> Double            -- ^ Interpolated result
evaluateDerivativeV =
  applyCFun "evaluateDerivativeV" "spline_eval_deriv" c_spline_eval_deriv

{- | Evaluate the derivative of a function by interpolating within the
given dataset.  For example:

>>> let xs = [1..10]
>>> let ys map (**2) [1..10]
>>> evaluateDerivative Akima (zip xs ys) 2.2
4.4

To successfully @evaluateDerivative points x@, the domain (@x@) values
in @points@ must be monotonically increasing.  The evaluation point
@x@ must lie between the smallest and largest values in the sampled
domain.

-}
evaluateDerivative :: InterpolationMethod    -- ^ What method to use to interpolate
                      -> [(Double, Double)]  -- ^ (domain, range) points sampling the function
                      -> Double              -- ^ Point @x@ at which to evaluate the derivative
                      -> Double              -- ^ Interpolated result
evaluateDerivative mth pts =
  applyCFun "evaluateDerivative" "spline_eval_deriv" c_spline_eval_deriv
  mth (fromList xs) (fromList ys)
  where
    (xs, ys) = unzip pts

foreign import ccall safe "spline_eval_deriv2" c_spline_eval_deriv2
  :: Ptr Double -> Ptr Double -> CUInt -> Double -> CInt -> Ptr Double -> IO CInt

{- | Evaluate the second derivative of a function by interpolating within the
given dataset.  For example:

>>> let xs = vector [1..10]
>>> let ys = vector $ map (**2) [1..10]
>>> evaluateDerivative2V CSpline xs ys 2.2
2.4

To successfully @evaluateDerivative2V xs ys x@, the vectors @xs@ and
@ys@ must have identical lengths, and @xs@ must be monotonically
increasing.  The evaluation point @x@ must lie between the smallest
and largest values in @xs@.

-}
evaluateDerivative2V :: InterpolationMethod  -- ^ What method to use to interpolate
                        -> Vector Double     -- ^ Data points @xs@ sampling the domain of the function
                        -> Vector Double     -- ^ Data points @ys@ sampling the range of the function
                        -> Double            -- ^ Point @x@ at which to evaluate the second derivative
                        -> Double            -- ^ Interpolated result
evaluateDerivative2V =
  applyCFun "evaluateDerivative2V" "spline_eval_deriv2" c_spline_eval_deriv2

{- | Evaluate the second derivative of a function by interpolating
within the given dataset.  For example:

>>> let xs = [1..10]
>>> let ys map (**2) [1..10]
>>> evaluateDerivative2 Akima (zip xs ys) 2.2
2.0

To successfully @evaluateDerivative2 points x@, the domain (@x@)
values in @points@ must be monotonically increasing.  The evaluation
point @x@ must lie between the smallest and largest values in the
sampled domain.

-}
evaluateDerivative2 :: InterpolationMethod    -- ^ What method to use to interpolate
                       -> [(Double, Double)]  -- ^ (domain, range) points sampling the function
                       -> Double              -- ^ Point @x@ at which to evaluate the second derivative
                       -> Double              -- ^ Interpolated result
evaluateDerivative2 mth pts =
  applyCFun "evaluateDerivative2" "spline_eval_deriv2" c_spline_eval_deriv2
  mth (fromList xs) (fromList ys)
  where
    (xs, ys) = unzip pts

foreign import ccall safe "spline_eval_integ" c_spline_eval_integ
  :: Ptr Double -> Ptr Double -> CUInt -> Double -> Double -> CInt -> Ptr Double -> IO CInt

applyCIntFun hsname cname fun mth xs ys a b
  | dim xs /= dim ys = error $
                         "Error: Vectors of unequal sizes " ++
                         show (dim xs) ++ " and " ++ show (dim ys) ++
                         " supplied to " ++ hsname
  | otherwise = unsafePerformIO $
      flip appVector xs $ \xs' ->
       flip appVector ys $ \ys' ->
        alloca $ \y' -> do
          fun xs' ys'
            (fromIntegral $ dim xs) a b
            (methodToInt mth) y' // check cname
          peek y'

{- | Evaluate the definite integral of a function by interpolating
within the given dataset.  For example:

>>> let xs = vector [1..10]
>>> let ys = vector $ map (**2) [1..10]
>>> evaluateIntegralV CSpline xs ys 2.2 5.5
51.89853207547169

To successfully @evaluateIntegralV xs ys a b@, the vectors @xs@ and
@ys@ must have identical lengths, and @xs@ must be monotonically
increasing.  The integration bounds @a@ and @b@ must lie between the
smallest and largest values in @xs@.

-}
evaluateIntegralV :: InterpolationMethod  -- ^ What method to use to interpolate
                     -> Vector Double     -- ^ Data points @xs@ sampling the domain of the function
                     -> Vector Double     -- ^ Data points @ys@ sampling the range of the function
                     -> Double            -- ^ Lower integration bound @a@
                     -> Double            -- ^ Upper integration bound @b@
                     -> Double            -- ^ Resulting area
evaluateIntegralV =
  applyCIntFun "evaluateIntegralV" "spline_eval_integ" c_spline_eval_integ

{- | Evaluate the definite integral of a function by interpolating
within the given dataset.  For example:

>>> let xs = [1..10]
>>> let ys = map (**2) [1..10]
>>> evaluateIntegralV CSpline (zip xs ys) (2.2, 5.5)
51.909

To successfully @evaluateIntegral points (a, b)@, the domain (@x@)
values of @points@ must be monotonically increasing.  The integration
bounds @a@ and @b@ must lie between the smallest and largest values in
the sampled domain..
-}
evaluateIntegral :: InterpolationMethod    -- ^ What method to use to interpolate
                    -> [(Double, Double)]  -- ^ (domain, range) points sampling the function
                    -> (Double, Double)    -- ^ Integration bounds (@a@, @b@)
                    -> Double              -- ^ Resulting area
evaluateIntegral mth pts (a, b) =
  applyCIntFun "evaluateIntegral" "spline_eval_integ" c_spline_eval_integ
  mth (fromList xs) (fromList ys) a b
  where
    (xs, ys) = unzip pts
