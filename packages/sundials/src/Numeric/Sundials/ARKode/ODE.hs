{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Numeric.Sundials.ARKode.ODE
-- Copyright   :  Dominic Steinitz 2018,
--                Novadiscovery 2018
-- License     :  BSD
-- Maintainer  :  Dominic Steinitz
-- Stability   :  provisional
--
-- Solution of ordinary differential equation (ODE) initial value problems.
--
-- <https://computation.llnl.gov/projects/sundials/sundials-software>
--
-- A simple example:
--
-- <<diagrams/brusselator.png#diagram=brusselator&height=400&width=500>>
--
-- @
-- import           Numeric.Sundials.ARKode.ODE
-- import           Numeric.LinearAlgebra
--
-- import           Plots as P
-- import qualified Diagrams.Prelude as D
-- import           Diagrams.Backend.Rasterific
--
-- brusselator :: Double -> [Double] -> [Double]
-- brusselator _t x = [ a - (w + 1) * u + v * u * u
--                    , w * u - v * u * u
--                    , (b - w) / eps - w * u
--                    ]
--   where
--     a = 1.0
--     b = 3.5
--     eps = 5.0e-6
--     u = x !! 0
--     v = x !! 1
--     w = x !! 2
--
-- lSaxis :: [[Double]] -> P.Axis B D.V2 Double
-- lSaxis xs = P.r2Axis &~ do
--   let ts = xs!!0
--       us = xs!!1
--       vs = xs!!2
--       ws = xs!!3
--   P.linePlot' $ zip ts us
--   P.linePlot' $ zip ts vs
--   P.linePlot' $ zip ts ws
--
-- main = do
--   let res1 = odeSolve brusselator [1.2, 3.1, 3.0] (fromList [0.0, 0.1 .. 10.0])
--   renderRasterific "diagrams/brusselator.png"
--                    (D.dims2D 500.0 500.0)
--                    (renderAxis $ lSaxis $ [0.0, 0.1 .. 10.0]:(toLists $ tr res1))
-- @
--
-- KVAERNO_4_2_3
--
-- \[
-- \begin{array}{c|cccc}
-- 0.0 & 0.0 & 0.0 & 0.0 & 0.0 \\
-- 0.871733043 & 0.4358665215 & 0.4358665215 & 0.0 & 0.0 \\
-- 1.0 & 0.490563388419108 & 7.3570090080892e-2 & 0.4358665215 & 0.0 \\
-- 1.0 & 0.308809969973036 & 1.490563388254106 & -1.235239879727145 & 0.4358665215 \\
-- \hline
--  & 0.308809969973036 & 1.490563388254106 & -1.235239879727145 & 0.4358665215 \\
--  & 0.490563388419108 & 7.3570090080892e-2 & 0.4358665215 & 0.0 \\
-- \end{array}
-- \]
--
-- SDIRK_2_1_2
--
-- \[
-- \begin{array}{c|cc}
-- 1.0 & 1.0 & 0.0 \\
-- 0.0 & -1.0 & 1.0 \\
-- \hline
--  & 0.5 & 0.5 \\
--  & 1.0 & 0.0 \\
-- \end{array}
-- \]
--
-- SDIRK_5_3_4
--
-- \[
-- \begin{array}{c|ccccc}
-- 0.25 & 0.25 & 0.0 & 0.0 & 0.0 & 0.0 \\
-- 0.75 & 0.5 & 0.25 & 0.0 & 0.0 & 0.0 \\
-- 0.55 & 0.34 & -4.0e-2 & 0.25 & 0.0 & 0.0 \\
-- 0.5 & 0.2727941176470588 & -5.036764705882353e-2 & 2.7573529411764705e-2 & 0.25 & 0.0 \\
-- 1.0 & 1.0416666666666667 & -1.0208333333333333 & 7.8125 & -7.083333333333333 & 0.25 \\
-- \hline
--  & 1.0416666666666667 & -1.0208333333333333 & 7.8125 & -7.083333333333333 & 0.25 \\
--  & 1.2291666666666667 & -0.17708333333333334 & 7.03125 & -7.083333333333333 & 0.0 \\
-- \end{array}
-- \]
-----------------------------------------------------------------------------
module Numeric.Sundials.ARKode.ODE ( odeSolve
                                   , odeSolveV
                                   , odeSolveVWith
                                   , odeSolveVWith'
                                   , ButcherTable(..)
                                   , butcherTable
                                   , ODEMethod(..)
                                   , StepControl(..)
                                   , Jacobian
                                   ) where

import qualified Language.C.Inline as C
import qualified Language.C.Inline.Unsafe as CU

import           Data.Monoid ((<>))
import           Data.Maybe (isJust)

import           Foreign.C.Types (CDouble, CInt, CLong)
import           Foreign.Ptr (Ptr)

import qualified Data.Vector.Storable as V

import           Data.Coerce (coerce)
import           System.IO.Unsafe (unsafePerformIO)
import           GHC.Generics (C1, Constructor, (:+:)(..), D1, Rep, Generic, M1(..),
                               from, conName)

import           Numeric.LinearAlgebra.Devel (createVector)

import           Numeric.LinearAlgebra.HMatrix (Vector, Matrix, toList, (><),
                                                subMatrix, rows, cols, toLists,
                                                size, subVector)

import qualified Numeric.Sundials.CLangToHaskellTypes as T
import           Numeric.Sundials.Arkode
import qualified Numeric.Sundials.Arkode as B
import qualified Numeric.Sundials.ODEOpts as SO


C.context (C.baseCtx <> C.vecCtx <> C.funCtx <> T.sunCtx)

C.include "<stdlib.h>"
C.include "<stdio.h>"
C.include "<math.h>"
C.include "<arkode/arkode.h>"                 -- prototypes for ARKODE fcts., consts.
C.include "<nvector/nvector_serial.h>"        -- serial N_Vector types, fcts., macros
C.include "<sunmatrix/sunmatrix_dense.h>"     -- access to dense SUNMatrix
C.include "<sunlinsol/sunlinsol_dense.h>"     -- access to dense SUNLinearSolver
C.include "<arkode/arkode_direct.h>"          -- access to ARKDls interface
C.include "<sundials/sundials_types.h>"       -- definition of type realtype
C.include "<sundials/sundials_math.h>"
C.include "../../../helpers.h"
C.include "Numeric/Sundials/Arkode_hsc.h"


type Jacobian = Double -> Vector Double -> Matrix Double

-- | Stepping functions
data ODEMethod = SDIRK_2_1_2            Jacobian
               | SDIRK_2_1_2'
               | BILLINGTON_3_3_2       Jacobian
               | BILLINGTON_3_3_2'
               | TRBDF2_3_3_2           Jacobian
               | TRBDF2_3_3_2'
               | KVAERNO_4_2_3          Jacobian
               | KVAERNO_4_2_3'
               | ARK324L2SA_DIRK_4_2_3  Jacobian
               | ARK324L2SA_DIRK_4_2_3'
               | CASH_5_2_4             Jacobian
               | CASH_5_2_4'
               | CASH_5_3_4             Jacobian
               | CASH_5_3_4'
               | SDIRK_5_3_4            Jacobian
               | SDIRK_5_3_4'
               | KVAERNO_5_3_4          Jacobian
               | KVAERNO_5_3_4'
               | ARK436L2SA_DIRK_6_3_4  Jacobian
               | ARK436L2SA_DIRK_6_3_4'
               | KVAERNO_7_4_5          Jacobian
               | KVAERNO_7_4_5'
               | ARK548L2SA_DIRK_8_4_5  Jacobian
               | ARK548L2SA_DIRK_8_4_5'
               | HEUN_EULER_2_1_2         Jacobian
               | HEUN_EULER_2_1_2'
               | BOGACKI_SHAMPINE_4_2_3   Jacobian
               | BOGACKI_SHAMPINE_4_2_3'
               | ARK324L2SA_ERK_4_2_3     Jacobian
               | ARK324L2SA_ERK_4_2_3'
               | ZONNEVELD_5_3_4          Jacobian
               | ZONNEVELD_5_3_4'
               | ARK436L2SA_ERK_6_3_4     Jacobian
               | ARK436L2SA_ERK_6_3_4'
               | SAYFY_ABURUB_6_3_4       Jacobian
               | SAYFY_ABURUB_6_3_4'
               | CASH_KARP_6_4_5          Jacobian
               | CASH_KARP_6_4_5'
               | FEHLBERG_6_4_5         Jacobian
               | FEHLBERG_6_4_5'
               | DORMAND_PRINCE_7_4_5     Jacobian
               | DORMAND_PRINCE_7_4_5'
               | ARK548L2SA_ERK_8_4_5     Jacobian
               | ARK548L2SA_ERK_8_4_5'
               | VERNER_8_5_6            Jacobian
               | VERNER_8_5_6'
               | FEHLBERG_13_7_8         Jacobian
               | FEHLBERG_13_7_8'
  deriving Generic

constrName :: (HasConstructor (Rep a), Generic a)=> a -> String
constrName = genericConstrName . from

class HasConstructor (f :: * -> *) where
  genericConstrName :: f x -> String

instance HasConstructor f => HasConstructor (D1 c f) where
  genericConstrName (M1 x) = genericConstrName x

instance (HasConstructor x, HasConstructor y) => HasConstructor (x :+: y) where
  genericConstrName (L1 l) = genericConstrName l
  genericConstrName (R1 r) = genericConstrName r

instance Constructor c => HasConstructor (C1 c f) where
  genericConstrName x = conName x

instance Show ODEMethod where
  show x = constrName x

-- FIXME: We can probably do better here with generics
getMethod :: ODEMethod -> Int
getMethod (SDIRK_2_1_2 _)            = sDIRK_2_1_2
getMethod (SDIRK_2_1_2')             = sDIRK_2_1_2
getMethod (BILLINGTON_3_3_2 _)       = bILLINGTON_3_3_2
getMethod (BILLINGTON_3_3_2')        = bILLINGTON_3_3_2
getMethod (TRBDF2_3_3_2 _)           = tRBDF2_3_3_2
getMethod (TRBDF2_3_3_2')            = tRBDF2_3_3_2
getMethod (KVAERNO_4_2_3  _)         = kVAERNO_4_2_3
getMethod (KVAERNO_4_2_3')           = kVAERNO_4_2_3
getMethod (ARK324L2SA_DIRK_4_2_3 _)  = aRK324L2SA_DIRK_4_2_3
getMethod (ARK324L2SA_DIRK_4_2_3')   = aRK324L2SA_DIRK_4_2_3
getMethod (CASH_5_2_4 _)             = cASH_5_2_4
getMethod (CASH_5_2_4')              = cASH_5_2_4
getMethod (CASH_5_3_4 _)             = cASH_5_3_4
getMethod (CASH_5_3_4')              = cASH_5_3_4
getMethod (SDIRK_5_3_4 _)            = sDIRK_5_3_4
getMethod (SDIRK_5_3_4')             = sDIRK_5_3_4
getMethod (KVAERNO_5_3_4 _)          = kVAERNO_5_3_4
getMethod (KVAERNO_5_3_4')           = kVAERNO_5_3_4
getMethod (ARK436L2SA_DIRK_6_3_4 _)  = aRK436L2SA_DIRK_6_3_4
getMethod (ARK436L2SA_DIRK_6_3_4')   = aRK436L2SA_DIRK_6_3_4
getMethod (KVAERNO_7_4_5 _)          = kVAERNO_7_4_5
getMethod (KVAERNO_7_4_5')           = kVAERNO_7_4_5
getMethod (ARK548L2SA_DIRK_8_4_5 _)  = aRK548L2SA_DIRK_8_4_5
getMethod (ARK548L2SA_DIRK_8_4_5')   = aRK548L2SA_DIRK_8_4_5
getMethod (HEUN_EULER_2_1_2 _)       = hEUN_EULER_2_1_2
getMethod (HEUN_EULER_2_1_2')        = hEUN_EULER_2_1_2
getMethod (BOGACKI_SHAMPINE_4_2_3 _) = bOGACKI_SHAMPINE_4_2_3
getMethod (BOGACKI_SHAMPINE_4_2_3')  = bOGACKI_SHAMPINE_4_2_3
getMethod (ARK324L2SA_ERK_4_2_3 _)   = aRK324L2SA_ERK_4_2_3
getMethod (ARK324L2SA_ERK_4_2_3')    = aRK324L2SA_ERK_4_2_3
getMethod (ZONNEVELD_5_3_4 _)        = zONNEVELD_5_3_4
getMethod (ZONNEVELD_5_3_4')         = zONNEVELD_5_3_4
getMethod (ARK436L2SA_ERK_6_3_4 _)   = aRK436L2SA_ERK_6_3_4
getMethod (ARK436L2SA_ERK_6_3_4')    = aRK436L2SA_ERK_6_3_4
getMethod (SAYFY_ABURUB_6_3_4 _)     = sAYFY_ABURUB_6_3_4
getMethod (SAYFY_ABURUB_6_3_4')      = sAYFY_ABURUB_6_3_4
getMethod (CASH_KARP_6_4_5 _)        = cASH_KARP_6_4_5
getMethod (CASH_KARP_6_4_5')         = cASH_KARP_6_4_5
getMethod (FEHLBERG_6_4_5 _)         = fEHLBERG_6_4_5
getMethod (FEHLBERG_6_4_5' )         = fEHLBERG_6_4_5
getMethod (DORMAND_PRINCE_7_4_5 _)   = dORMAND_PRINCE_7_4_5
getMethod (DORMAND_PRINCE_7_4_5')    = dORMAND_PRINCE_7_4_5
getMethod (ARK548L2SA_ERK_8_4_5 _)   = aRK548L2SA_ERK_8_4_5
getMethod (ARK548L2SA_ERK_8_4_5')    = aRK548L2SA_ERK_8_4_5
getMethod (VERNER_8_5_6 _)           = vERNER_8_5_6
getMethod (VERNER_8_5_6')            = vERNER_8_5_6
getMethod (FEHLBERG_13_7_8 _)        = fEHLBERG_13_7_8
getMethod (FEHLBERG_13_7_8')         = fEHLBERG_13_7_8

getJacobian :: ODEMethod -> Maybe Jacobian
getJacobian (SDIRK_2_1_2 j)            = Just j
getJacobian (BILLINGTON_3_3_2 j)       = Just j
getJacobian (TRBDF2_3_3_2 j)           = Just j
getJacobian (KVAERNO_4_2_3  j)         = Just j
getJacobian (ARK324L2SA_DIRK_4_2_3 j)  = Just j
getJacobian (CASH_5_2_4 j)             = Just j
getJacobian (CASH_5_3_4 j)             = Just j
getJacobian (SDIRK_5_3_4 j)            = Just j
getJacobian (KVAERNO_5_3_4 j)          = Just j
getJacobian (ARK436L2SA_DIRK_6_3_4 j)  = Just j
getJacobian (KVAERNO_7_4_5 j)          = Just j
getJacobian (ARK548L2SA_DIRK_8_4_5 j)  = Just j
getJacobian (HEUN_EULER_2_1_2 j)       = Just j
getJacobian (BOGACKI_SHAMPINE_4_2_3 j) = Just j
getJacobian (ARK324L2SA_ERK_4_2_3 j)   = Just j
getJacobian (ZONNEVELD_5_3_4 j)        = Just j
getJacobian (ARK436L2SA_ERK_6_3_4 j)   = Just j
getJacobian (SAYFY_ABURUB_6_3_4 j)     = Just j
getJacobian (CASH_KARP_6_4_5 j)        = Just j
getJacobian (FEHLBERG_6_4_5 j)         = Just j
getJacobian (DORMAND_PRINCE_7_4_5 j)   = Just j
getJacobian (ARK548L2SA_ERK_8_4_5 j)   = Just j
getJacobian (VERNER_8_5_6 j)           = Just j
getJacobian (FEHLBERG_13_7_8 j)        = Just j
getJacobian _                          = Nothing

-- | A version of 'odeSolveVWith' with reasonable default step control.
odeSolveV
    :: ODEMethod
    -> Maybe Double      -- ^ initial step size - by default, ARKode
                         -- estimates the initial step size to be the
                         -- solution \(h\) of the equation
                         -- \(\|\frac{h^2\ddot{y}}{2}\| = 1\), where
                         -- \(\ddot{y}\) is an estimated value of the
                         -- second derivative of the solution at \(t_0\)
    -> Double            -- ^ absolute tolerance for the state vector
    -> Double            -- ^ relative tolerance for the state vector
    -> (Double -> Vector Double -> Vector Double) -- ^ The RHS of the system \(\dot{y} = f(t,y)\)
    -> Vector Double     -- ^ initial conditions
    -> Vector Double     -- ^ desired solution times
    -> Matrix Double     -- ^ solution
odeSolveV meth hi epsAbs epsRel f y0 ts =
  case odeSolveVWith' meth (X epsAbs epsRel) hi g y0 ts of
    Left c        -> error $ show c -- FIXME
    -- FIXME: Can we do better than using lists?
    Right (v, _d) -> (nR >< nC) (V.toList v)
  where
    us = toList ts
    nR = length us
    nC = size y0
    g t x0 = coerce $ f t x0

-- | A version of 'odeSolveV' with reasonable default parameters and
-- system of equations defined using lists. FIXME: we should say
-- something about the fact we could use the Jacobian but don't for
-- compatibility with hmatrix-gsl.
odeSolve :: (Double -> [Double] -> [Double]) -- ^ The RHS of the system \(\dot{y} = f(t,y)\)
         -> [Double]                         -- ^ initial conditions
         -> Vector Double                    -- ^ desired solution times
         -> Matrix Double                    -- ^ solution
odeSolve f y0 ts =
  -- FIXME: These tolerances are different from the ones in GSL
  case odeSolveVWith' SDIRK_5_3_4' (XX' 1.0e-6 1.0e-10 1 1)  Nothing g (V.fromList y0) (V.fromList $ toList ts) of
    Left c        -> error $ show c -- FIXME
    Right (v, _d) -> (nR >< nC) (V.toList v)
  where
    us = toList ts
    nR = length us
    nC = length y0
    g t x0 = V.fromList $ f t (V.toList x0)

odeSolveVWith ::
  ODEMethod
  -> StepControl
  -> Maybe Double -- ^ initial step size - by default, ARKode
                  -- estimates the initial step size to be the
                  -- solution \(h\) of the equation
                  -- \(\|\frac{h^2\ddot{y}}{2}\| = 1\), where
                  -- \(\ddot{y}\) is an estimated value of the second
                  -- derivative of the solution at \(t_0\)
  -> (Double -> V.Vector Double -> V.Vector Double) -- ^ The RHS of the system \(\dot{y} = f(t,y)\)
  -> V.Vector Double                     -- ^ Initial conditions
  -> V.Vector Double                     -- ^ Desired solution times
  -> Matrix Double                       -- ^ Error code or solution
odeSolveVWith method control initStepSize f y0 tt =
  case odeSolveVWith' method control initStepSize f y0 tt of
    Left c        -> error $ show c -- FIXME
    Right (v, _d) -> (nR >< nC) (V.toList v)
  where
    nR = V.length tt
    nC = V.length y0

odeSolveVWith' ::
  ODEMethod
  -> StepControl
  -> Maybe Double -- ^ initial step size - by default, ARKode
                  -- estimates the initial step size to be the
                  -- solution \(h\) of the equation
                  -- \(\|\frac{h^2\ddot{y}}{2}\| = 1\), where
                  -- \(\ddot{y}\) is an estimated value of the second
                  -- derivative of the solution at \(t_0\)
  -> (Double -> V.Vector Double -> V.Vector Double) -- ^ The RHS of the system \(\dot{y} = f(t,y)\)
  -> V.Vector Double                     -- ^ Initial conditions
  -> V.Vector Double                     -- ^ Desired solution times
  -> Either Int ((V.Vector Double), SO.SundialsDiagnostics) -- ^ Error code or solution
odeSolveVWith' method control initStepSize f y0 tt =
  case solveOdeC (fromIntegral $ getMethod method) (coerce initStepSize) jacH (scise control)
                 (coerce f) (coerce y0) (coerce tt) of
    Left c -> Left $ fromIntegral c
    Right (v, d) -> Right (coerce v, d)
  where
    l = size y0
    scise (X absTol relTol)                          = coerce (V.replicate l absTol, relTol)
    scise (X' absTol relTol)                         = coerce (V.replicate l absTol, relTol)
    scise (XX' absTol relTol yScale _yDotScale)      = coerce (V.replicate l absTol, yScale * relTol)
    -- FIXME; Should we check that the length of ss is correct?
    scise (ScXX' absTol relTol yScale _yDotScale ss) = coerce (V.map (* absTol) ss, yScale * relTol)
    jacH = fmap (\g t v -> matrixToSunMatrix $ g (coerce t) (coerce v)) $
           getJacobian method
    matrixToSunMatrix m = T.SunMatrix { T.rows = nr, T.cols = nc, T.vals = vs }
      where
        nr = fromIntegral $ rows m
        nc = fromIntegral $ cols m
        -- FIXME: efficiency
        vs = V.fromList $ map coerce $ concat $ toLists m

solveOdeC ::
  CInt ->
  Maybe CDouble ->
  (Maybe (CDouble -> V.Vector CDouble -> T.SunMatrix)) ->
  (V.Vector CDouble, CDouble) ->
  (CDouble -> V.Vector CDouble -> V.Vector CDouble) -- ^ The RHS of the system \(\dot{y} = f(t,y)\)
  -> V.Vector CDouble -- ^ Initial conditions
  -> V.Vector CDouble -- ^ Desired solution times
  -> Either CInt ((V.Vector CDouble), SO.SundialsDiagnostics) -- ^ Error code or solution
solveOdeC method initStepSize jacH (absTols, relTol) fun f0 ts = unsafePerformIO $ do

  let isInitStepSize :: CInt
      isInitStepSize = fromIntegral $ fromEnum $ isJust initStepSize
      ss :: CDouble
      ss = case initStepSize of
             -- It would be better to put an error message here but
             -- inline-c seems to evaluate this even if it is never
             -- used :(
             Nothing -> 0.0
             Just x  -> x
  let dim = V.length f0
      nEq :: CLong
      nEq = fromIntegral dim
      nTs :: CInt
      nTs = fromIntegral $ V.length ts
  -- FIXME: fMut is not actually mutatated
  fMut <- V.thaw f0
  tMut <- V.thaw ts
  -- FIXME: I believe this gets taken from the ghc heap and so should
  -- be subject to garbage collection.
  quasiMatrixRes <- createVector ((fromIntegral dim) * (fromIntegral nTs))
  qMatMut <- V.thaw quasiMatrixRes
  diagnostics :: V.Vector CLong <- createVector 10 -- FIXME
  diagMut <- V.thaw diagnostics
  -- We need the types that sundials expects. These are tied together
  -- in 'CLangToHaskellTypes'. FIXME: The Haskell type is currently empty!
  let funIO :: CDouble -> Ptr T.SunVector -> Ptr T.SunVector -> Ptr () -> IO CInt
      funIO x y f _ptr = do
        -- Convert the pointer we get from C (y) to a vector, and then
        -- apply the user-supplied function.
        fImm <- fun x <$> SO.getDataFromContents dim y
        -- Fill in the provided pointer with the resulting vector.
        SO.putDataInContents fImm dim f
        -- FIXME: I don't understand what this comment means
        -- Unsafe since the function will be called many times.
        [CU.exp| int{ 0 } |]
  let isJac :: CInt
      isJac = fromIntegral $ fromEnum $ isJust jacH
      jacIO :: CDouble -> Ptr T.SunVector -> Ptr T.SunVector -> Ptr T.SunMatrix ->
               Ptr () -> Ptr T.SunVector -> Ptr T.SunVector -> Ptr T.SunVector ->
               IO CInt
      jacIO t y _fy jacS _ptr _tmp1 _tmp2 _tmp3 = do
        case jacH of
          Nothing   -> error "Numeric.Sundials.ARKode.ODE: Jacobian not defined"
          Just jacI -> do j <- jacI t <$> SO.getDataFromContents dim y
                          SO.putMatrixDataFromContents j jacS
                          -- FIXME: I don't understand what this comment means
                          -- Unsafe since the function will be called many times.
                          [CU.exp| int{ 0 } |]

  res <- [C.block| int {
                         /* general problem variables */

                         int flag;                  /* reusable error-checking flag                 */
                         int i, j;                  /* reusable loop indices                        */
                         N_Vector y = NULL;         /* empty vector for storing solution            */
                         N_Vector tv = NULL;        /* empty vector for storing absolute tolerances */
                         SUNMatrix A = NULL;        /* empty matrix for linear solver               */
                         SUNLinearSolver LS = NULL; /* empty linear solver object                   */
                         void *arkode_mem = NULL;   /* empty ARKode memory structure                */
                         realtype t;
                         long int nst, nst_a, nfe, nfi, nsetups, nje, nfeLS, nni, ncfn, netf;

                         /* general problem parameters */

                         realtype T0 = RCONST(($vec-ptr:(double *tMut))[0]); /* initial time              */
                         sunindextype NEQ = $(sunindextype nEq);             /* number of dependent vars. */

                         /* Initialize data structures */

                         y = N_VNew_Serial(NEQ); /* Create serial vector for solution */
                         if (check_flag((void *)y, "N_VNew_Serial", 0)) return 1;
                         /* Specify initial condition */
                         for (i = 0; i < NEQ; i++) {
                           NV_Ith_S(y,i) = ($vec-ptr:(double *fMut))[i];
                         };

                         tv = N_VNew_Serial(NEQ); /* Create serial vector for absolute tolerances */
                         if (check_flag((void *)tv, "N_VNew_Serial", 0)) return 1;
                         /* Specify tolerances */
                         for (i = 0; i < NEQ; i++) {
                           NV_Ith_S(tv,i) = ($vec-ptr:(double *absTols))[i];
                         };

                         arkode_mem = ARKodeCreate(); /* Create the solver memory */
                         if (check_flag((void *)arkode_mem, "ARKodeCreate", 0)) return 1;

                         /* Call ARKodeInit to initialize the integrator memory and specify the */
                         /* right-hand side function in y'=f(t,y), the inital time T0, and      */
                         /* the initial dependent variable vector y.  Note: we treat the        */
                         /* problem as fully implicit and set f_E to NULL and f_I to f.         */

                         /* Here we use the C types defined in helpers.h which tie up with */
                         /* the Haskell types defined in CLangToHaskellTypes                             */
                         if ($(int method) < MIN_DIRK_NUM) {
                           flag = ARKodeInit(arkode_mem, $fun:(int (* funIO) (double t, SunVector y[], SunVector dydt[], void * params)), NULL, T0, y);
                           if (check_flag(&flag, "ARKodeInit", 1)) return 1;
                         } else {
                           flag = ARKodeInit(arkode_mem, NULL, $fun:(int (* funIO) (double t, SunVector y[], SunVector dydt[], void * params)), T0, y);
                           if (check_flag(&flag, "ARKodeInit", 1)) return 1;
                         }

                         /* FIXME: A hack for initial testing */
                         flag = ARKodeSetMinStep(arkode_mem, 1.0e-12);
                         if (check_flag(&flag, "ARKodeSetMinStep", 1)) return 1;
                         flag = ARKodeSetMaxNumSteps(arkode_mem, 10000);
                         if (check_flag(&flag, "ARKodeSetMaxNumSteps", 1)) return 1;

                         /* Set routines */
                         flag = ARKodeSVtolerances(arkode_mem, $(double relTol), tv);
                         if (check_flag(&flag, "ARKodeSVtolerances", 1)) return 1;

                         /* Initialize dense matrix data structure and solver */
                         A = SUNDenseMatrix(NEQ, NEQ);
                         if (check_flag((void *)A, "SUNDenseMatrix", 0)) return 1;
                         LS = SUNDenseLinearSolver(y, A);
                         if (check_flag((void *)LS, "SUNDenseLinearSolver", 0)) return 1;

                         /* Attach matrix and linear solver */
                         flag = ARKDlsSetLinearSolver(arkode_mem, LS, A);
                         if (check_flag(&flag, "ARKDlsSetLinearSolver", 1)) return 1;

                         /* Set the initial step size if there is one */
                         if ($(int isInitStepSize)) {
                           /* FIXME: We could check if the initial step size is 0 */
                           /* or even NaN and then throw an error                 */
                           flag = ARKodeSetInitStep(arkode_mem, $(double ss));
                           if (check_flag(&flag, "ARKodeSetInitStep", 1)) return 1;
                         }

                         /* Set the Jacobian if there is one */
                         if ($(int isJac)) {
                           flag = ARKDlsSetJacFn(arkode_mem, $fun:(int (* jacIO) (double t, SunVector y[], SunVector fy[], SunMatrix Jac[], void * params, SunVector tmp1[], SunVector tmp2[], SunVector tmp3[])));
                           if (check_flag(&flag, "ARKDlsSetJacFn", 1)) return 1;
                         }

                         /* Store initial conditions */
                         for (j = 0; j < NEQ; j++) {
                           ($vec-ptr:(double *qMatMut))[0 * $(int nTs) + j] = NV_Ith_S(y,j);
                         }

                         /* Explicitly set the method */
                         if ($(int method) >= MIN_DIRK_NUM) {
                           flag = ARKodeSetIRKTableNum(arkode_mem, $(int method));
                           if (check_flag(&flag, "ARKodeSetIRKTableNum", 1)) return 1;
                         } else {
                           flag = ARKodeSetERKTableNum(arkode_mem, $(int method));
                           if (check_flag(&flag, "ARKodeSetERKTableNum", 1)) return 1;
                         }

                         /* Main time-stepping loop: calls ARKode to perform the integration */
                         /* Stops when the final time has been reached                       */
                         for (i = 1; i < $(int nTs); i++) {

                           flag = ARKode(arkode_mem, ($vec-ptr:(double *tMut))[i], y, &t, ARK_NORMAL); /* call integrator */
                           if (check_flag(&flag, "ARKode", 1)) break;

                           /* Store the results for Haskell */
                           for (j = 0; j < NEQ; j++) {
                             ($vec-ptr:(double *qMatMut))[i * NEQ + j] = NV_Ith_S(y,j);
                           }

                           /* unsuccessful solve: break */
                           if (flag < 0) {
                             fprintf(stderr,"Solver failure, stopping integration\n");
                             break;
                           }
                         }

                         /* Get some final statistics on how the solve progressed */

                         flag = ARKodeGetNumSteps(arkode_mem, &nst);
                         check_flag(&flag, "ARKodeGetNumSteps", 1);
                         ($vec-ptr:(long int *diagMut))[0] = nst;

                         flag = ARKodeGetNumStepAttempts(arkode_mem, &nst_a);
                         check_flag(&flag, "ARKodeGetNumStepAttempts", 1);
                         ($vec-ptr:(long int *diagMut))[1] = nst_a;

                         flag = ARKodeGetNumRhsEvals(arkode_mem, &nfe, &nfi);
                         check_flag(&flag, "ARKodeGetNumRhsEvals", 1);
                         ($vec-ptr:(long int *diagMut))[2] = nfe;
                         ($vec-ptr:(long int *diagMut))[3] = nfi;

                         flag = ARKodeGetNumLinSolvSetups(arkode_mem, &nsetups);
                         check_flag(&flag, "ARKodeGetNumLinSolvSetups", 1);
                         ($vec-ptr:(long int *diagMut))[4] = nsetups;

                         flag = ARKodeGetNumErrTestFails(arkode_mem, &netf);
                         check_flag(&flag, "ARKodeGetNumErrTestFails", 1);
                         ($vec-ptr:(long int *diagMut))[5] = netf;

                         flag = ARKodeGetNumNonlinSolvIters(arkode_mem, &nni);
                         check_flag(&flag, "ARKodeGetNumNonlinSolvIters", 1);
                         ($vec-ptr:(long int *diagMut))[6] = nni;

                         flag = ARKodeGetNumNonlinSolvConvFails(arkode_mem, &ncfn);
                         check_flag(&flag, "ARKodeGetNumNonlinSolvConvFails", 1);
                         ($vec-ptr:(long int *diagMut))[7] = ncfn;

                         flag = ARKDlsGetNumJacEvals(arkode_mem, &nje);
                         check_flag(&flag, "ARKDlsGetNumJacEvals", 1);
                         ($vec-ptr:(long int *diagMut))[8] = ncfn;

                         flag = ARKDlsGetNumRhsEvals(arkode_mem, &nfeLS);
                         check_flag(&flag, "ARKDlsGetNumRhsEvals", 1);
                         ($vec-ptr:(long int *diagMut))[9] = ncfn;

                         /* Clean up and return */
                         N_VDestroy(y);            /* Free y vector          */
                         N_VDestroy(tv);           /* Free tv vector         */
                         ARKodeFree(&arkode_mem);  /* Free integrator memory */
                         SUNLinSolFree(LS);        /* Free linear solver     */
                         SUNMatDestroy(A);         /* Free A matrix          */

                         return flag;
                       } |]
  if res == 0
    then do
      preD <- V.freeze diagMut
      let d = SO.SundialsDiagnostics (fromIntegral $ preD V.!0)
                                     (fromIntegral $ preD V.!1)
                                     (fromIntegral $ preD V.!2)
                                     (fromIntegral $ preD V.!3)
                                     (fromIntegral $ preD V.!4)
                                     (fromIntegral $ preD V.!5)
                                     (fromIntegral $ preD V.!6)
                                     (fromIntegral $ preD V.!7)
                                     (fromIntegral $ preD V.!8)
                                     (fromIntegral $ preD V.!9)
      m <- V.freeze qMatMut
      return $ Right (m, d)
    else do
      return $ Left res

data ButcherTable = ButcherTable { am  :: Matrix Double
                                 , cv  :: Vector Double
                                 , bv  :: Vector Double
                                 , b2v :: Vector Double
                                 }
  deriving Show

data ButcherTable' a = ButcherTable' { am'  :: V.Vector a
                                     , cv'  :: V.Vector a
                                     , bv'  :: V.Vector a
                                     , b2v' :: V.Vector a
                                     }
  deriving Show

butcherTable :: ODEMethod -> ButcherTable
butcherTable method =
  case getBT method of
    Left c -> error $ show c -- FIXME
    Right (ButcherTable' v w x y, sqp) ->
      ButcherTable { am = subMatrix (0, 0) (s, s) $ (B.arkSMax >< B.arkSMax) (V.toList v)
                   , cv = subVector 0 s w
                   , bv = subVector 0 s x
                   , b2v = subVector 0 s y
                   }
      where
        s = fromIntegral $ sqp V.! 0

getBT :: ODEMethod -> Either Int (ButcherTable' Double, V.Vector Int)
getBT method = case getButcherTable method of
                 Left c ->
                   Left $ fromIntegral c
                 Right (ButcherTable' a b c d, sqp) ->
                   Right $ ( ButcherTable' (coerce a) (coerce b) (coerce c) (coerce d)
                           , V.map fromIntegral sqp )

getButcherTable :: ODEMethod
                -> Either CInt (ButcherTable' CDouble, V.Vector CInt)
getButcherTable method = unsafePerformIO $ do
  -- ARKode seems to want an ODE in order to set and then get the
  -- Butcher tableau so here's one to keep it happy
  let funI :: CDouble -> V.Vector CDouble -> V.Vector CDouble
      funI _t ys = V.fromList [ ys V.! 0 ]
  let funE :: CDouble -> V.Vector CDouble -> V.Vector CDouble
      funE _t ys = V.fromList [ ys V.! 0 ]
      f0        = V.fromList [ 1.0 ]
      ts        = V.fromList [ 0.0 ]
      dim = V.length f0
      nEq :: CLong
      nEq = fromIntegral dim
      mN :: CInt
      mN = fromIntegral $ getMethod method

  btSQP :: V.Vector CInt <- createVector 3
  btSQPMut <- V.thaw btSQP
  btAs :: V.Vector CDouble <- createVector (B.arkSMax * B.arkSMax)
  btAsMut <- V.thaw btAs
  btCs  :: V.Vector CDouble <- createVector B.arkSMax
  btBs  :: V.Vector CDouble <- createVector B.arkSMax
  btB2s :: V.Vector CDouble <- createVector B.arkSMax
  btCsMut  <- V.thaw btCs
  btBsMut  <- V.thaw btBs
  btB2sMut <- V.thaw btB2s
  let funIOI :: CDouble -> Ptr T.SunVector -> Ptr T.SunVector -> Ptr () -> IO CInt
      funIOI x y f _ptr = do
        fImm <- funI x <$> SO.getDataFromContents dim y
        SO.putDataInContents fImm dim f
        -- FIXME: I don't understand what this comment means
        -- Unsafe since the function will be called many times.
        [CU.exp| int{ 0 } |]
  let funIOE :: CDouble -> Ptr T.SunVector -> Ptr T.SunVector -> Ptr () -> IO CInt
      funIOE x y f _ptr = do
        fImm <- funE x <$> SO.getDataFromContents dim y
        SO.putDataInContents fImm dim f
        -- FIXME: I don't understand what this comment means
        -- Unsafe since the function will be called many times.
        [CU.exp| int{ 0 } |]
  res <- [C.block| int {
                         /* general problem variables */

                         int flag;                /* reusable error-checking flag      */
                         N_Vector y = NULL;       /* empty vector for storing solution */
                         void *arkode_mem = NULL; /* empty ARKode memory structure     */
                         int i, j;                /* reusable loop indices             */

                         /* general problem parameters */

                         realtype T0 = RCONST(($vec-ptr:(double *ts))[0]); /* initial time             */
                         sunindextype NEQ = $(sunindextype nEq);           /* number of dependent vars */

                         /* Initialize data structures */

                         y = N_VNew_Serial(NEQ); /* Create serial vector for solution */
                         if (check_flag((void *)y, "N_VNew_Serial", 0)) return 1;
                         /* Specify initial condition */
                         for (i = 0; i < NEQ; i++) {
                           NV_Ith_S(y,i) = ($vec-ptr:(double *f0))[i];
                         };
                         arkode_mem = ARKodeCreate(); /* Create the solver memory */
                         if (check_flag((void *)arkode_mem, "ARKodeCreate", 0)) return 1;

                         flag = ARKodeInit(arkode_mem, $fun:(int (* funIOE) (double t, SunVector y[], SunVector dydt[], void * params)), $fun:(int (* funIOI) (double t, SunVector y[], SunVector dydt[], void * params)), T0, y);
                         if (check_flag(&flag, "ARKodeInit", 1)) return 1;

                         if ($(int mN) >= MIN_DIRK_NUM) {
                         flag = ARKodeSetIRKTableNum(arkode_mem, $(int mN));
                         if (check_flag(&flag, "ARKodeSetIRKTableNum", 1)) return 1;
                         } else {
                         flag = ARKodeSetERKTableNum(arkode_mem, $(int mN));
                         if (check_flag(&flag, "ARKodeSetERKTableNum", 1)) return 1;
                         }

                         int s, q, p;
                         realtype *ai = (realtype *)malloc(ARK_S_MAX * ARK_S_MAX * sizeof(realtype));
                         realtype *ae = (realtype *)malloc(ARK_S_MAX * ARK_S_MAX * sizeof(realtype));
                         realtype *ci = (realtype *)malloc(ARK_S_MAX * sizeof(realtype));
                         realtype *ce = (realtype *)malloc(ARK_S_MAX * sizeof(realtype));
                         realtype *bi = (realtype *)malloc(ARK_S_MAX * sizeof(realtype));
                         realtype *be = (realtype *)malloc(ARK_S_MAX * sizeof(realtype));
                         realtype *b2i = (realtype *)malloc(ARK_S_MAX * sizeof(realtype));
                         realtype *b2e = (realtype *)malloc(ARK_S_MAX * sizeof(realtype));
                         flag = ARKodeGetCurrentButcherTables(arkode_mem, &s, &q, &p, ai, ae, ci, ce, bi, be, b2i, b2e);
                         if (check_flag(&flag, "ARKode", 1)) return 1;
                         $vec-ptr:(int *btSQPMut)[0] = s;
                         $vec-ptr:(int *btSQPMut)[1] = q;
                         $vec-ptr:(int *btSQPMut)[2] = p;
                         for (i = 0; i < s; i++) {
                           for (j = 0; j < s; j++) {
                             /* FIXME: double should be realtype */
                             ($vec-ptr:(double *btAsMut))[i * ARK_S_MAX + j] = ai[i * ARK_S_MAX + j];
                           }
                         }

                         for (i = 0; i < s; i++) {
                           ($vec-ptr:(double *btCsMut))[i]  = ci[i];
                           ($vec-ptr:(double *btBsMut))[i]  = bi[i];
                           ($vec-ptr:(double *btB2sMut))[i] = b2i[i];
                         }

                         /* Clean up and return */
                         N_VDestroy(y);            /* Free y vector */
                         ARKodeFree(&arkode_mem);  /* Free integrator memory */

                         return flag;
                       } |]
  if res == 0
    then do
      x <- V.freeze btAsMut
      y <- V.freeze btSQPMut
      z <- V.freeze btCsMut
      u <- V.freeze btBsMut
      v <- V.freeze btB2sMut
      return $ Right (ButcherTable' { am' = x, cv' = z, bv' = u, b2v' = v }, y)
    else do
      return $ Left res

-- | Adaptive step-size control
-- functions.
--
-- [GSL](https://www.gnu.org/software/gsl/doc/html/ode-initval.html#adaptive-step-size-control)
-- allows the user to control the step size adjustment using
-- \(D_i = \epsilon^{abs}s_i + \epsilon^{rel}(a_{y} |y_i| + a_{dy/dt} h |\dot{y}_i|)\) where
-- \(\epsilon^{abs}\) is the required absolute error, \(\epsilon^{rel}\)
-- is the required relative error, \(s_i\) is a vector of scaling
-- factors, \(a_{y}\) is a scaling factor for the solution \(y\) and
-- \(a_{dydt}\) is a scaling factor for the derivative of the solution \(dy/dt\).
--
-- [ARKode](https://computation.llnl.gov/projects/sundials/arkode)
-- allows the user to control the step size adjustment using
-- \(\eta^{rel}|y_i| + \eta^{abs}_i\). For compatibility with
-- [hmatrix-gsl](https://hackage.haskell.org/package/hmatrix-gsl),
-- tolerances for \(y\) and \(\dot{y}\) can be specified but the latter have no
-- effect.
data StepControl = X     Double Double -- ^ absolute and relative tolerance for \(y\); in GSL terms, \(a_{y} = 1\) and \(a_{dy/dt} = 0\); in ARKode terms, the \(\eta^{abs}_i\) are identical
                 | X'    Double Double -- ^ absolute and relative tolerance for \(\dot{y}\); in GSL terms, \(a_{y} = 0\) and \(a_{dy/dt} = 1\); in ARKode terms, the latter is treated as the relative tolerance for \(y\) so this is the same as specifying 'X' which may be entirely incorrect for the given problem
                 | XX'   Double Double Double Double -- ^ include both via relative tolerance
                                                     -- scaling factors \(a_y\), \(a_{{dy}/{dt}}\); in ARKode terms, the latter is ignored and \(\eta^{rel} = a_{y}\epsilon^{rel}\)
                 | ScXX' Double Double Double Double (Vector Double) -- ^ scale absolute tolerance of \(y_i\); in ARKode terms, \(a_{{dy}/{dt}}\) is ignored, \(\eta^{abs}_i = s_i \epsilon^{abs}\) and \(\eta^{rel} = a_{y}\epsilon^{rel}\)
