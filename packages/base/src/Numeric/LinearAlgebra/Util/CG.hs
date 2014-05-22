{-# LANGUAGE FlexibleContexts, FlexibleInstances #-}
{-# LANGUAGE RecordWildCards #-}

module Numeric.LinearAlgebra.Util.CG(
    cgSolve,
    CGMat
) where

import Numeric.Container
import Numeric.Vector()

{-
import Util.Misc(debug, debugMat)

(//) :: Show a => a -> String -> a
infix 0 // -- , ///
a // b = debug b id a

(///) :: DV -> String -> DV
infix 0 ///
v /// b = debugMat b 2 asRow v
-}


type DV = Vector Double

data CGState = CGState
    { cgp  :: DV
    , cgr  :: DV
    , cgr2 :: Double
    , cgx  :: DV
    , cgdx :: Double
    }

cg :: Bool -> (DV -> DV) -> (DV -> DV) -> CGState -> CGState
cg sym at a (CGState p r r2 x _) = CGState p' r' r'2 x' rdx
  where
    ap1 = a p
    ap  | sym       = ap1
        | otherwise = at ap1
    pap | sym       = p ◇ ap1
        | otherwise = norm2 ap1 ** 2
    alpha = r2 / pap
    dx = scale alpha p
    x' = x + dx
    r' = r - scale alpha ap
    r'2 = r' ◇ r'
    beta = r'2 / r2
    p' = r' + scale beta p

    rdx = norm2 dx / max 1 (norm2 x)

conjugrad
  :: (Transposable m, Contraction m DV DV)
  => Bool -> m -> DV -> DV -> Double -> Double -> [CGState]
conjugrad sym a b = solveG (tr a ◇) (a ◇) (cg sym) b

solveG
    :: (DV -> DV) -> (DV -> DV)
    -> ((DV -> DV) -> (DV -> DV) -> CGState -> CGState)
    -> DV
    -> DV
    -> Double -> Double
    -> [CGState]
solveG mat ma meth rawb x0' ϵb ϵx
    = takeUntil ok . iterate (meth mat ma) $ CGState p0 r0 r20 x0 1
  where
    a = mat . ma
    b = mat rawb
    x0  = if x0' == 0 then konst 0 (dim b) else x0'
    r0  = b - a x0
    r20 = r0 ◇ r0
    p0  = r0
    nb2 = b ◇ b
    ok CGState {..}
        =  cgr2 <nb2*ϵb**2
        || cgdx < ϵx


takeUntil :: (a -> Bool) -> [a] -> [a]
takeUntil q xs = a++ take 1 b
  where
    (a,b) = break q xs

class (Transposable m, Contraction m (Vector Double) (Vector Double)) => CGMat m

cgSolve
  :: CGMat m
  => Bool   -- ^ symmetric
  -> Double -- ^ relative tolerance for the residual (e.g. 1E-4)
  -> Double -- ^ relative tolerance for δx (e.g. 1E-3)
  -> m      -- ^ coefficient matrix
  -> Vector Double -- ^ right-hand side
  -> Vector Double -- ^ solution
cgSolve sym er es a b  = cgx $ last $ conjugrad sym a b 0 er es

