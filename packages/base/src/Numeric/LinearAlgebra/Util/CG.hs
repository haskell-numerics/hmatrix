{-# LANGUAGE FlexibleContexts, FlexibleInstances #-}
{-# LANGUAGE RecordWildCards #-}

module Numeric.LinearAlgebra.Util.CG(
    cgSolve, cgSolve',
    CGMat, CGState(..), R, V
) where

import Numeric.Container
import Numeric.Vector()

{-
import Util.Misc(debug, debugMat)

(//) :: Show a => a -> String -> a
infix 0 // -- , ///
a // b = debug b id a

(///) :: V -> String -> V
infix 0 ///
v /// b = debugMat b 2 asRow v
-}

type R = Double
type V = Vector R

data CGState = CGState
    { cgp  :: V  -- ^ conjugate gradient
    , cgr  :: V  -- ^ residual
    , cgr2 :: R  -- ^ squared norm of residual
    , cgx  :: V  -- ^ current solution
    , cgdx :: R  -- ^ normalized size of correction
    }

cg :: Bool -> (V -> V) -> (V -> V) -> CGState -> CGState
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
  :: (Transposable m, Contraction m V V)
  => Bool -> m -> V -> V -> R -> R -> [CGState]
conjugrad sym a b = solveG (tr a ◇) (a ◇) (cg sym) b

solveG
    :: (V -> V) -> (V -> V)
    -> ((V -> V) -> (V -> V) -> CGState -> CGState)
    -> V
    -> V
    -> R -> R
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

class (Transposable m, Contraction m V V) => CGMat m

cgSolve
  :: CGMat m
  => Bool     -- ^ is symmetric
  -> m        -- ^ coefficient matrix
  -> Vector Double -- ^ right-hand side
  -> Vector Double        -- ^ solution
cgSolve sym a b  = cgx $ last $ cgSolve' sym 1E-4 1E-3 n a b 0
  where
    n = max 10 (round $ sqrt (fromIntegral (dim b) :: Double))

cgSolve'
  :: CGMat m
  => Bool      -- ^ symmetric
  -> R         -- ^ relative tolerance for the residual (e.g. 1E-4)
  -> R         -- ^ relative tolerance for δx (e.g. 1E-3)
  -> Int       -- ^ maximum number of iterations
  -> m         -- ^ coefficient matrix
  -> V         -- ^ initial solution
  -> V         -- ^ right-hand side
  -> [CGState] -- ^ solution
cgSolve' sym er es n a b x = take n $ conjugrad sym a b x er es

