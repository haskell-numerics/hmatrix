{-# LANGUAGE FlexibleContexts, FlexibleInstances #-}
{-# LANGUAGE RecordWildCards #-}

module Numeric.LinearAlgebra.Util.CG(
    cgSolve, cgSolve',
    CGState(..), R, V
) where

import Data.Packed.Numeric
import Numeric.Sparse
import Numeric.Vector()
import Numeric.LinearAlgebra.Algorithms(linearSolveLS, relativeError, NormType(..))
import Control.Arrow((***))

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
  :: (Transposable m mt, Contraction m V V, Contraction mt V V)
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

cgSolve
  :: Bool          -- ^ is symmetric
  -> GMatrix       -- ^ coefficient matrix
  -> Vector Double -- ^ right-hand side
  -> Vector Double -- ^ solution
cgSolve sym a b  = cgx $ last $ cgSolve' sym 1E-4 1E-3 n a b 0
  where
    n = max 10 (round $ sqrt (fromIntegral (dim b) :: Double))

cgSolve'
  :: Bool      -- ^ symmetric
  -> R         -- ^ relative tolerance for the residual (e.g. 1E-4)
  -> R         -- ^ relative tolerance for δx (e.g. 1E-3)
  -> Int       -- ^ maximum number of iterations
  -> GMatrix   -- ^ coefficient matrix
  -> V         -- ^ initial solution
  -> V         -- ^ right-hand side
  -> [CGState] -- ^ solution
cgSolve' sym er es n a b x = take n $ conjugrad sym a b x er es


--------------------------------------------------------------------------------

instance Testable GMatrix
  where
    checkT _ = (ok,info)
      where
        sma = convo2 20 3
        x1 = vect [1..20]
        x2 = vect [1..40]
        sm = (mkSparse . mkCSR) sma
        dm = toDense sma

        s1 = sm !#> x1
        d1 = dm #> x1

        s2 = tr sm !#> x2
        d2 = tr dm #> x2

        sdia = mkDiagR 40 20 (vect [1..10])
        s3 =    sdia !#> x1
        s4 = tr sdia !#> x2
        ddia = diagRect 0 (vect [1..10])  40 20
        d3 = ddia #> x1
        d4 = tr ddia #> x2

        v = testb 40
        s5 = cgSolve False sm v
        d5 = denseSolve dm v

        info = do
            print sm
            disp (toDense sma)
            print s1; print d1
            print s2; print d2
            print s3; print d3
            print s4; print d4
            print s5; print d5
            print $ relativeError Infinity s5 d5

        ok = s1==d1
          && s2==d2
          && s3==d3
          && s4==d4
          && relativeError Infinity s5 d5 < 1E-10

        disp = putStr . dispf 2

        vect = fromList :: [Double] -> Vector Double

        convomat :: Int -> Int -> AssocMatrix
        convomat n k = [ ((i,j `mod` n),1) | i<-[0..n-1], j <- [i..i+k-1]]

        convo2 :: Int -> Int -> AssocMatrix
        convo2 n k = m1 ++ m2
          where
            m1 = convomat n k
            m2 = map (((+n) *** id) *** id) m1
            
        testb n = vect $ take n $ cycle ([0..10]++[9,8..1])
        
        denseSolve a = flatten . linearSolveLS a . asColumn

        -- mkDiag v = mkDiagR (dim v) (dim v) v

