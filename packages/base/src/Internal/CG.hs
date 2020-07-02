{-# LANGUAGE FlexibleContexts, FlexibleInstances #-}
{-# LANGUAGE RecordWildCards #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Internal.CG(
    cgSolve, cgSolve',
    CGState(..), R, V
) where

import Internal.Vector
import Internal.Matrix
import Internal.Numeric
import Internal.Element
import Internal.IO
import Internal.Container
import Internal.Sparse
import Numeric.Vector()
import Internal.Algorithms(linearSolveLS, linearSolve, relativeError, pnorm, NormType(..))
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

type V = Vector R

data CGState = CGState
    { cgp  :: Vector R  -- ^ conjugate gradient
    , cgr  :: Vector R  -- ^ residual
    , cgr2 :: R         -- ^ squared norm of residual
    , cgx  :: Vector R  -- ^ current solution
    , cgdx :: R         -- ^ normalized size of correction
    }

cg :: Bool -> (V -> V) -> (V -> V) -> CGState -> CGState
cg sym at a (CGState p r r2 x _) = CGState p' r' r'2 x' rdx
  where
    ap1 = a p
    ap  | sym       = ap1
        | otherwise = at ap1
    pap | sym       = p <.> ap1
        | otherwise = norm2 ap1 ** 2
    alpha = r2 / pap
    dx = scale alpha p
    x' = x + dx
    r' = r - scale alpha ap
    r'2 = r' <.> r'
    beta = r'2 / r2
    p' = r' + scale beta p

    rdx = norm2 dx / max 1 (norm2 x)

conjugrad
  :: Bool -> GMatrix -> V -> V -> R -> R -> [CGState]
conjugrad sym a b = solveG sym (tr a !#>) (a !#>) (cg sym) b

solveG
    :: Bool
    -> (V -> V) -> (V -> V)
    -> ((V -> V) -> (V -> V) -> CGState -> CGState)
    -> V
    -> V
    -> R -> R
    -> [CGState]
solveG sym mat ma meth rawb x0' ϵb ϵx
    = takeUntil ok . iterate (meth mat ma) $ CGState p0 r0 r20 x0 1
  where
    a = if sym then ma else mat . ma
    b = if sym then rawb else mat rawb
    x0  = if x0' == 0 then konst 0 (dim b) else x0'
    r0  = b - a x0
    r20 = r0 <.> r0
    p0  = r0
    nb2 = b <.> b
    ok CGState {..}
        =  cgr2 <nb2*ϵb**2
        || cgdx < ϵx


takeUntil :: (a -> Bool) -> [a] -> [a]
takeUntil q xs = a++ take 1 b
  where
    (a,b) = break q xs

-- | Solve a sparse linear system using the conjugate gradient method with default parameters.
cgSolve
  :: Bool          -- ^ is symmetric
  -> GMatrix       -- ^ coefficient matrix
  -> Vector R      -- ^ right-hand side
  -> Vector R      -- ^ solution
cgSolve sym a b  = cgx $ last $ cgSolve' sym 1E-4 1E-3 n a b 0
  where
    n = max 10 (round $ sqrt (fromIntegral (dim b) :: Double))

-- | Solve a sparse linear system using the conjugate gradient method with default parameters.
cgSolve'
  :: Bool      -- ^ symmetric
  -> R         -- ^ relative tolerance for the residual (e.g. 1E-4)
  -> R         -- ^ relative tolerance for δx (e.g. 1E-3)
  -> Int       -- ^ maximum number of iterations
  -> GMatrix   -- ^ coefficient matrix
  -> Vector R  -- ^ initial solution
  -> Vector R  -- ^ right-hand side
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
        sm = mkSparse sma
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

        symassoc = [((0,0),1.0),((1,1),2.0),((0,1),0.5),((1,0),0.5)]
        b = vect [3,4]
        d6 = flatten $ linearSolve (toDense symassoc) (asColumn b)
        s6 = cgSolve True (mkSparse symassoc) b

        info = do
            print sm
            disp (toDense sma)
            print s1; print d1
            print s2; print d2
            print s3; print d3
            print s4; print d4
            print s5; print d5
            print $ relativeError (pnorm Infinity) s5 d5
            print s6; print d6
            print $ relativeError (pnorm Infinity) s6 d6

        ok = s1==d1
          && s2==d2
          && s3==d3
          && s4==d4
          && relativeError (pnorm Infinity) s5 d5 < 1E-10
          && relativeError (pnorm Infinity) s6 d6 < 1E-10

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

