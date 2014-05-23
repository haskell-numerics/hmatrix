{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

module Numeric.Sparse(
    SMatrix(..),
    mkCSR, mkDiag,
    AssocMatrix,
    toDense,
    smXv
)where

import Numeric.Container
import qualified Data.Vector.Storable as V
import Data.Function(on)
import Control.Arrow((***))
import Control.Monad(when)
import Data.List(groupBy, sort)
import Foreign.C.Types(CInt(..))
import Numeric.LinearAlgebra.Util.CG(CGMat,cgSolve)
import Numeric.LinearAlgebra.Algorithms(linearSolveLS, relativeError, NormType(..))
import Data.Packed.Development
import System.IO.Unsafe(unsafePerformIO)
import Foreign(Ptr)
import Text.Printf(printf)

infixl 0 ~!~
c ~!~ msg = when c (error msg)

type AssocMatrix = [((Int,Int),Double)]

data SMatrix
    = CSR
        { csrVals :: Vector Double
        , csrCols :: Vector CInt
        , csrRows :: Vector CInt
        , nRows   :: Int
        , nCols   :: Int
        }
    | CSC
        { cscVals :: Vector Double
        , cscRows :: Vector CInt
        , cscCols :: Vector CInt
        , nRows   :: Int
        , nCols   :: Int
        }
    | Diag
        { diagVals :: Vector Double
        , nRows   :: Int
        , nCols   :: Int
        }
--    | Banded
    deriving Show

mkCSR :: AssocMatrix -> SMatrix
mkCSR sm' = CSR{..}
  where
    sm = sort sm'
    rws = map ((fromList *** fromList)
              . unzip
              . map ((succ.fi.snd) *** id)
              )
        . groupBy ((==) `on` (fst.fst))
        $ sm
    rszs = map (fi . dim . fst) rws
    csrRows = fromList (scanl (+) 1 rszs)
    csrVals = vjoin (map snd rws)
    csrCols = vjoin (map fst rws)
    nRows = dim csrRows - 1
    nCols = fromIntegral (V.maximum csrCols)


mkDiagR r c v
    | dim v <= min r c = Diag{..}
    | otherwise = error $ printf "mkDiagR: incorrect sizes (%d,%d) [%d]" r c (dim v)
  where
    nRows = r
    nCols = c
    diagVals = v

mkDiag v = mkDiagR (dim v) (dim v) v


type IV t = CInt -> Ptr CInt   -> t
type  V t = CInt -> Ptr Double -> t
type SMxV = V (IV (IV (V (V (IO CInt)))))

smXv :: SMatrix -> Vector Double -> Vector Double
smXv CSR{..} v = unsafePerformIO $ do
    dim v /= nCols ~!~ printf "smXv (CSR): incorrect sizes: (%d,%d) x %d" nRows nCols (dim v)
    r <- createVector nRows
    app5 c_smXv vec csrVals vec csrCols vec csrRows vec v vec r "CSRXv"
    return r

smXv CSC{..} v = unsafePerformIO $ do
    dim v /= nCols ~!~ printf "smXv (CSC): incorrect sizes: (%d,%d) x %d" nRows nCols (dim v)
    r <- createVector nRows
    app5 c_smTXv vec cscVals vec cscRows vec cscCols vec v vec r "CSCXv"
    return r

smXv Diag{..} v
    | dim v == nCols
        = vjoin [ subVector 0 (dim diagVals) v `mul` diagVals
                , konst 0 (nRows - dim diagVals) ]
    | otherwise = error $ printf "smXv (Diag): incorrect sizes: (%d,%d) [%d] x %d"
                                 nRows nCols (dim diagVals) (dim v)


instance Contraction SMatrix (Vector Double) (Vector Double)
  where
    contraction = smXv

--------------------------------------------------------------------------------

foreign import ccall unsafe "smXv"
  c_smXv :: SMxV

foreign import ccall unsafe "smTXv"
  c_smTXv :: SMxV

--------------------------------------------------------------------------------

toDense :: AssocMatrix -> Matrix Double
toDense asm = assoc (r+1,c+1) 0 asm
  where
    (r,c) = (maximum *** maximum) . unzip . map fst $ asm



instance Transposable SMatrix
  where
    tr (CSR vs cs rs n m) = CSC vs cs rs m n
    tr (CSC vs rs cs n m) = CSR vs rs cs m n
    tr (Diag v n m) = Diag v m n

instance Transposable (Matrix Double)
  where
    tr = trans


instance CGMat SMatrix
instance CGMat (Matrix Double)

--------------------------------------------------------------------------------

instance Testable SMatrix
  where
    checkT _ = (ok,info)
      where
        sma = convo2 20 3
        x1 = vect [1..20]
        x2 = vect [1..40]
        sm = mkCSR sma
        dm = toDense sma

        s1 = sm ◇ x1
        d1 = dm  ◇  x1

        s2 = tr sm  ◇  x2
        d2 = tr dm  ◇  x2

        sdia = mkDiagR 40 20 (vect [1..10])
        s3 =    sdia  ◇  x1
        s4 = tr sdia  ◇  x2
        ddia = diagRect 0 (vect [1..10])  40 20
        d3 = ddia  ◇  x1
        d4 = tr ddia  ◇  x2

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

