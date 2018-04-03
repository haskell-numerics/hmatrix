{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module Internal.Sparse(
    GMatrix(..), CSR(..), mkCSR, fromCSR,
    mkSparse, mkDiagR, mkDense,
    AssocMatrix,
    toDense,
    gmXv, (!#>)
)where

import Internal.Vector
import Internal.Matrix
import Internal.Numeric
import qualified Data.Vector.Storable as V
import Data.Function(on)
import Control.Arrow((***))
import Control.Monad(when)
import Data.List(groupBy, sort)
import Foreign.C.Types(CInt(..))

import Internal.Devel
import System.IO.Unsafe(unsafePerformIO)
import Foreign(Ptr)
import Text.Printf(printf)

infixl 0 ~!~
c ~!~ msg = when c (error msg)

type AssocMatrix = [((Int,Int),Double)]

data CSR = CSR
        { csrVals  :: Vector Double
        , csrCols  :: Vector CInt
        , csrRows  :: Vector CInt
        , csrNRows :: Int
        , csrNCols :: Int
        } deriving Show

data CSC = CSC
        { cscVals  :: Vector Double
        , cscRows  :: Vector CInt
        , cscCols  :: Vector CInt
        , cscNRows :: Int
        , cscNCols :: Int
        } deriving Show


mkCSR :: AssocMatrix -> CSR
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
    csrNRows = dim csrRows - 1
    csrNCols = fromIntegral (V.maximum csrCols)

{- | General matrix with specialized internal representations for
     dense, sparse, diagonal, banded, and constant elements.

>>> let m = mkSparse [((0,999),1.0),((1,1999),2.0)]
>>> m
SparseR {gmCSR = CSR {csrVals = fromList [1.0,2.0],
                      csrCols = fromList [1000,2000],
                      csrRows = fromList [1,2,3],
                      csrNRows = 2,
                      csrNCols = 2000},
                      nRows = 2,
                      nCols = 2000}

>>> let m = mkDense (mat 2 [1..4])
>>> m
Dense {gmDense = (2><2)
 [ 1.0, 2.0
 , 3.0, 4.0 ], nRows = 2, nCols = 2}

-}
data GMatrix
    = SparseR
        { gmCSR   :: CSR
        , nRows   :: Int
        , nCols   :: Int
        }
    | SparseC
        { gmCSC   :: CSC
        , nRows   :: Int
        , nCols   :: Int
        }
    | Diag
        { diagVals :: Vector Double
        , nRows    :: Int
        , nCols    :: Int
        }
    | Dense
        { gmDense :: Matrix Double
        , nRows   :: Int
        , nCols   :: Int
        }
--    | Banded
    deriving Show


mkDense :: Matrix Double -> GMatrix
mkDense m = Dense{..}
  where
    gmDense = m
    nRows = rows m
    nCols = cols m

mkSparse :: AssocMatrix -> GMatrix
mkSparse = fromCSR . mkCSR

fromCSR :: CSR -> GMatrix
fromCSR csr = SparseR {..}
  where
    gmCSR @ CSR {..} = csr
    nRows = csrNRows
    nCols = csrNCols


mkDiagR r c v
    | dim v <= min r c = Diag{..}
    | otherwise = error $ printf "mkDiagR: incorrect sizes (%d,%d) [%d]" r c (dim v)
  where
    nRows = r
    nCols = c
    diagVals = v


type IV t = CInt -> Ptr CInt   -> t
type  V t = CInt -> Ptr Double -> t
type SMxV = V (IV (IV (V (V (IO CInt)))))

gmXv :: GMatrix -> Vector Double -> Vector Double
gmXv SparseR { gmCSR = CSR{..}, .. } v = unsafePerformIO $ do
    dim v /= nCols ~!~ printf "gmXv (CSR): incorrect sizes: (%d,%d) x %d" nRows nCols (dim v)
    r <- createVector nRows
    (csrVals # csrCols # csrRows # v #! r) c_smXv #|"CSRXv"
    return r

gmXv SparseC { gmCSC = CSC{..}, .. } v = unsafePerformIO $ do
    dim v /= nCols ~!~ printf "gmXv (CSC): incorrect sizes: (%d,%d) x %d" nRows nCols (dim v)
    r <- createVector nRows
    (cscVals # cscRows # cscCols # v #! r) c_smTXv #|"CSCXv"
    return r

gmXv Diag{..} v
    | dim v == nCols
        = vjoin [ subVector 0 (dim diagVals) v `mul` diagVals
                , konst 0 (nRows - dim diagVals) ]
    | otherwise = error $ printf "gmXv (Diag): incorrect sizes: (%d,%d) [%d] x %d"
                                 nRows nCols (dim diagVals) (dim v)

gmXv Dense{..} v
    | dim v == nCols
        = mXv gmDense v
    | otherwise = error $ printf "gmXv (Dense): incorrect sizes: (%d,%d) x %d"
                                 nRows nCols (dim v)


{- | general matrix - vector product

>>> let m = mkSparse [((0,999),1.0),((1,1999),2.0)]
>>> m !#> vector [1..2000]
fromList [1000.0,4000.0]

-}
infixr 8 !#>
(!#>) :: GMatrix -> Vector Double -> Vector Double
(!#>) = gmXv

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


instance Transposable CSR CSC
  where
    tr (CSR vs cs rs n m) = CSC vs cs rs m n
    tr' = tr

instance Transposable CSC CSR
  where
    tr (CSC vs rs cs n m) = CSR vs rs cs m n
    tr' = tr

instance Transposable GMatrix GMatrix
  where
    tr (SparseR s n m) = SparseC (tr s) m n
    tr (SparseC s n m) = SparseR (tr s) m n
    tr (Diag v n m) = Diag v m n
    tr (Dense a n m) = Dense (tr a) m n
    tr' = tr
