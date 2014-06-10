{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE RecordWildCards #-}



module Numeric.LinearAlgebra.Sparse (
    dss
) where

import Foreign.C.Types(CInt(..))
import Numeric.LinearAlgebra.Devel
import System.IO.Unsafe(unsafePerformIO)
import Foreign(Ptr)
import Numeric.LinearAlgebra.HMatrix
import Text.Printf
import Numeric.LinearAlgebra.Util((~!~))


type IV t = CInt -> Ptr CInt   -> t
type  V t = CInt -> Ptr Double -> t
type SMxV = V (IV (IV (V (V (IO CInt)))))

dss :: CSR -> Vector Double -> Vector Double
dss CSR{..} b = unsafePerformIO $ do
    size b /= csrNRows ~!~ printf "dss: incorrect sizes: (%d,%d) x %d" csrNRows csrNCols (size b)
    r <- createVector csrNCols
    app5 c_dss vec csrVals vec csrCols vec csrRows vec b vec r "dss"
    return r

foreign import ccall unsafe "dss"
  c_dss :: SMxV

