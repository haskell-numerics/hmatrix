{-# LANGUAGE RecordWildCards #-}

module Example where

import Foreign
import Foreign.C.Types
import Foreign.C.String

#include "/Users/dom/sundials/include/sundials/sundials_nvector.h"
#include "/Users/dom/sundials/include/nvector/nvector_serial.h"

#def typedef struct _generic_N_Vector BarType;
#def typedef struct _N_VectorContent_Serial BazType;


getContentPtr :: Storable a => Ptr b -> IO a
getContPtr ptr = (#peek BarType, content) ptr

getData ptr = (#peek BazType, data) ptr

foo ptr = do
  qtr <- getContPtr ptr
  getData qtr

