{-# LANGUAGE RecordWildCards #-}

module Bar where

import Foreign
import Foreign.C.Types
import Foreign.C.String

#include "/Users/dom/sundials/include/sundials/sundials_nvector.h"
#include "/Users/dom/sundials/include/nvector/nvector_serial.h"
#include "/Users/dom/sundials/include/arkode/arkode.h"

#def typedef struct _generic_N_Vector BarType;
#def typedef struct _N_VectorContent_Serial BazType;


getContentPtr :: Storable a => Ptr b -> IO a
getContentPtr ptr = (#peek BarType, content) ptr

getData :: Storable a => Ptr b -> IO a
getData ptr = (#peek BazType, data) ptr

arkSMax :: Int
arkSMax = #const ARK_S_MAX


  

