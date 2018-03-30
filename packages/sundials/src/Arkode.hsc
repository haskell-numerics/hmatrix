{-# LANGUAGE RecordWildCards #-}

module Arkode where

import Foreign
import Foreign.C.Types
import Foreign.C.String


#include <stdio.h>
#include <sundials/sundials_nvector.h>
#include <nvector/nvector_serial.h>
#include <arkode/arkode.h>

#def typedef struct _generic_N_Vector SunVector;
#def typedef struct _N_VectorContent_Serial SunContent;

getContentPtr :: Storable a => Ptr b -> IO a
getContentPtr ptr = (#peek SunVector, content) ptr

getData :: Storable a => Ptr b -> IO a
getData ptr = (#peek SunContent, data) ptr

arkSMax :: Int
arkSMax = #const ARK_S_MAX

-- FIXME: We could just use inline-c instead
sDIRK_2_1_2 :: Int
sDIRK_2_1_2 = #const SDIRK_2_1_2
-- #define BILLINGTON_3_3_2        13
-- #define TRBDF2_3_3_2            14
kVAERNO_4_2_3 :: Int
kVAERNO_4_2_3 = #const KVAERNO_4_2_3
-- #define ARK324L2SA_DIRK_4_2_3   16
-- #define CASH_5_2_4              17
-- #define CASH_5_3_4              18
-- #define SDIRK_5_3_4             19
-- #define KVAERNO_5_3_4           20
-- #define ARK436L2SA_DIRK_6_3_4   21
-- #define KVAERNO_7_4_5           22
-- #define ARK548L2SA_DIRK_8_4_5   23

-- #define DEFAULT_DIRK_2          SDIRK_2_1_2
-- #define DEFAULT_DIRK_3          ARK324L2SA_DIRK_4_2_3
-- #define DEFAULT_DIRK_4          SDIRK_5_3_4
-- #define DEFAULT_DIRK_5          ARK548L2SA_DIRK_8_4_5
