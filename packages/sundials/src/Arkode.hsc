{-# LANGUAGE RecordWildCards #-}

module Arkode where

import Foreign
import Foreign.C.Types


#include <stdio.h>
#include <sundials/sundials_nvector.h>
#include <sundials/sundials_matrix.h>
#include <nvector/nvector_serial.h>
#include <sunmatrix/sunmatrix_dense.h>
#include <arkode/arkode.h>


#def typedef struct _generic_N_Vector SunVector;
#def typedef struct _N_VectorContent_Serial SunContent;

#def typedef struct _generic_SUNMatrix SunMatrix;
#def typedef struct _SUNMatrixContent_Dense SunMatrixContent;

getContentMatrixPtr :: Storable a => Ptr b -> IO a
getContentMatrixPtr ptr = (#peek SunMatrix, content) ptr

getNRows :: Ptr b -> IO CInt
getNRows ptr = (#peek SunMatrixContent, M) ptr
putNRows :: CInt -> Ptr b -> IO ()
putNRows nr ptr = (#poke SunMatrixContent, M) ptr nr

getNCols :: Ptr b -> IO CInt
getNCols ptr = (#peek SunMatrixContent, N) ptr
putNCols :: CInt -> Ptr b -> IO ()
putNCols nc ptr = (#poke SunMatrixContent, N) ptr nc

getMatrixData :: Storable a => Ptr b -> IO a
getMatrixData ptr = (#peek SunMatrixContent, data) ptr

getContentPtr :: Storable a => Ptr b -> IO a
getContentPtr ptr = (#peek SunVector, content) ptr

getData :: Storable a => Ptr b -> IO a
getData ptr = (#peek SunContent, data) ptr

arkSMax :: Int
arkSMax = #const ARK_S_MAX

-- FIXME: We could just use inline-c instead

-- /* Butcher table accessors -- implicit */
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
sDIRK_5_3_4 :: Int
sDIRK_5_3_4 = #const SDIRK_5_3_4
-- #define KVAERNO_5_3_4           20
-- #define ARK436L2SA_DIRK_6_3_4   21
-- #define KVAERNO_7_4_5           22
-- #define ARK548L2SA_DIRK_8_4_5   23

-- #define DEFAULT_DIRK_2          SDIRK_2_1_2
-- #define DEFAULT_DIRK_3          ARK324L2SA_DIRK_4_2_3
-- #define DEFAULT_DIRK_4          SDIRK_5_3_4
-- #define DEFAULT_DIRK_5          ARK548L2SA_DIRK_8_4_5

-- /* Butcher table accessors -- explicit */
-- #define HEUN_EULER_2_1_2         0
-- #define BOGACKI_SHAMPINE_4_2_3   1
-- #define ARK324L2SA_ERK_4_2_3     2
-- #define ZONNEVELD_5_3_4          3
-- #define ARK436L2SA_ERK_6_3_4     4
-- #define SAYFY_ABURUB_6_3_4       5
-- #define CASH_KARP_6_4_5          6
fEHLBERG_6_4_5 :: Int
fEHLBERG_6_4_5 = #const FEHLBERG_6_4_5
-- #define FEHLBERG_6_4_5           7
-- #define DORMAND_PRINCE_7_4_5     8
-- #define ARK548L2SA_ERK_8_4_5     9
-- #define VERNER_8_5_6            10
-- #define FEHLBERG_13_7_8         11

-- #define DEFAULT_ERK_2           HEUN_EULER_2_1_2
-- #define DEFAULT_ERK_3           BOGACKI_SHAMPINE_4_2_3
-- #define DEFAULT_ERK_4           ZONNEVELD_5_3_4
-- #define DEFAULT_ERK_5           CASH_KARP_6_4_5
-- #define DEFAULT_ERK_6           VERNER_8_5_6
-- #define DEFAULT_ERK_8           FEHLBERG_13_7_8
