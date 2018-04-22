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

mIN_DIRK_NUM, mAX_DIRK_NUM :: Int
mIN_DIRK_NUM = #const MIN_DIRK_NUM
mAX_DIRK_NUM = #const MAX_DIRK_NUM

-- FIXME: We could just use inline-c instead

-- Butcher table accessors -- implicit
sDIRK_2_1_2 :: Int
sDIRK_2_1_2 = #const SDIRK_2_1_2
bILLINGTON_3_3_2 :: Int
bILLINGTON_3_3_2 = #const BILLINGTON_3_3_2
tRBDF2_3_3_2 :: Int
tRBDF2_3_3_2 = #const TRBDF2_3_3_2
kVAERNO_4_2_3 :: Int
kVAERNO_4_2_3 = #const KVAERNO_4_2_3
aRK324L2SA_DIRK_4_2_3 :: Int
aRK324L2SA_DIRK_4_2_3 = #const ARK324L2SA_DIRK_4_2_3
cASH_5_2_4 :: Int
cASH_5_2_4 = #const CASH_5_2_4
cASH_5_3_4 :: Int
cASH_5_3_4 = #const CASH_5_3_4
sDIRK_5_3_4 :: Int
sDIRK_5_3_4 = #const SDIRK_5_3_4
kVAERNO_5_3_4 :: Int
kVAERNO_5_3_4 = #const KVAERNO_5_3_4
aRK436L2SA_DIRK_6_3_4 :: Int
aRK436L2SA_DIRK_6_3_4 = #const ARK436L2SA_DIRK_6_3_4
kVAERNO_7_4_5 :: Int
kVAERNO_7_4_5 = #const KVAERNO_7_4_5
aRK548L2SA_DIRK_8_4_5 :: Int
aRK548L2SA_DIRK_8_4_5 = #const ARK548L2SA_DIRK_8_4_5

-- #define DEFAULT_DIRK_2          SDIRK_2_1_2
-- #define DEFAULT_DIRK_3          ARK324L2SA_DIRK_4_2_3
-- #define DEFAULT_DIRK_4          SDIRK_5_3_4
-- #define DEFAULT_DIRK_5          ARK548L2SA_DIRK_8_4_5

-- Butcher table accessors -- explicit
hEUN_EULER_2_1_2 :: Int
hEUN_EULER_2_1_2 = #const HEUN_EULER_2_1_2
bOGACKI_SHAMPINE_4_2_3 :: Int
bOGACKI_SHAMPINE_4_2_3 = #const BOGACKI_SHAMPINE_4_2_3
aRK324L2SA_ERK_4_2_3 :: Int
aRK324L2SA_ERK_4_2_3 = #const ARK324L2SA_ERK_4_2_3
zONNEVELD_5_3_4 :: Int
zONNEVELD_5_3_4 = #const ZONNEVELD_5_3_4
aRK436L2SA_ERK_6_3_4 :: Int
aRK436L2SA_ERK_6_3_4 = #const ARK436L2SA_ERK_6_3_4
sAYFY_ABURUB_6_3_4 :: Int
sAYFY_ABURUB_6_3_4 = #const SAYFY_ABURUB_6_3_4
cASH_KARP_6_4_5 :: Int
cASH_KARP_6_4_5 = #const CASH_KARP_6_4_5
fEHLBERG_6_4_5 :: Int
fEHLBERG_6_4_5 = #const FEHLBERG_6_4_5
dORMAND_PRINCE_7_4_5 :: Int
dORMAND_PRINCE_7_4_5 = #const DORMAND_PRINCE_7_4_5
aRK548L2SA_ERK_8_4_5 :: Int
aRK548L2SA_ERK_8_4_5 = #const ARK548L2SA_ERK_8_4_5
vERNER_8_5_6 :: Int
vERNER_8_5_6 = #const VERNER_8_5_6
fEHLBERG_13_7_8 :: Int
fEHLBERG_13_7_8 = #const FEHLBERG_13_7_8

-- #define DEFAULT_ERK_2           HEUN_EULER_2_1_2
-- #define DEFAULT_ERK_3           BOGACKI_SHAMPINE_4_2_3
-- #define DEFAULT_ERK_4           ZONNEVELD_5_3_4
-- #define DEFAULT_ERK_5           CASH_KARP_6_4_5
-- #define DEFAULT_ERK_6           VERNER_8_5_6
-- #define DEFAULT_ERK_8           FEHLBERG_13_7_8
