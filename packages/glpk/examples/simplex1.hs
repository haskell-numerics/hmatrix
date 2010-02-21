-- first example in glpk manual

import Numeric.LinearProgramming

objFun = Maximize [10, 6, 4]

constr = Dense [ [1,1,1]  :<: 100
              , [10,4,5] :<: 600 
              , [2,2,6]  :<: 300 ]

-- default bounds
bnds = [ 1 :>: 0
       , 2 :>: 0
       , 3 :>: 0 ]
         
main = do
    print $ simplex objFun constr []
    print $ simplex objFun constr bnds
    print $ simplex objFun constr [Free 3]
    print $ simplex objFun constr [ 2 :<: 50 ]

