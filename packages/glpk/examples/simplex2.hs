import Numeric.LinearProgramming

prob = Maximize [4, 3, -2, 7]

constr1 = Sparse [ [1#1, 1#2] :<: 10
                 , [1#3, 1#4] :<: 10 
                 ]

constr2 = Dense [ [1,1,0,0] :<: 10
                , [0,0,1,1] :<: 10 
                ]

main = do
    print $ simplex prob constr1 []
    print $ simplex prob constr2 [ 2 :>: 1, 4 :&: (2,7)]
    print $ simplex prob constr2 [ Free 3 ]

