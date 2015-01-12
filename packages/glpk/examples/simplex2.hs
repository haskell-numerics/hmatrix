import Numeric.LinearProgramming

prob = Maximize [4, -3, 2]

constr1 = Sparse [ [2#1, 1#2] :<=: 10
                 , [1#2, 5#3] :<=: 20
                 ]

constr2 = Dense [ [2,1,0] :<=: 10
                , [0,1,5] :<=: 20
                ]

main = do
    print $ simplex prob constr1 []
    print $ simplex prob constr2 []
    print $ simplex prob constr2 [ 2 :>=: 1, 3 :&: (2,7)]
    print $ simplex prob constr2 [ Free 2 ]

