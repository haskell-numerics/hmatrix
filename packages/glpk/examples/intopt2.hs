import Numeric.LinearProgramming

prob = Minimize [1,1,1,1,1]

constr1 = Dense $ [ [1,1,0,1,0] :==: 41
                  , [0,1,1,0,1] :==: 20
                  , [1,0,0,1,1] :==: 51.5
                  ]
-- Example problem which intopt doesn't work for
main = do
    print $ exact prob constr1 [] 
    print $ intoptExact  prob constr1 [] 