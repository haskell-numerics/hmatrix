import Numeric.LinearProgramming

-- Represents a button-pressing system, each button increments a set of counters until a desired state is reached.
-- Since a button can only increment in terms of integers, simplex by itself will lead to incorrect results.
prob = Minimize [1,1,1,1,1,1,1,1,1]

constr1 = Dense $ [ [1,0,0,1,0,1,1,0,0] :==: 206
                  , [0,1,1,1,0,0,0,1,1] :==: 201
                  , [0,1,1,0,1,1,0,1,0] :==: 19
                  , [0,0,0,1,1,1,0,0,0] :==: 200
                  , [1,0,1,1,1,0,0,0,0] :==: 207
                  , [0,1,1,1,0,1,1,0,1] :==: 213
                  , [0,0,0,0,1,1,0,1,1] :==: 11
                  ]
main = do
    print $ simplex prob constr1 [] -- 220.33
    print $ intoptExact  prob constr1 [] -- 223