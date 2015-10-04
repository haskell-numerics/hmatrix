import Numeric.GSL
import Numeric.GSL.Special
import Numeric.LinearAlgebra
import Prelude hiding (catch)
import Control.Exception

test x = catch
       (print x)
       (\e -> putStrLn $ "captured ["++ show (e :: SomeException) ++"]")


main = do
    setErrorHandlerOff

    test $ log_e (-1)
    test $ 5 + (fst.exp_e) 1000
    test $ bessel_zero_Jnu_e (-0.3) 2

    test $ (inv 0 :: Matrix Double)
    test $ (linearSolveLS 5 (sqrt (-1)) :: Matrix Double)

    putStrLn "Bye"

