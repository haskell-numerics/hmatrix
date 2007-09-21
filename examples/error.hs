import GSL
import Prelude hiding (catch)
import Control.Exception

test x = catch
       (print x)
       (\e -> putStrLn $ "captured ["++ show e++"]")

main = do
    setErrorHandlerOff

    test $ log_e (-1)
    test $ 5 + (fst.exp_e) 1000
    test $ bessel_zero_Jnu_e (-0.3) 2

    putStrLn "Bye"