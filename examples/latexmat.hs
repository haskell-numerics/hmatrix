import Numeric.LinearAlgebra
import Text.Printf

disp w l fmt m = unlines $ map (++l) $ lines $ format w (printf fmt) m

latex fmt m = "\\begin{bmatrix}\n" ++ disp " & " " \\\\" fmt m ++ "\\end{bmatrix}"

main = do
    let m = (3><4) [1..12::Double]
    putStrLn $ disp " | " "" "%.2f" m
    putStrLn $ latex "%.3f" m
