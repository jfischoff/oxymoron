import Oxymoron.Description.Symbol

main = do
    writeFile "leqSymbol.txt" . unlines $ 
        ["leqAChar " ++ show x ++ " " ++ show y ++ " = " ++ show (x < y) | x <- [CA .. CZ], y <- [CA .. CZ]]