solveRPN :: String -> Double
--solveRPN expression = head (foldl foldingFunction [] (words expression))
solveRPN = head . foldl foldingFunction [] . words
--  where foldingFunction stack item = undefined
  where foldingFunction (x:y:ys) "*" = (y * x):ys
        foldingFunction (x:y:ys) "+" = (y + x):ys
        foldingFunction (x:y:z:ys) "++" = (y + x + z):ys
        foldingFunction (x:y:ys) "-" = (y - x):ys
        foldingFunction (x:y:ys) "/" = (y / x):ys
        foldingFunction (x:y:ys) "^" = (y ** x):ys
        foldingFunction (x:xs) "ln" = log x:xs
        foldingFunction (xs) "sum" = [sum xs]
        foldingFunction xs numberString = read numberString:xs



