
removeNonUppercase :: [Char] -> [Char]
removeNonUppercase st = [c | c <- st, c `elem` ['A'..'Z']]

factorical :: Integer -> Integer
factorical n = product [1..n]

circumference :: Float -> Float
circumference r = 2 * pi * r


-- x = 100 :: Int

func :: [Int] -> Bool
func [] = True
func (x:[]) | x > 10 = True
            | otherwise = False
func (_:_) = False












func' = 1 :: Int






