import Data.List
import Data.Char

numUniques :: (Eq a) => [a] -> Int
numUniques = length . nub

wordNums :: String -> [(String,Int)]
wordNums = map (\ws -> (head ws, length ws)) . group . sort . words

isIn :: (Eq a) => [a] -> [a] -> Bool
needle `isIn` haystack = any (needle `isPrefixOf`) (tails haystack)

------------

encode :: Int -> String -> String
encode offset msg = map (\c -> chr $ ord c + offset) msg
-- (chr . (+ offset) . ord)

decode :: Int -> String -> String
decode shift msg = encode (negate shift) msg

------------

digitSum :: Int -> Int
digitSum = sum . map digitToInt . show

firstTo40 :: Maybe Int
firstTo40 = find (\x -> digitSum x == 40) [1..]

firstTo :: Int -> [Int] -> Maybe Int
firstTo n xs = find (\x -> digitSum x == n) xs -- [1..]

phoneBook = id $
  [("betty", "555-2938") ,("bonnie", "452-2928") ]

findKey :: (Eq k) => k -> [(k,v)] -> v
findKey key xs = snd . head . filter (\(k,v) -> key == k) $ xs

findKey' :: (Eq k) => k -> [(k,v)] -> Maybe v
findKey' key [] = Nothing
findKey' key ((k,v):xs)
  | key == k = Just v
  | otherwise = findKey' key xs





