telFortune :: String -> String
telFortune n = n ++ " Good!!"

main :: IO ()
main = do
  putStrLn "Hello, what's your name"
  name <- getLine
  putStrLn $ "Zis is your future: " ++ telFortune name

-- String ++ IO String
-- nameTag = "Hello" ++ (name <- getLine)
