telFortune n = n ++ " Good!!"

main = do
  putStrLn "Hello, what's your name"
  name <- getLine
  putStrLn $ "Zis is your future: " ++ telFortune name

