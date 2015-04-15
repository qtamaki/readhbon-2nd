main = do
  input <- getLine
  if (input == "SWORDFISH")
      then putStrLn $ input ++ " is greate good!"
      else return ()

