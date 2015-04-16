main = do
  let a = "hell"
      b = "yeah!"
  name <- sub "Tamaki"
  putStrLn $ a ++ " " ++ b ++ name

sub :: String -> IO String
sub name = do
  x <- getLine
  return "YAMADA!"

  
