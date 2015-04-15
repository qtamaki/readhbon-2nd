import Control.Monad

main = do
  input <- getLine
  when (input == "SWORDFISH") $ do
    putStrLn $ input ++ " is greate good!"

